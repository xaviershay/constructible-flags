{-# LANGUAGE OverloadedStrings #-}

-- | Build animated movies of a flag's construction sequence.
--
-- The pipeline is:
--
--   1. Take the canonical pruned construction layer list (same one used by
--      the cost calculation and the debug viewer).
--   2. Plan a sequence of 'Frame's: a build-up section showing each step
--      with dotted construction geometry and accumulating fills, plus a
--      hold section with the canonical final SVG.
--   3. Smooth the per-frame bounding box trajectory with an EMA so the
--      camera doesn't jerk between steps, while still focusing on the
--      area being constructed.
--   4. Render each frame to SVG, rasterise via @rsvg-convert@, and assemble
--      the final movie via @ffmpeg@.
--
-- 'buildFrames' is pure and therefore unit-testable; the IO functions are
-- thin wrappers around external tools.
module Flag.Render.Animation
  ( -- * Configuration
    AnimationConfig (..),
    defaultAnimationConfig,

    -- * Output formats
    OutputFormat (..),
    outputExtension,

    -- * Frame planning (pure)
    Frame (..),
    frameBBox,
    layersForAnimation,
    pathsForAnimation,
    buildFrames,

    -- * IO
    writeFrameSVG,
    writeFramePNG,
    writeAllFrames,
    encodeAnimation,
  )
where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Flag.Construction.Geometry (dist)
import Flag.Construction.Layers (ConstructionLayer (..))
import Flag.Construction.Optimize (optimize)
import Flag.Construction.Tree (ConstructionTree, flattenTree, layerGroupPaths, pruneTree)
import Flag.Construction.Types (Drawing (..))
import Flag.Render.Bounds (BBox, applyPadding, drawingBounds)
import Flag.Render.Diagram
  ( drawingToElement,
    renderConstructionDots,
    renderConstructionGeom,
    renderConstructionStrokes,
  )
import Flag.Render.SVGOverlay
  ( OverlayPlacement,
    OverlaySource,
    extractOverlayPlacements,
    injectOverlays,
    loadOverlaySources,
  )
import Graphics.Svg
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (callProcess)
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Knobs controlling animation production.  Defaults are in
-- 'defaultAnimationConfig'.
data AnimationConfig = AnimationConfig
  { -- | Frames per second of the output movie
    acFps :: Int,
    -- | How many frames each construction step occupies
    acFramesPerStep :: Int,
    -- | Trailing frames showing only the final flag (no overlays)
    acHoldFrames :: Int,
    -- | Pixel width of each frame; height derived from the final flag aspect
    acWidth :: Double,
    -- | How many previous steps' construction geometry to show faintly as a trail
    acTrailSteps :: Int,
    -- | Multiplicative bbox padding applied to the final flag bbox
    -- (e.g. 1.15 = 15% extra around the flag).  The camera is fixed at
    -- this bbox for the entire animation; construction steps whose
    -- scaffolding extends beyond this bbox will be partially or fully
    -- off-screen, which is the accepted trade for a perfectly stable
    -- camera.
    acPadding :: Double
  }
  deriving (Show)

-- | Defaults: 12 fps, 6 frames/step, 36 hold frames, 600 px wide,
-- 20-step trail, 15 % padding around the final flag bbox.
defaultAnimationConfig :: AnimationConfig
defaultAnimationConfig =
  AnimationConfig
    { acFps = 12,
      acFramesPerStep = 6,
      acHoldFrames = 36,
      acWidth = 600,
      acTrailSteps = 20,
      acPadding = 1.15
    }

-- ---------------------------------------------------------------------------
-- Output formats
-- ---------------------------------------------------------------------------

-- | Supported output formats.  All go through the same intermediate PNG
-- frame sequence; only the final ffmpeg encode step differs.
data OutputFormat = FmtGif | FmtMp4 | FmtWebm | FmtApng | FmtWebp
  deriving (Show, Eq)

outputExtension :: OutputFormat -> String
outputExtension FmtGif = "gif"
outputExtension FmtMp4 = "mp4"
outputExtension FmtWebm = "webm"
outputExtension FmtApng = "apng"
outputExtension FmtWebp = "webp"

-- ---------------------------------------------------------------------------
-- Frame planning
-- ---------------------------------------------------------------------------

-- | A single frame in the planned animation.
--
-- 'BuildupFrame' carries everything needed to render a step-in-progress
-- view: the accumulated fills so far, an SVG element for the dotted
-- construction geometry of the active (and trailing) steps, and the
-- enclosing group-label path of the active step (outermost first).  The
-- path is rendered as a small caption in the lower right; it gives the
-- viewer a human-readable hint of where in the construction we are
-- (e.g. @["Horizontal stripes", "Fill rectangle"]@) without exposing
-- low-level primitive names like "Intersect line/circle".
--
-- 'HoldFrame' renders the canonical final flag (with all overlays
-- injected), exactly matching the SVG produced by the regular build.  No
-- caption is drawn on hold frames.
--
-- Fields are positional (no record syntax) to avoid partial-field selectors;
-- callers should pattern-match or use the helpers below.
data Frame
  = -- | @BuildupFrame frameIndex frameBBox settled overlay groupPath@
    BuildupFrame !Int !BBox !Drawing Element ![String]
  | -- | @HoldFrame frameIndex frameBBox finalDrawing@
    HoldFrame !Int !BBox !Drawing

-- | Bounding box of any frame.
frameBBox :: Frame -> BBox
frameBBox (BuildupFrame _ b _ _ _) = b
frameBBox (HoldFrame _ b _) = b

-- | Extract the canonical pruned layer list from a flag's construction tree.
-- This mirrors what 'app/Main.hs' uses for the cost calculation, so the
-- animation step count agrees with the per-flag debug viewer.
layersForAnimation :: [ConstructionTree] -> [ConstructionLayer]
layersForAnimation = concatMap flattenTree . pruneTree

-- | Group-label paths (outermost first) for each layer in the pruned tree,
-- in the same order as 'layersForAnimation'.  Empty path means the layer
-- is not enclosed in any 'TreeGroup'.
pathsForAnimation :: [ConstructionTree] -> [[String]]
pathsForAnimation = layerGroupPaths . pruneTree

-- | Plan the full frame sequence (build-up + hold) for a flag.
--
-- 'buildFrames' is pure.  Every frame uses the same fixed bbox: the
-- (padded) final-flag bbox.  Construction steps whose scaffolding extends
-- beyond that bbox will be partially or fully off-screen — we deliberately
-- accept this in exchange for a perfectly stable camera.
--
-- @paths@ should be the same length as @layers@ (typically obtained via
-- 'pathsForAnimation' on the same tree).  If it is shorter, missing
-- entries default to the empty path (no caption); extra entries are
-- ignored.
buildFrames :: AnimationConfig -> Drawing -> [ConstructionLayer] -> [[String]] -> [Frame]
buildFrames cfg finalDrawing layers paths =
  let finalBBoxRaw = case drawingBounds (optimize finalDrawing) of
        Just b -> b
        Nothing -> (0, 0, 1, 1)
      bbox = applyPadding (acPadding cfg) finalBBoxRaw

      n = length layers
      fps_ = max 1 (acFramesPerStep cfg)
      nBuildup = n * fps_
      nHold = max 0 (acHoldFrames cfg)

      pathFor i
        | i >= 0 && i < length paths = paths !! i
        | otherwise = []

      mkBuildup f =
        let i = min (max 0 (n - 1)) (f `div` fps_)
            settled = drawingFromLayers (take (i + 1) layers)
         in BuildupFrame
              f
              bbox
              (optimize settled)
              (activeOverlay cfg bbox layers i)
              (pathFor i)

      mkHold h = HoldFrame (nBuildup + h) bbox finalDrawing

      buildupFrames = if n == 0 then [] else map mkBuildup [0 .. nBuildup - 1]
      holdFrames = map mkHold [0 .. nHold - 1]
   in buildupFrames ++ holdFrames

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

-- | Compose the visible fills from a list of layers into a single 'Drawing'.
-- Geometric layers (intersections, n-gon vertices) and labels contribute
-- nothing — they are pure data dependencies, not visible output.
drawingFromLayers :: [ConstructionLayer] -> Drawing
drawingFromLayers = foldl' (<>) EmptyDrawing . map layerToFill

layerToFill :: ConstructionLayer -> Drawing
layerToFill (LayerTriangle col p1 p2 p3) = DrawTriangle col p1 p2 p3
layerToFill (LayerCircle col c e) = DrawCircle col c (dist c e)
layerToFill (LayerMasked mode content maskD) = DrawMasked mode content maskD
layerToFill (LayerSVGOverlay path c e) = DrawSVGOverlay path c e
layerToFill _ = EmptyDrawing

-- | Construction overlay element for the active step plus any trailing
-- previous steps drawn at reduced opacity.  Trail opacity fades linearly
-- from 'trailMaxOpacity' (most recent step) down to 'trailMinOpacity'
-- (oldest step in the trail), so a long trail still leaves the active step
-- visually dominant without making old history vanish.  The fade applies
-- only to the dotted strokes (lines/circles); result-point dots are kept
-- fully opaque and accumulated across /all/ previous steps (independent
-- of 'acTrailSteps'), so every constructed point stays visible for the
-- remainder of the animation.
--
-- @bbox@ is used to derive a @geomScale@ factor (@bboxWidth / svgWidthPx@)
-- so that construction markers (dots, strokes, dashes) are sized in pixels
-- rather than diagram-coordinate units, giving a consistent visual size
-- regardless of the flag's internal coordinate scale.
activeOverlay :: AnimationConfig -> BBox -> [ConstructionLayer] -> Int -> Element
activeOverlay cfg (minX, _, maxX, _) layers i =
  let geomScale = (maxX - minX) / acWidth cfg
      active = renderConstructionGeom geomScale (layers !! i)
      trailIdxs = [max 0 (i - acTrailSteps cfg) .. i - 1]
      total = length trailIdxs
      -- pos 0 = oldest in the trail, pos (total - 1) = most recent.
      opacityFor pos
        | total <= 0 = trailMaxOpacity
        | total == 1 = trailMaxOpacity
        | otherwise =
            let frac = fromIntegral pos / fromIntegral (total - 1) :: Double
             in trailMinOpacity + (trailMaxOpacity - trailMinOpacity) * frac
      trailStrokeEls =
        mconcat
          [ g_
              [makeAttribute "opacity" (opacityText (opacityFor pos))]
              (renderConstructionStrokes geomScale (layers !! j))
          | (pos, j) <- zip [0 :: Int ..] trailIdxs
          ]
      -- Dots from every prior step, drawn at full opacity so the set of
      -- constructed points only ever grows.
      priorDotEls =
        mconcat
          [ renderConstructionDots geomScale (layers !! j)
          | j <- [0 .. i - 1]
          ]
   in priorDotEls <> trailStrokeEls <> active
  where
    trailMinOpacity = 0.2
    trailMaxOpacity = 0.6

-- | Render a Double as a Text suitable for an SVG attribute value.
opacityText :: Double -> T.Text
opacityText d = T.pack (showFFloat (Just 3) d "")

-- | A solid white rectangle covering the entire bbox, in diagram
-- coordinates.  Inserted at the bottom of every frame canvas so the
-- rasterised PNG (and therefore the assembled movie) has an opaque white
-- background rather than alpha transparency, which encodes badly to GIF
-- and renders as black under most video pipelines.
whiteBackground :: BBox -> Element
whiteBackground (minX, minY, maxX, maxY) =
  rect_
    [ X_ <<- coord minX,
      Y_ <<- coord minY,
      Width_ <<- coord (maxX - minX),
      Height_ <<- coord (maxY - minY),
      Fill_ <<- "white",
      Stroke_ <<- "none"
    ]
  where
    coord d = T.pack (showFFloat (Just 6) d "")

-- ---------------------------------------------------------------------------
-- IO
-- ---------------------------------------------------------------------------

-- | Write a single frame to an SVG file.  Build-up frames skip overlay
-- injection (cheap and avoids a per-frame file read of overlay sources)
-- and get a lower-right caption naming the active step's enclosing
-- group-label path; hold frames go through the full canonical pipeline so
-- they exactly match the SVG the regular build emits, with no caption.
-- Both kinds get a solid white background so the encoded movie is opaque.
writeFrameSVG :: AnimationConfig -> FilePath -> Frame -> IO ()
writeFrameSVG cfg outPath (BuildupFrame _ bbox settled overlay path) = do
  let canvas = whiteBackground bbox <> drawingToElement settled <> overlay
      caption = formatGroupPath path
      captionEl =
        if T.null caption
          then mempty
          else buildCaptionElement bbox (acWidth cfg) caption
  TLIO.writeFile outPath (assembleSVGWithCaption (acWidth cfg) bbox canvas captionEl Map.empty [])
writeFrameSVG cfg outPath (HoldFrame _ bbox drawing) = do
  let opt = optimize drawing
      placements = extractOverlayPlacements opt
      canvas = whiteBackground bbox <> drawingToElement opt
  sources <- loadOverlaySources opt
  TLIO.writeFile outPath (assembleSVGWithCaption (acWidth cfg) bbox canvas mempty sources placements)

-- | Assemble a frame SVG document in memory.  @content@ is placed inside a
-- y-flip transform group (diagram coordinates → SVG coordinates).
-- @captionEl@ is placed as a direct sibling of that group — outside the
-- y-flip — so it is rendered right-side-up.  Pass 'mempty' for @captionEl@
-- when no caption is required.
assembleSVGWithCaption ::
  Double ->
  BBox ->
  Element ->
  Element ->
  Map.Map FilePath OverlaySource ->
  [OverlayPlacement] ->
  TL.Text
assembleSVGWithCaption svgW (minX, minY, maxX, maxY) content captionEl overlaySources placements =
  let vbW = maxX - minX
      vbH = maxY - minY
      svgH = svgW * vbH / vbW
      xform = "translate(" <> showT (- minX) <> "," <> showT maxY <> ") scale(1,-1)"
      doc =
        with
          (svg11_ (g_ [Transform_ <<- xform] content <> captionEl))
          [ ViewBox_ <<- ("0 0 " <> showT vbW <> " " <> showT vbH),
            Width_ <<- showT svgW,
            Height_ <<- showT svgH
          ]
      baseSvg = prettyText doc
   in if null placements
        then baseSvg
        else
          TL.fromStrict
            ( injectOverlays
                (TL.toStrict baseSvg)
                overlaySources
                placements
                (minX, minY, maxY)
            )

-- | Format a group path for display as a single caption line.  Empty
-- entries are filtered out; remaining entries are joined with " \x2013 "
-- (an en-dash with spaces on either side), so e.g.
-- @["Horizontal stripes", "Fill rectangle"]@ becomes
-- @"Horizontal stripes \x2013 Fill rectangle"@.
formatGroupPath :: [String] -> T.Text
formatGroupPath =
  T.intercalate (T.pack " \x2013 ") . map T.pack . filter (not . null)

-- | Build a caption bar 'Element' positioned at the bottom of the SVG
-- viewBox.
--
-- The caption is wrapped in a nested @\<svg\>@ element with its own
-- pixel-sized @viewBox@.  This keeps the @font-size@ in normal
-- integer-sized pixel units, which @rsvg-convert@ (versions \<= 2.54)
-- rasterises cleanly — tiny fractional viewBox units cause individual
-- glyph strokes to drop out and produce splotchy, unreadable text.
-- The nested @\<svg\>@ is placed as a sibling of the y-flipped diagram
-- group by 'assembleSVGWithCaption', so it is not affected by the
-- diagram's y-flip transform.
buildCaptionElement :: BBox -> Double -> T.Text -> Element
buildCaptionElement (minX, minY, maxX, maxY) svgW caption =
  let vbW = maxX - minX
      vbH = maxY - minY
      -- Pixel-space metrics
      fontPx = 14 :: Double
      paddingPx = 6 :: Double
      marginPx = 10 :: Double
      barHpx = fontPx + 2 * paddingPx
      -- Convert pixels to outer viewBox units
      pxToUnits px = px * vbW / svgW
      barHunits = pxToUnits barHpx
      barYunits = vbH - barHunits
      -- Inner coordinate system: 1 unit = 1 output pixel, so glyph
      -- metrics like font-size sit at human-scale integer values.
      innerVbW = svgW
      innerVbH = barHpx
      -- Text baseline: place it so descenders sit within the bar.
      textY = barHpx - paddingPx
   in with
        ( svg11_
            ( rect_
                [ X_ <<- "0",
                  Y_ <<- "0",
                  Width_ <<- showT innerVbW,
                  Height_ <<- showT innerVbH,
                  Fill_ <<- "black",
                  Fill_opacity_ <<- "0.5",
                  Stroke_ <<- "none"
                ]
                <> text_
                  [ X_ <<- showT marginPx,
                    Y_ <<- showT textY,
                    makeAttribute "font-family" "sans-serif",
                    makeAttribute "font-size" (showT fontPx),
                    makeAttribute "text-anchor" "start",
                    Fill_ <<- "white"
                  ]
                  (toElement caption)
            )
        )
        [ X_ <<- "0",
          Y_ <<- showT barYunits,
          Width_ <<- showT vbW,
          Height_ <<- showT barHunits,
          ViewBox_ <<- ("0 0 " <> showT innerVbW <> " " <> showT innerVbH),
          makeAttribute "preserveAspectRatio" "none"
        ]

-- | Show a 'Double' with reasonable precision for SVG numeric attributes.
showT :: Double -> T.Text
showT d = T.pack (showFFloat (Just 4) d "")

-- | Write a frame as PNG: render the SVG to disk, then convert to PNG via
-- @rsvg-convert@ (already required by the existing PNGBackend).
writeFramePNG :: AnimationConfig -> FilePath -> FilePath -> Frame -> IO ()
writeFramePNG cfg svgPath pngPath frame = do
  writeFrameSVG cfg svgPath frame
  callProcess "rsvg-convert" [svgPath, "-o", pngPath]

-- | Write all frames into @outDir@ as @frame-NNNNN.svg@ + @frame-NNNNN.png@
-- pairs.  Returns the list of PNG paths in order.
writeAllFrames :: AnimationConfig -> FilePath -> [Frame] -> IO [FilePath]
writeAllFrames cfg outDir frames = do
  createDirectoryIfMissing True outDir
  forM (zip [0 ..] frames) $ \(i, frame) -> do
    let svgPath = outDir </> printf "frame-%05d.svg" (i :: Int)
        pngPath = outDir </> printf "frame-%05d.png" (i :: Int)
    writeFramePNG cfg svgPath pngPath frame
    pure pngPath

-- | Run @ffmpeg@ to assemble PNG frames into the requested format.  Frames
-- are expected to be named @frame-NNNNN.png@ inside @framesDir@.
encodeAnimation :: AnimationConfig -> OutputFormat -> FilePath -> FilePath -> IO ()
encodeAnimation cfg fmt framesDir outPath = do
  let pat = framesDir </> "frame-%05d.png"
      fps = show (acFps cfg)
  case fmt of
    FmtGif -> do
      -- Two-pass palette-optimised GIF encode.
      --
      -- A direct encode (or ffmpeg's default 256-colour palette) does not
      -- contain enough neutral greys, so cross-fades between flag layers
      -- get quantised to the nearest available colours -- which tend to
      -- carry a warm cast, producing visible yellowing on mid-greys.
      --
      -- We previously avoided the two-pass approach because of fragility
      -- with the single-command @split + paletteuse@ filtergraph (only
      -- one output frame) and "Internal bug, should not have happened"
      -- crashes under @image2 + paletteuse@ in one invocation.  Splitting
      -- it into two separate @ffmpeg@ runs with an explicit intermediate
      -- palette PNG sidesteps both issues and is well-supported across
      -- ffmpeg builds.
      --
      -- @stats_mode=full@ weights every frame equally so the palette
      -- covers the full fade range; @sierra2_4a@ dithering gives smooth
      -- gradients without the colour drift of @bayer@.
      let palettePath = framesDir </> "palette.png"
      callProcess
        "ffmpeg"
        [ "-y",
          "-framerate",
          fps,
          "-i",
          pat,
          "-vf",
          "palettegen=stats_mode=full",
          palettePath
        ]
      callProcess
        "ffmpeg"
        [ "-y",
          "-framerate",
          fps,
          "-i",
          pat,
          "-i",
          palettePath,
          "-lavfi",
          "paletteuse=dither=sierra2_4a",
          "-loop",
          "0",
          outPath
        ]
    FmtMp4 ->
      callProcess
        "ffmpeg"
        [ "-y",
          "-framerate",
          fps,
          "-i",
          pat,
          "-c:v",
          "libx264",
          "-pix_fmt",
          "yuv420p",
          "-movflags",
          "+faststart",
          -- h264 requires even dimensions
          "-vf",
          "scale=trunc(iw/2)*2:trunc(ih/2)*2",
          outPath
        ]
    FmtWebm ->
      callProcess
        "ffmpeg"
        [ "-y",
          "-framerate",
          fps,
          "-i",
          pat,
          "-c:v",
          "libvpx-vp9",
          "-pix_fmt",
          "yuva420p",
          "-b:v",
          "0",
          "-crf",
          "30",
          outPath
        ]
    FmtApng ->
      callProcess
        "ffmpeg"
        [ "-y",
          "-framerate",
          fps,
          "-i",
          pat,
          "-plays",
          "0",
          outPath
        ]
    FmtWebp ->
      callProcess
        "ffmpeg"
        [ "-y",
          "-framerate",
          fps,
          "-i",
          pat,
          "-c:v",
          "libwebp",
          "-loop",
          "0",
          "-lossless",
          "1",
          outPath
        ]
