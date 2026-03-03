{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.SVGOverlay
  ( OverlaySource (..),
    OverlayPlacement (..),
    loadOverlaySources,
    extractOverlayPlacements,
    injectOverlays,
    assembleSVG,
    writeSVG,
    renderDrawingToText,

    -- * Re-exported from Flag.Render.Bounds
    drawingBounds,

    -- * Internal helpers (exported for tests)
    parseSVG,
  )
where

import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Read as TR
import Flag.Construction.FieldNumber (toDouble)
import Flag.Construction.Optimize (optimize)
import Flag.Construction.Types (Drawing (..))
import Flag.Render.Bounds (BBox, drawingBounds)
import Graphics.Svg
import Numeric (showFFloat)

-- | Parsed SVG overlay source: raw inner content plus original dimensions.
data OverlaySource = OverlaySource
  { -- | Inner SVG content (xml decl and outer <svg> stripped)
    overlayContent :: T.Text,
    -- | Original width from the SVG root element
    overlayWidth :: Double,
    -- | Original height from the SVG root element
    overlayHeight :: Double
  }
  deriving (Show)

-- | Placement info for an overlay extracted from the Drawing tree.
data OverlayPlacement = OverlayPlacement
  { opFilePath :: FilePath,
    -- | Center point in diagram coordinates
    opCenter :: (Double, Double),
    -- | Edge point in diagram coordinates (defines radius)
    opEdge :: (Double, Double)
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- SVG file writing
-- ---------------------------------------------------------------------------

-- | Assemble a canvas 'Element' into a complete SVG document as lazy 'TL.Text',
-- injecting any overlay sources at the given placements — all in memory.
-- The bounding box is used to compute the viewBox and aspect ratio.
-- A y-flip transform is applied so that construction coordinates (y-up)
-- map correctly to SVG coordinates (y-down).
assembleSVG :: Double -> BBox -> Element -> Map FilePath OverlaySource -> [OverlayPlacement] -> TL.Text
assembleSVG svgW (minX, minY, maxX, maxY) content overlaySources placements =
  let vbW = maxX - minX
      vbH = maxY - minY
      svgH = svgW * vbH / vbW
      xform = "translate(" <> showT (-minX) <> "," <> showT maxY <> ") scale(1,-1)"
      doc =
        with
          (svg11_ (g_ [Transform_ <<- xform] content))
          [ ViewBox_ <<- ("0 0 " <> showT vbW <> " " <> showT vbH),
            Width_ <<- showT svgW,
            Height_ <<- showT svgH
          ]
      baseSvg = prettyText doc
  in if null placements
       then baseSvg
       else TL.fromStrict
              (injectOverlays
                (TL.toStrict baseSvg)
                overlaySources
                placements
                (minX, minY, maxY))

-- | Write an SVG 'Element' to a file at the given pixel width, injecting
-- overlays in memory before writing.  No file read-back occurs.
writeSVG :: FilePath -> Double -> BBox -> Element -> Map FilePath OverlaySource -> [OverlayPlacement] -> IO ()
writeSVG path svgW bbox content overlaySources placements =
  TLIO.writeFile path (assembleSVG svgW bbox content overlaySources placements)

-- | Render a 'Drawing' to fully-assembled SVG text in memory, then write to
-- a file.  Takes a function to convert a 'Drawing' to an 'Element' so that
-- this module does not need to import 'Flag.Render.Diagram' (which would
-- create a cycle via 'SVGBuilderBackend').
renderDrawingToText :: (Drawing -> Element) -> FilePath -> Double -> Drawing -> IO ()
renderDrawingToText toElement outPath svgWidth drawing = do
  let optimized = optimize drawing
      bbox = case drawingBounds optimized of
               Just bb -> bb
               Nothing -> (0, 0, 1, 1)
      placements = extractOverlayPlacements optimized
      canvas     = toElement optimized
  overlaySources <- loadOverlaySources optimized
  TLIO.writeFile outPath (assembleSVG svgWidth bbox canvas overlaySources placements)

-- ---------------------------------------------------------------------------
-- Loading overlay sources
-- ---------------------------------------------------------------------------

-- | Load all SVG overlay files referenced in a Drawing as raw text,
-- extracting their dimensions. Files that fail to load or parse are
-- reported as fatal errors.
loadOverlaySources :: Drawing -> IO (Map FilePath OverlaySource)
loadOverlaySources drawing = do
  let paths = nub (extractPaths drawing)
  sources <- mapM loadOne paths
  pure (Map.fromList sources)
  where
    loadOne path = do
      txt <- TIO.readFile path
      case parseSVG txt of
        Left err ->
          ioError $ userError $ "could not parse SVG overlay " ++ path ++ ": " ++ err
        Right src -> pure (path, src)

    extractPaths :: Drawing -> [FilePath]
    extractPaths EmptyDrawing = []
    extractPaths (Overlay a b) = extractPaths a ++ extractPaths b
    extractPaths (DrawSVGOverlay path _ _) = [path]
    extractPaths _ = []

-- | Parse SVG text: extract width/height from root <svg> and inner content.
--
-- Historically we expected the root <svg> element to have explicit
-- "width" and "height" attributes.  Many tools will
-- omit them and rely solely on a "viewBox" attribute instead; the
-- example at "data/images/btn/dragon-optimized.svg" falls into this
-- category.  The previous implementation returned a parsing error in that
-- case and the overlay was silently skipped.  We now fall back to the
-- width and height encoded in the viewBox when the explicit attributes are
-- missing.  Any remaining failure (missing viewBox or bad numbers) is
-- reported as an error so the program can fail fast.
parseSVG :: T.Text -> Either String OverlaySource
parseSVG txt = do
  let stripped = stripXmlDecl (stripDoctype txt)
  (svgTag, rest) <- findSvgTag stripped
  -- try width/height attributes first; fall back to viewBox
  wVal <- case maybeAttr "width" svgTag of
    Just w -> parseDouble w
    Nothing -> parseViewBoxDim svgTag 2
  hVal <- case maybeAttr "height" svgTag of
    Just h -> parseDouble h
    Nothing -> parseViewBoxDim svgTag 3
  let inner = stripInkscapeMetadata (extractInnerContent rest)
  Right
    OverlaySource
      { overlayContent = inner,
        overlayWidth = wVal,
        overlayHeight = hVal
      }

-- | Look for an attribute in a tag and return 'Nothing' if not present.
maybeAttr :: T.Text -> T.Text -> Maybe T.Text
maybeAttr name tag =
  case extractAttr name tag of
    Right v -> Just v
    Left _ -> Nothing

-- | Extract a specific dimension from the viewBox attribute.  The index
-- indicates which field to parse: 0=min-x, 1=min-y, 2=width, 3=height.
parseViewBoxDim :: T.Text -> Int -> Either String Double
parseViewBoxDim tag idx = do
  vb <- extractAttr "viewBox" tag
  -- split on whitespace, allow multiple spaces
  let parts = T.words vb
  if length parts == 4
    then parseDouble (parts !! idx)
    else Left $ "viewBox does not have four components: " ++ T.unpack vb

-- | Strip XML declaration (<?xml ... ?>)
stripXmlDecl :: T.Text -> T.Text
stripXmlDecl t =
  case T.stripPrefix "<?" t of
    Just rest -> case T.breakOn "?>" rest of
      (_, after)
        | T.null after -> t
        | otherwise -> T.drop 2 after
    Nothing -> t

-- | Strip DOCTYPE declaration (<!DOCTYPE ... >)
stripDoctype :: T.Text -> T.Text
stripDoctype t =
  let trimmed = T.stripStart t
   in case T.stripPrefix "<!DOCTYPE" trimmed of
        Just rest -> case T.breakOn ">" rest of
          (_, after)
            | T.null after -> t
            | otherwise -> T.drop 1 after
        Nothing -> t

-- | Find the opening <svg ...> tag. Returns (tag text, rest after '>').
findSvgTag :: T.Text -> Either String (T.Text, T.Text)
findSvgTag t =
  let trimmed = T.stripStart t
   in case T.stripPrefix "<svg" trimmed of
        Nothing -> Left "No <svg> tag found"
        Just rest ->
          let (tagBody, after) = T.breakOn ">" rest
           in if T.null after
                then Left "Unclosed <svg> tag"
                else Right ("<svg" <> tagBody <> ">", T.drop 1 after)

-- | Extract an attribute value from a tag string.
-- Handles both single and double quotes.
extractAttr :: T.Text -> T.Text -> Either String T.Text
extractAttr name tag =
  let patterns = [name <> "=\"", name <> "='"]
      tryPattern [] = Left $ "Attribute " ++ T.unpack name ++ " not found"
      tryPattern (p : ps) =
        case T.breakOn p tag of
          (_, match) | T.null match -> tryPattern ps
          (_, match) ->
            let afterEq = T.drop (T.length p) match
                quote = T.last p
                (val, _) = T.breakOn (T.singleton quote) afterEq
             in Right val
   in tryPattern patterns

-- | Parse a Text as Double.
parseDouble :: T.Text -> Either String Double
parseDouble t =
  case TR.double t of
    Right (d, _) -> Right d
    Left err -> Left $ "Could not parse double: " ++ err

-- | Strip Inkscape/Sodipodi metadata elements and attributes that cause
-- namespace errors when embedded in another SVG.
stripInkscapeMetadata :: T.Text -> T.Text
stripInkscapeMetadata = stripSodipodiNamedview . stripDefs
  where
    -- Remove <sodipodi:namedview ... /> or <sodipodi:namedview ...>...</sodipodi:namedview>
    stripSodipodiNamedview t =
      case T.breakOn "<sodipodi:namedview" t of
        (_, match) | T.null match -> t
        (before, match) ->
          -- Try self-closing first
          case T.breakOn "/>" match of
            (tag1, rest1) ->
              -- Also try closing tag
              case T.breakOn "</sodipodi:namedview>" match of
                (_, rest2)
                  | T.null rest2 ->
                      -- No closing tag, try self-closing
                      if T.null rest1
                        then t
                        else before <> T.drop 2 rest1
                (tag2, rest2) ->
                  -- Pick whichever ends first
                  if T.length tag1 < T.length tag2
                    then before <> T.drop 2 rest1
                    else before <> T.drop (T.length ("</sodipodi:namedview>" :: T.Text)) rest2
    -- Remove empty <defs .../> elements (Inkscape adds these with ids)
    stripDefs t =
      case T.breakOn "<defs" t of
        (_, match) | T.null match -> t
        (before, match) ->
          case T.breakOn "/>" match of
            (_, rest) | T.null rest -> t
            (tag, rest) ->
              -- Only strip if it's a self-closing empty defs
              if not (T.isInfixOf ">" (T.takeWhile (/= '/') (T.drop 5 tag)))
                then before <> T.drop 2 rest
                else t

-- | Extract content between the end of the opening <svg> tag and the last </svg>.
extractInnerContent :: T.Text -> T.Text
extractInnerContent t =
  -- Find the last </svg> and take everything before it
  let (before, _) = T.breakOnEnd "</svg>" t
   in if T.null before
        then t -- no closing tag found, return everything
        else T.dropEnd (T.length ("</svg>" :: T.Text)) before

-- ---------------------------------------------------------------------------
-- Overlay placements from Drawing
-- ---------------------------------------------------------------------------

-- | Extract overlay placement info from a Drawing tree.
extractOverlayPlacements :: Drawing -> [OverlayPlacement]
extractOverlayPlacements EmptyDrawing = []
extractOverlayPlacements (Overlay a b) =
  extractOverlayPlacements a ++ extractOverlayPlacements b
extractOverlayPlacements (DrawSVGOverlay path center edge) =
  [ OverlayPlacement
      { opFilePath = path,
        opCenter = (toDouble (fst center), toDouble (snd center)),
        opEdge = (toDouble (fst edge), toDouble (snd edge))
      }
  ]
extractOverlayPlacements _ = []

-- ---------------------------------------------------------------------------
-- SVG injection (post-processing)
-- ---------------------------------------------------------------------------

-- | Inject raw SVG overlays into a rendered SVG file.
--
-- Takes the rendered SVG text, a map of overlay sources, a list of
-- placements, and the diagram bounding box corners (minX, minY, maxY).
-- Coordinates are placed in viewBox units, which equal diagram units
-- because writeSVG sets viewBox = "0 0 diagramW diagramH".
injectOverlays ::
  -- | Rendered SVG text
  T.Text ->
  Map FilePath OverlaySource ->
  [OverlayPlacement] ->
  -- | (minX, minY, maxY) of diagram bounding box
  (Double, Double, Double) ->
  T.Text
injectOverlays svgText sources placements (minX, _minY, maxY) =
  let overlayElements = concatMap (renderOverlay sources minX maxY) placements
      injection = T.concat overlayElements
   in if T.null injection
        then svgText
        else
          -- Insert before the closing </svg> tag
          let (before, after) = T.breakOnEnd "</svg>" svgText
           in if T.null before
                then svgText -- no closing tag found
                else T.dropEnd (T.length ("</svg>" :: T.Text)) before <> injection <> "</svg>" <> after

-- | Render a single overlay placement as an SVG element.
renderOverlay ::
  Map FilePath OverlaySource ->
  -- | diagram bounding box minX
  Double ->
  -- | diagram bounding box maxY
  Double ->
  OverlayPlacement ->
  [T.Text]
renderOverlay sources minX maxY placement =
  case Map.lookup (opFilePath placement) sources of
    Nothing -> []
    Just src ->
      let (cx, cy) = opCenter placement
          (ex, ey) = opEdge placement
          -- Radius in diagram coordinates
          r = sqrt ((ex - cx) ** 2 + (ey - cy) ** 2)
          -- Overlay original dimensions
          ow = overlayWidth src
          oh = overlayHeight src
          overDiag = sqrt (ow * ow + oh * oh)
          -- Display size in viewBox units (= diagram units, since viewBox == diagram space)
          dispW = 2 * r * ow / overDiag
          dispH = 2 * r * oh / overDiag
          -- Center in viewBox coordinates (y-flipped):
          --   vb_x = diagramX - minX
          --   vb_y = maxY - diagramY
          cx_svg = cx - minX
          cy_svg = maxY - cy
          -- Top-left corner
          x_svg = cx_svg - dispW / 2
          y_svg = cy_svg - dispH / 2
          -- scale factors from original overlay dimensions to viewBox units
          -- (use higher precision: these are small numbers like 0.003187 and
          -- showT's 4 decimal places gives only 2 significant figures)
          sx = dispW / ow
          sy = dispH / oh
       in [ "\n<g transform=\"translate("
              <> showT x_svg
              <> ","
              <> showT y_svg
              <> ") scale("
              <> showTScale sx
              <> ","
              <> showTScale sy
              <> ")\">\n"
              <> overlayContent src
              <> "\n</g>\n"
          ]

-- ---------------------------------------------------------------------------
-- Internal text helpers
-- ---------------------------------------------------------------------------

-- | Show a Double with reasonable precision for SVG attributes.
showT :: Double -> T.Text
showT d = T.pack (showFFloat (Just 4) d "")

-- | Show a Double with higher precision, used for scale factors that can
-- be small (e.g. 0.003187) where 4 decimal places gives too few sig figs.
showTScale :: Double -> T.Text
showTScale d = T.pack (showFFloat (Just 8) d "")
