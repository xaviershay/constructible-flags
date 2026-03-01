{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.SVGOverlay
    ( OverlaySource(..)
    , OverlayPlacement(..)
    , loadOverlaySources
    , extractOverlayPlacements
    , injectOverlays
    , renderDrawingToSVG
    , drawingBounds
    , writeSVG
    -- *Internal helpers (exported for tests)
    , parseSVG
    ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Read as TR
import Numeric (showFFloat)
import Graphics.Svg

import Flag.Construction.Types (Drawing(..), Point)
import Flag.Construction.FieldNumber (toDouble)
import Flag.Construction.Optimize (optimize)
import Flag.Render.Diagram (drawingToElement)
import Control.Monad (when)

-- | Parsed SVG overlay source: raw inner content plus original dimensions.
data OverlaySource = OverlaySource
    { overlayContent :: T.Text   -- ^ Inner SVG content (xml decl and outer <svg> stripped)
    , overlayWidth   :: Double   -- ^ Original width from the SVG root element
    , overlayHeight  :: Double   -- ^ Original height from the SVG root element
    } deriving (Show)

-- | Placement info for an overlay extracted from the Drawing tree.
data OverlayPlacement = OverlayPlacement
    { opFilePath :: FilePath
    , opCenter   :: (Double, Double)  -- ^ Center point in diagram coordinates
    , opEdge     :: (Double, Double)  -- ^ Edge point in diagram coordinates (defines radius)
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- Bounding box
-- ---------------------------------------------------------------------------

type BBox = (Double, Double, Double, Double)  -- ^ (minX, minY, maxX, maxY)

unionBBox :: BBox -> BBox -> BBox
unionBBox (ax1, ay1, ax2, ay2) (bx1, by1, bx2, by2) =
    (min ax1 bx1, min ay1 by1, max ax2 bx2, max ay2 by2)

-- | Compute the bounding box of a 'Drawing' from its geometry.
-- Returns 'Nothing' for 'EmptyDrawing'.
drawingBounds :: Drawing -> Maybe BBox
drawingBounds EmptyDrawing = Nothing
drawingBounds (Overlay a b) =
    case (drawingBounds a, drawingBounds b) of
        (Nothing, x)    -> x
        (x, Nothing)    -> x
        (Just ba, Just bb) -> Just (unionBBox ba bb)
drawingBounds (DrawTriangle _ p1 p2 p3) =
    let pts = map toDP [p1, p2, p3]
    in Just (minimum (map fst pts), minimum (map snd pts),
             maximum (map fst pts), maximum (map snd pts))
drawingBounds (DrawPath _ []) = Nothing
drawingBounds (DrawPath _ pts) =
    let dpts = map toDP pts
    in Just (minimum (map fst dpts), minimum (map snd dpts),
             maximum (map fst dpts), maximum (map snd dpts))
drawingBounds (DrawCircle _ center rd) =
    let (cx, cy) = toDP center
        r = toDouble rd
    in Just (cx - r, cy - r, cx + r, cy + r)
drawingBounds (DrawCrescent _ outerCenter outerR _ _) =
    let (ocx, ocy) = toDP outerCenter
        r = toDouble outerR
    in Just (ocx - r, ocy - r, ocx + r, ocy + r)
-- SVG overlays are injected as post-processing and do not contribute to the
-- viewBox bounding box, matching the old Diagrams behaviour where
-- drawingToDiagram (DrawSVGOverlay _ _ _) = mempty.
drawingBounds (DrawSVGOverlay _ _ _) = Nothing

toDP :: Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

-- ---------------------------------------------------------------------------
-- SVG file writing
-- ---------------------------------------------------------------------------

-- | Write an SVG 'Element' to a file at the given pixel width.
-- The bounding box is used to compute the viewBox and aspect ratio.
-- A y-flip transform is applied so that construction coordinates (y-up)
-- map correctly to SVG coordinates (y-down).
writeSVG :: FilePath -> Double -> BBox -> Element -> IO ()
writeSVG path svgW (minX, minY, maxX, maxY) content = do
    let vbW  = maxX - minX
        vbH  = maxY - minY
        svgH = svgW * vbH / vbW
        -- translate so (minX,maxY) → (0,0), then flip y-axis
        xform = "translate(" <> showT (-minX) <> "," <> showT maxY <> ") scale(1,-1)"
        doc = with (svg11_ (g_ [Transform_ <<- xform] content))
                   [ ViewBox_ <<- ("0 0 " <> showT vbW <> " " <> showT vbH)
                   , Width_   <<- showT svgW
                   , Height_  <<- showT svgH
                   ]
    TLIO.writeFile path (prettyText doc)

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
    Right OverlaySource
        { overlayContent = inner
        , overlayWidth   = wVal
        , overlayHeight  = hVal
        }

-- | Look for an attribute in a tag and return 'Nothing' if not present.
maybeAttr :: T.Text -> T.Text -> Maybe T.Text
maybeAttr name tag =
    case extractAttr name tag of
        Right v -> Just v
        Left _  -> Nothing

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
            (_, after) | T.null after -> t
                       | otherwise    -> T.drop 2 after
        Nothing -> t

-- | Strip DOCTYPE declaration (<!DOCTYPE ... >)
stripDoctype :: T.Text -> T.Text
stripDoctype t =
    let trimmed = T.stripStart t
    in case T.stripPrefix "<!DOCTYPE" trimmed of
        Just rest -> case T.breakOn ">" rest of
            (_, after) | T.null after -> t
                       | otherwise    -> T.drop 1 after
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
    let patterns = [ name <> "=\"", name <> "='" ]
        tryPattern [] = Left $ "Attribute " ++ T.unpack name ++ " not found"
        tryPattern (p:ps) =
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
        Left err     -> Left $ "Could not parse double: " ++ err

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
                            (_, rest2) | T.null rest2 ->
                                -- No closing tag, try self-closing
                                if T.null rest1 then t
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
       then t  -- no closing tag found, return everything
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
        { opFilePath = path
        , opCenter   = (toDouble (fst center), toDouble (snd center))
        , opEdge     = (toDouble (fst edge), toDouble (snd edge))
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
injectOverlays
    :: T.Text                    -- ^ Rendered SVG text
    -> Map FilePath OverlaySource
    -> [OverlayPlacement]
    -> (Double, Double, Double)  -- ^ (minX, minY, maxY) of diagram bounding box
    -> T.Text
injectOverlays svgText sources placements (minX, _minY, maxY) =
    let overlayElements = concatMap (renderOverlay sources minX maxY) placements
        injection = T.concat overlayElements
    in if T.null injection
       then svgText
       else
           -- Insert before the closing </svg> tag
           let (before, after) = T.breakOnEnd "</svg>" svgText
           in if T.null before
              then svgText  -- no closing tag found
              else T.dropEnd (T.length ("</svg>" :: T.Text)) before <> injection <> "</svg>" <> after


-- | Render a single overlay placement as an SVG element.
renderOverlay
    :: Map FilePath OverlaySource
    -> Double   -- ^ diagram bounding box minX
    -> Double   -- ^ diagram bounding box maxY
    -> OverlayPlacement
    -> [T.Text]
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
            in [ "\n<g transform=\"translate(" <> showT x_svg
                 <> "," <> showT y_svg
                 <> ") scale(" <> showTScale sx <> "," <> showTScale sy
                 <> ")\">\n"
                 <> overlayContent src
                 <> "\n</g>\n"
               ]

-- | Render an already-optimized 'Drawing' to an SVG file, injecting any
-- SVG overlays found in the drawing.
renderOptimizedDrawingToSVG
    :: FilePath    -- ^ output path
    -> Double      -- ^ SVG width (pixels)
    -> Drawing     -- ^ optimized drawing (typically produced by 'optimize')
    -> IO ()
renderOptimizedDrawingToSVG outPath svgOutputWidth optimizedDrawing = do
    let (minX, minY, maxX, maxY) = case drawingBounds optimizedDrawing of
                                       Just bb -> bb
                                       Nothing -> (0, 0, 1, 1)
        placements = extractOverlayPlacements optimizedDrawing
    overlaySources <- loadOverlaySources optimizedDrawing
    let element = drawingToElement optimizedDrawing
    writeSVG outPath svgOutputWidth (minX, minY, maxX, maxY) element
    when (not (null placements)) $ do
        svgText <- TIO.readFile outPath
        let result = injectOverlays svgText overlaySources placements
                      (minX, minY, maxY)
        TIO.writeFile outPath result

-- | Convenience wrapper that takes an arbitrary 'Drawing' and optimizes it
-- before rendering.
renderDrawingToSVG :: FilePath -> Double -> Drawing -> IO ()
renderDrawingToSVG outPath svgWidth drawing =
    renderOptimizedDrawingToSVG outPath svgWidth (optimize drawing)

-- | Show a Double with reasonable precision for SVG attributes.
showT :: Double -> T.Text
showT d = T.pack (showFFloat (Just 4) d "")

-- | Show a Double with higher precision, used for scale factors that can
-- be small (e.g. 0.003187) where 4 decimal places gives too few sig figs.
showTScale :: Double -> T.Text
showTScale d = T.pack (showFFloat (Just 8) d "")
