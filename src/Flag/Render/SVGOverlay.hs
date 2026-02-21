{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.SVGOverlay
    ( OverlaySource(..)
    , OverlayPlacement(..)
    , loadOverlaySources
    , extractOverlayPlacements
    , injectOverlays
    ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Numeric (showFFloat)

import Flag.Construction.Types (Drawing(..))
import Flag.Construction.Radical (toDouble)

-- | Parsed SVG overlay source: raw inner content plus original dimensions.
data OverlaySource = OverlaySource
    { overlayContent :: T.Text   -- ^ Inner SVG content (xml decl and outer <svg> stripped)
    , overlayWidth   :: Double   -- ^ Original width from the SVG root element
    , overlayHeight  :: Double   -- ^ Original height from the SVG root element
    }

-- | Placement info for an overlay extracted from the Drawing tree.
data OverlayPlacement = OverlayPlacement
    { opFilePath :: FilePath
    , opCenter   :: (Double, Double)  -- ^ Center point in diagram coordinates
    , opEdge     :: (Double, Double)  -- ^ Edge point in diagram coordinates (defines radius)
    }

-- ---------------------------------------------------------------------------
-- Loading overlay sources
-- ---------------------------------------------------------------------------

-- | Load all SVG overlay files referenced in a Drawing as raw text,
-- extracting their dimensions. Files that fail to load are omitted with a
-- warning.
loadOverlaySources :: Drawing -> IO (Map FilePath OverlaySource)
loadOverlaySources drawing = do
    let paths = nub (extractPaths drawing)
    pairs <- mapM loadOne paths
    pure (Map.fromList [(p, s) | (p, Just s) <- pairs])
  where
    loadOne path = do
        result <- tryLoadSVG path
        case result of
            Left err -> do
                putStrLn $ "Warning: could not load SVG overlay " ++ show path ++ ": " ++ err
                pure (path, Nothing)
            Right src -> pure (path, Just src)

    extractPaths :: Drawing -> [FilePath]
    extractPaths EmptyDrawing = []
    extractPaths (Overlay a b) = extractPaths a ++ extractPaths b
    extractPaths (DrawSVGOverlay path _ _) = [path]
    extractPaths _ = []

-- | Try to load and parse an SVG file into an OverlaySource.
tryLoadSVG :: FilePath -> IO (Either String OverlaySource)
tryLoadSVG path = do
    txt <- TIO.readFile path
    pure $ parseSVG txt

-- | Parse SVG text: extract width/height from root <svg> and inner content.
parseSVG :: T.Text -> Either String OverlaySource
parseSVG txt = do
    let stripped = stripXmlDecl (stripDoctype txt)
    (svgTag, rest) <- findSvgTag stripped
    w <- extractAttr "width" svgTag
    h <- extractAttr "height" svgTag
    wVal <- parseDouble w
    hVal <- parseDouble h
    let inner = stripInkscapeMetadata (extractInnerContent rest)
    Right OverlaySource
        { overlayContent = inner
        , overlayWidth   = wVal
        , overlayHeight  = hVal
        }

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
-- placements, the diagram bounding box corners (minX, minY, maxY), and the
-- SVG output width to determine the coordinate transform.
injectOverlays
    :: T.Text                    -- ^ Rendered SVG text
    -> Map FilePath OverlaySource
    -> [OverlayPlacement]
    -> (Double, Double, Double)  -- ^ (minX, minY, maxY) of diagram bounding box
    -> Double                    -- ^ Diagram width (in diagram coordinates)
    -> Double                    -- ^ SVG output width (pixels, from mkWidth)
    -> T.Text
injectOverlays svgText sources placements (minX, _minY, maxY) diagramW svgW =
    let scaleFactor = svgW / diagramW
        overlayElements = concatMap (renderOverlay sources scaleFactor minX maxY) placements
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
    -> Double   -- ^ scale factor (SVG pixels per diagram unit)
    -> Double   -- ^ diagram bounding box minX
    -> Double   -- ^ diagram bounding box maxY
    -> OverlayPlacement
    -> [T.Text]
renderOverlay sources scaleFactor minX maxY placement =
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
                -- Display size in diagram coordinates
                dispW = 2 * r * ow / overDiag
                dispH = 2 * r * oh / overDiag
                -- Convert to SVG coordinates
                dispW_svg = dispW * scaleFactor
                dispH_svg = dispH * scaleFactor
                -- Center in SVG coordinates (y-flipped)
                -- SVG x = (diagramX - minX) * scale
                -- SVG y = (maxY - diagramY) * scale
                cx_svg = (cx - minX) * scaleFactor
                cy_svg = (maxY - cy) * scaleFactor
                -- Top-left corner
                x_svg = cx_svg - dispW_svg / 2
                y_svg = cy_svg - dispH_svg / 2
                -- viewBox from original dimensions
                viewBox = "0 0 " <> showT ow <> " " <> showT oh
            in [ "\n<svg x=\"" <> showT x_svg
                 <> "\" y=\"" <> showT y_svg
                 <> "\" width=\"" <> showT dispW_svg
                 <> "\" height=\"" <> showT dispH_svg
                 <> "\" viewBox=\"" <> viewBox
                 <> "\" xmlns=\"http://www.w3.org/2000/svg\">\n"
                 <> overlayContent src
                 <> "\n</svg>\n"
               ]

-- | Show a Double with reasonable precision for SVG attributes.
showT :: Double -> T.Text
showT d = T.pack (showFFloat (Just 4) d "")
