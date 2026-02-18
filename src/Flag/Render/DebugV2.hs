{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.DebugV2
    ( writeDebugViewer
    , writeConstructionJson
    ) where

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB, channelRed, channelGreen, channelBlue)
import Numeric (showFFloat, showHex)
import System.Directory (createDirectoryIfMissing, copyFile)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Flag.Construction.Radical (Radical, toDouble, toKaTeX)
import Flag.Construction.Types (Point)
import Flag.Construction.Layers (ConstructionLayer(..), layerInputPoints,
                                 layerOutputPoints, pointDist)
import Flag.Construction.Tree (ConstructionTree)
import Flag.Render.Debug (NumberedEntry(..), numberTree, numberedLeaves)

-- | Convert a Point (Radical, Radical) to (Double, Double) for SVG rendering.
toDP :: Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

-- | Convert a Radical to Double for SVG rendering.
toD :: Radical -> Double
toD = toDouble

-- ---------------------------------------------------------------------------
-- Build debug V2 output
-- ---------------------------------------------------------------------------

-- | Write the debug-v2 viewer HTML shell and JS (once, not per flag).
writeDebugViewer :: IO ()
writeDebugViewer = do
  createDirectoryIfMissing True "out/debug-v2"
  writeFile "out/debug-v2/index.html" generateHtmlShell
  copyFile "sources/debug-v2.js" "out/debug-v2/debug-v2.js"
  putStrLn "  Wrote out/debug-v2/index.html"
  putStrLn "  Wrote out/debug-v2/debug-v2.js"

-- | Write the construction JSON for a single flag.
-- Takes the flag name, ISO code, initial points, and pre-evaluated construction tree.
writeConstructionJson :: String -> String -> (Point, Point) -> [ConstructionTree] -> IO ()
writeConstructionJson name isoCode input tree = do
  createDirectoryIfMissing True "out/construction"

  let (_, numbered) = numberTree 1 tree
      leaves = numberedLeaves numbered
      allLayers = map snd leaves
      initialPts = [fst input, snd input]

  -- Compute bounding box from all points (convert to Double for SVG viewBox)
  let allPoints = initialPts
                  ++ concatMap layerInputPoints allLayers
                  ++ concatMap layerOutputPoints allLayers
      xs = map (toDouble . fst) allPoints
      ys = map (toDouble . snd) allPoints
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
      -- Add padding (20%)
      padX = (maxX - minX) * 0.15
      padY = (maxY - minY) * 0.15
      vbMinX = minX - padX
      vbMinY = minY - padY
      vbW = (maxX - minX) + 2 * padX
      vbH = (maxY - minY) + 2 * padY

  -- Compute fill bounding box from only filled layers (triangles + circles)
  let fillBounds = concatMap fillBoundingPoints allLayers
      fxs = map fst fillBounds
      fys = map snd fillBounds
      fMinX = minimum fxs
      fMaxX = maximum fxs
      fMinY = minimum fys
      fMaxY = maximum fys
      fPadX = (fMaxX - fMinX) * 0.30
      fPadY = (fMaxY - fMinY) * 0.30
      fvbMinX = fMinX - fPadX
      fvbMinY = fMinY - fPadY
      fvbW = (fMaxX - fMinX) + 2 * fPadX
      fvbH = (fMaxY - fMinY) + 2 * fPadY

  -- Compute live-after sets (single backward pass using Double-keyed Sets).
  let inputPtSets = map (Set.fromList . map toDP . layerInputPoints) allLayers
      liveAfterSets = scanr Set.union Set.empty inputPtSets

  -- Pre-compute cumulative output points (as Double pairs) per step.
  let outputPtLists = map (map toDP . layerOutputPoints) allLayers
      prevOutputsDbl = scanl (\acc outs -> acc ++ outs) [] outputPtLists

  -- Build JSON using Aeson
  let viewBox = showF vbMinX ++ " " ++ showF vbMinY
                ++ " " ++ showF vbW ++ " " ++ showF vbH
      fillViewBox = showF fvbMinX ++ " " ++ showF fvbMinY
                    ++ " " ++ showF fvbW ++ " " ++ showF fvbH
      json = jObj
        [ ("flagName", jStr name)
        , ("viewBox", jStr viewBox)
        , ("fillViewBox", jStr fillViewBox)
        , ("initialPoints", Aeson.Array $ fromList
            [ jsonPoint (fst input) "A"
            , jsonPoint (snd input) "B"
            ])
        , ("tree", treeToJson numbered allLayers initialPts
                     liveAfterSets prevOutputsDbl)
        ]

  let isoLower = map toLower isoCode
      path = "out/construction/" ++ isoLower ++ ".json"
  BL.writeFile path (Aeson.encode json)
  putStrLn $ "  Wrote " ++ path

-- ---------------------------------------------------------------------------
-- Aeson helpers
-- ---------------------------------------------------------------------------

jObj :: [(T.Text, Aeson.Value)] -> Aeson.Value
jObj pairs = Aeson.Object $ KM.fromList [(Key.fromText k, v) | (k, v) <- pairs]

jStr :: String -> Aeson.Value
jStr = Aeson.String . T.pack

jNum :: Double -> Aeson.Value
jNum = Aeson.Number . realToFrac

fromList :: [Aeson.Value] -> Aeson.Array
fromList = foldl (\v x -> v <> pure x) mempty

-- ---------------------------------------------------------------------------
-- Tree → JSON
-- ---------------------------------------------------------------------------

treeToJson :: [NumberedEntry] -> [ConstructionLayer] -> [Point]
           -> [Set.Set (Double, Double)]   -- ^ liveAfterSets
           -> [[(Double, Double)]]         -- ^ prevOutputsDbl
           -> Aeson.Value
treeToJson entries _allLayers initialPts liveAfterSets prevOutputsDbl =
    Aeson.Array $ fromList (map entryToJson entries)
  where
    initialDbl = map toDP initialPts

    entryToJson :: NumberedEntry -> Aeson.Value
    entryToJson (NLeaf idx label layer) =
      let layerIdx = idx - 1
          liveSet = if layerIdx + 1 < length liveAfterSets
                      then liveAfterSets !! (layerIdx + 1)
                      else Set.empty
          prevOutDbl = if layerIdx < length prevOutputsDbl
                         then prevOutputsDbl !! layerIdx
                         else []
          curOutputs = layerOutputPoints layer
          curOutDbl = map toDP curOutputs
          allKnownDbl = initialDbl ++ prevOutDbl ++ curOutDbl
          liveDotsDbl = filter (`Set.member` liveSet) allKnownDbl
          dotPtsDbl = dedupDbl (curOutDbl ++ liveDotsDbl)
      in jObj
        [ ("type", jStr "leaf")
        , ("index", jNum (fromIntegral idx))
        , ("label", jStr label)
        , ("geomSvg", jStr (layerGeomSvg layer))
        , ("fillSvg", jStr (layerFillSvg layer))
        , ("dotsSvg", jStr (dotsSvgDbl dotPtsDbl))
        , ("points", Aeson.Array $ fromList (map (\p -> jsonPoint p "") curOutputs))
        , ("inputPoints", Aeson.Array $ fromList (map (\p -> jsonPoint p "") (layerInputPoints layer)))
        ]

    entryToJson (NGroup label children) =
      jObj
        [ ("type", jStr "group")
        , ("label", jStr label)
        , ("children", Aeson.Array $ fromList (map entryToJson children))
        ]

jsonPoint :: Point -> String -> Aeson.Value
jsonPoint (x, y) label = jObj
    [ ("x", jNum (toDouble x))
    , ("y", jNum (toDouble y))
    , ("exactX", jStr (toKaTeX x))
    , ("exactY", jStr (toKaTeX y))
    , ("label", jStr label)
    ]

-- ---------------------------------------------------------------------------
-- Fill bounding box helpers
-- ---------------------------------------------------------------------------

-- | Extract bounding points (as Doubles) from fill layers only.
-- For triangles: the three vertices.
-- For circles: axis-aligned bounding box corners (center ± radius).
fillBoundingPoints :: ConstructionLayer -> [(Double, Double)]
fillBoundingPoints (LayerTriangle _ p1 p2 p3) =
    [toDP p1, toDP p2, toDP p3]
fillBoundingPoints (LayerCircle _ cc ce) =
    let (cx, cy) = toDP cc
        r = toD (pointDist cc ce)
    in [(cx - r, cy - r), (cx + r, cy + r)]
fillBoundingPoints _ = []

-- ---------------------------------------------------------------------------
-- Layer → SVG fragments (manual SVG generation)
-- ---------------------------------------------------------------------------

-- | Generate the construction geometry SVG for a layer
layerGeomSvg :: ConstructionLayer -> String
layerGeomSvg (LayerIntersectLL p1 p2 p3 p4 _) =
    let (x1,y1) = toDP p1; (x2,y2) = toDP p2
        (x3,y3) = toDP p3; (x4,y4) = toDP p4
    in  svgLine x1 y1 x2 y2 ++ "\n" ++ svgLine x3 y3 x4 y4

layerGeomSvg (LayerIntersectLC lp1 lp2 cc ce _) =
    let (x1,y1) = toDP lp1; (x2,y2) = toDP lp2
        (cx,cy) = toDP cc
    in  svgLine x1 y1 x2 y2 ++ "\n" ++ svgCircleOutline cx cy (toD (pointDist cc ce))

layerGeomSvg (LayerIntersectCC c1 e1 c2 e2 _) =
    let (c1x,c1y) = toDP c1; (c2x,c2y) = toDP c2
    in  svgCircleOutline c1x c1y (toD (pointDist c1 e1))
        ++ "\n"
        ++ svgCircleOutline c2x c2y (toD (pointDist c2 e2))

layerGeomSvg (LayerNGonVertex _ _ _) = ""
layerGeomSvg (LayerTriangle _ _ _ _) = ""

layerGeomSvg (LayerCircle _ cc ce) =
    let (cx,cy) = toDP cc
    in  svgCircleOutline cx cy (toD (pointDist cc ce))

-- | Generate the persistent fill SVG for a layer
layerFillSvg :: ConstructionLayer -> String
layerFillSvg (LayerTriangle col p1 p2 p3) =
    let (x1,y1) = toDP p1; (x2,y2) = toDP p2; (x3,y3) = toDP p3
    in  "<polygon points=\"" ++ showF x1 ++ "," ++ showF y1
        ++ " " ++ showF x2 ++ "," ++ showF y2
        ++ " " ++ showF x3 ++ "," ++ showF y3
        ++ "\" fill=\"" ++ colourToHex col
        ++ "\" fill-opacity=\"0.7\" stroke=\"" ++ colourToHex col
        ++ "\" stroke-width=\"0.02\"/>"

layerFillSvg (LayerCircle col cc ce) =
    let (cx,cy) = toDP cc
        r = toD (pointDist cc ce)
    in  "<circle cx=\"" ++ showF cx ++ "\" cy=\"" ++ showF cy
        ++ "\" r=\"" ++ showF r
        ++ "\" fill=\"" ++ colourToHex col
        ++ "\" fill-opacity=\"0.7\" stroke=\"" ++ colourToHex col
        ++ "\" stroke-width=\"0.02\"/>"

layerFillSvg _ = ""

-- | Generate SVG for dot markers from pre-computed (Double, Double) pairs.
dotsSvgDbl :: [(Double, Double)] -> String
dotsSvgDbl pts =
    intercalate "\n"
      [ "<circle cx=\"" ++ showF x ++ "\" cy=\"" ++ showF y
        ++ "\" r=\"0.04\" fill=\"black\" class=\"dot\" data-x=\""
        ++ showF x ++ "\" data-y=\"" ++ showF y ++ "\"/>"
      | (x, y) <- pts
      ]

-- | Deduplicate a list of (Double, Double) pairs preserving order.
dedupDbl :: [(Double, Double)] -> [(Double, Double)]
dedupDbl = go Set.empty
  where
    go _ [] = []
    go seen (p:ps)
      | Set.member p seen = go seen ps
      | otherwise         = p : go (Set.insert p seen) ps

-- ---------------------------------------------------------------------------
-- SVG primitives
-- ---------------------------------------------------------------------------

svgLine :: Double -> Double -> Double -> Double -> String
svgLine x1 y1 x2 y2 =
    "<line x1=\"" ++ showF x1 ++ "\" y1=\"" ++ showF y1
    ++ "\" x2=\"" ++ showF x2 ++ "\" y2=\"" ++ showF y2
    ++ "\" stroke=\"#999\" stroke-width=\"0.02\""
    ++ " stroke-dasharray=\"0.05,0.05\" fill=\"none\"/>"

svgCircleOutline :: Double -> Double -> Double -> String
svgCircleOutline cx cy r =
    "<circle cx=\"" ++ showF cx ++ "\" cy=\"" ++ showF cy
    ++ "\" r=\"" ++ showF r
    ++ "\" stroke=\"#999\" stroke-width=\"0.02\""
    ++ " stroke-dasharray=\"0.08,0.05\" fill=\"none\"/>"

-- ---------------------------------------------------------------------------
-- Colour helpers
-- ---------------------------------------------------------------------------

colourToHex :: Data.Colour.Colour Double -> String
colourToHex col =
    let rgb = toSRGB col
        r = clamp $ round (channelRed rgb * 255) :: Int
        g = clamp $ round (channelGreen rgb * 255) :: Int
        b = clamp $ round (channelBlue rgb * 255) :: Int
    in "#" ++ hexByte r ++ hexByte g ++ hexByte b
  where
    clamp x = max 0 (min 255 x)
    hexByte n
      | n < 16    = "0" ++ showHex n ""
      | otherwise = showHex n ""

-- | Show a Double with enough precision
showF :: Double -> String
showF x = showFFloat (Just 6) x ""

-- ---------------------------------------------------------------------------
-- HTML shell
-- ---------------------------------------------------------------------------

generateHtmlShell :: String
generateHtmlShell = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"UTF-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "<title>Construction Debug</title>"
  , "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css\">"
  , "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js\"></script>"
  , "<style>"
  , "  * { box-sizing: border-box; margin: 0; padding: 0; }"
  , "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;"
  , "         background: #f5f5f5; color: #333; }"
  , "</style>"
  , "</head>"
  , "<body>"
  , "<div id=\"app\"></div>"
  , "<script type=\"module\" src=\"debug-v2.js\"></script>"
  , "</body>"
  , "</html>"
  ]
