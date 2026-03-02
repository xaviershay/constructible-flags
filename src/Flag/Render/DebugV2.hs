{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.DebugV2
  ( writeDebugViewer,
    writeConstructionJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Colour (Colour)
import Data.Colour.SRGB (channelBlue, channelGreen, channelRed, toSRGB)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Flag.Construction.FieldNumber (FieldNumber, toDouble, toKaTeX)
import Flag.Construction.Layers
  ( ConstructionLayer (..),
    layerInputPoints,
    layerOutputPoints,
    pointDist,
  )
import Flag.Construction.Tree (ConstructionTree)
import Flag.Construction.Types (Point)
import Flag.Render.Debug (NumberedEntry (..), numberTree, numberedLeaves)
import Numeric (showFFloat, showHex)
import System.Directory (copyFile, createDirectoryIfMissing)

-- | Convert a Point (Radical, Radical) to (Double, Double) for SVG rendering.
toDP :: Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

-- | Convert a Number to Double for SVG rendering.
toD :: FieldNumber -> Double
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
-- Takes the flag name, ISO code, initial points, pre-evaluated construction tree,
-- and a list of labelled points collected via 'evalLabels'.
writeConstructionJson :: String -> String -> (Point, Point) -> [ConstructionTree] -> [(Point, String)] -> IO ()
writeConstructionJson name isoCode input tree labelList = do
  createDirectoryIfMissing True "out/construction"

  let labelMap = Map.fromList labelList
      (_, numbered) = numberTree 1 tree
      leaves = numberedLeaves numbered
      allLayers = map snd leaves
      initialPts = [fst input, snd input]

  -- Compute bounding box from all points (convert to Double for SVG viewBox)
  -- Filter non-finite coordinates (NaN/Infinity from degenerate constructions)
  -- to prevent them from poisoning the bounding box.
  let finite x = not (isNaN x || isInfinite x)
      allPoints =
        initialPts
          ++ concatMap layerInputPoints allLayers
          ++ concatMap layerOutputPoints allLayers
      xs = filter finite $ map (toDouble . fst) allPoints
      ys = filter finite $ map (toDouble . snd) allPoints
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
      fxs = filter finite $ map fst fillBounds
      fys = filter finite $ map snd fillBounds
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

  -- Build JSON using Aeson
  let viewBox =
        showF vbMinX
          ++ " "
          ++ showF vbMinY
          ++ " "
          ++ showF vbW
          ++ " "
          ++ showF vbH
      fillViewBox =
        showF fvbMinX
          ++ " "
          ++ showF fvbMinY
          ++ " "
          ++ showF fvbW
          ++ " "
          ++ showF fvbH
      json =
        jObj
          [ ("flagName", jStr name),
            ("viewBox", jStr viewBox),
            ("fillViewBox", jStr fillViewBox),
            ( "initialPoints",
              Aeson.Array $
                fromList
                  [ jsonPoint (fst input) "A",
                    jsonPoint (snd input) "B"
                  ]
            ),
            ("tree", treeToJson labelMap numbered)
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

treeToJson :: Map.Map Point String -> [NumberedEntry] -> Aeson.Value
treeToJson labelMap entries =
  Aeson.Array $ fromList (map entryToJson entries)
  where
    lookupLabel :: Point -> String
    lookupLabel p = Map.findWithDefault "" p labelMap

    entryToJson :: NumberedEntry -> Aeson.Value
    entryToJson (NLeaf idx label layer) =
      let curOutputs = layerOutputPoints layer
       in jObj
            [ ("type", jStr "leaf"),
              ("index", jNum (fromIntegral idx)),
              ("label", jStr label),
              ("geom", layerGeomJson layer),
              ("fill", layerFillJson layer),
              ("points", Aeson.Array $ fromList (map (\p -> jsonPoint p (lookupLabel p)) curOutputs)),
              ("inputPoints", Aeson.Array $ fromList (map (\p -> jsonPoint p (lookupLabel p)) (layerInputPoints layer)))
            ]
    entryToJson (NGroup label children) =
      jObj
        [ ("type", jStr "group"),
          ("label", jStr label),
          ("children", Aeson.Array $ fromList (map entryToJson children))
        ]

jsonPoint :: Point -> String -> Aeson.Value
jsonPoint (x, y) label =
  jObj
    [ ("x", jNum (toDouble x)),
      ("y", jNum (toDouble y)),
      ("exactX", jStr (toKaTeX x)),
      ("exactY", jStr (toKaTeX y)),
      ("label", jStr label)
    ]

-- | Compact [x, y] array for use in geometry instructions.
jsonXY :: Double -> Double -> Aeson.Value
jsonXY x y = Aeson.Array $ fromList [jNum x, jNum y]

-- | A line as [[x1,y1],[x2,y2]].
jsonLine :: Point -> Point -> Aeson.Value
jsonLine p1 p2 =
  let (x1, y1) = toDP p1
      (x2, y2) = toDP p2
   in Aeson.Array $ fromList [jsonXY x1 y1, jsonXY x2 y2]

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
fillBoundingPoints (LayerLabel _ _) = []
fillBoundingPoints _ = []

-- ---------------------------------------------------------------------------
-- Layer → structured geometry JSON
-- ---------------------------------------------------------------------------

-- | Structured description of the construction geometry for a layer.
-- Returns Null for layers with no construction to display.
layerGeomJson :: ConstructionLayer -> Aeson.Value
layerGeomJson (LayerIntersectLL p1 p2 p3 p4 _) =
  jObj
    [ ("type", jStr "intersectLL"),
      ("l1", jsonLine p1 p2),
      ("l2", jsonLine p3 p4)
    ]
layerGeomJson (LayerIntersectLC lp1 lp2 cc ce _) =
  let (cx, cy) = toDP cc
      r = toD (pointDist cc ce)
   in jObj
        [ ("type", jStr "intersectLC"),
          ("line", jsonLine lp1 lp2),
          ("cx", jNum cx),
          ("cy", jNum cy),
          ("r", jNum r)
        ]
layerGeomJson (LayerIntersectCC c1 e1 c2 e2 _) =
  let (c1x, c1y) = toDP c1
      r1 = toD (pointDist c1 e1)
      (c2x, c2y) = toDP c2
      r2 = toD (pointDist c2 e2)
   in jObj
        [ ("type", jStr "intersectCC"),
          ("cx1", jNum c1x),
          ("cy1", jNum c1y),
          ("r1", jNum r1),
          ("cx2", jNum c2x),
          ("cy2", jNum c2y),
          ("r2", jNum r2)
        ]
layerGeomJson (LayerNGonVertex _ _ _) = Aeson.Null
layerGeomJson (LayerTriangle _ _ _ _) = Aeson.Null
layerGeomJson (LayerCircle _ cc ce) =
  let (cx, cy) = toDP cc
      r = toD (pointDist cc ce)
   in jObj
        [ ("type", jStr "circle"),
          ("cx", jNum cx),
          ("cy", jNum cy),
          ("r", jNum r)
        ]
layerGeomJson (LayerMasked _ _ _) = Aeson.Null
layerGeomJson (LayerSVGOverlay _ _ _) = Aeson.Null
layerGeomJson (LayerLabel _ _) = Aeson.Null

-- | Structured description of the persistent fill for a layer.
-- Returns Null for layers that don't produce a filled shape.
layerFillJson :: ConstructionLayer -> Aeson.Value
layerFillJson (LayerTriangle col p1 p2 p3) =
  let (x1, y1) = toDP p1
      (x2, y2) = toDP p2
      (x3, y3) = toDP p3
   in jObj
        [ ("type", jStr "triangle"),
          ( "pts",
            Aeson.Array $
              fromList
                [jsonXY x1 y1, jsonXY x2 y2, jsonXY x3 y3]
          ),
          ("color", jStr (colourToHex col))
        ]
layerFillJson (LayerCircle col cc ce) =
  let (cx, cy) = toDP cc
      r = toD (pointDist cc ce)
   in jObj
        [ ("type", jStr "circle"),
          ("cx", jNum cx),
          ("cy", jNum cy),
          ("r", jNum r),
          ("color", jStr (colourToHex col))
        ]
layerFillJson (LayerLabel _ _) = Aeson.Null
layerFillJson _ = Aeson.Null

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
      | n < 16 = "0" ++ showHex n ""
      | otherwise = showHex n ""

-- | Show a Double with enough precision
showF :: Double -> String
showF x = showFFloat (Just 6) x ""

-- ---------------------------------------------------------------------------
-- HTML shell
-- ---------------------------------------------------------------------------

generateHtmlShell :: String
generateHtmlShell =
  unlines
    [ "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"UTF-8\">",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
      "<title>Construction Debug</title>",
      "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css\">",
      "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js\"></script>",
      "<style>",
      "  * { box-sizing: border-box; margin: 0; padding: 0; }",
      "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;",
      "         background: #f5f5f5; color: #333; }",
      "</style>",
      "</head>",
      "<body>",
      "<div id=\"app\"></div>",
      "<script type=\"module\" src=\"debug-v2.js\"></script>",
      "</body>",
      "</html>"
    ]
