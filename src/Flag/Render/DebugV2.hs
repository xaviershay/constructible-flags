{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.DebugV2
  ( writeDebugViewer,
    writeConstructionJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Flag.Construction.FieldNumber (FieldNumber, toDouble)
import Flag.Construction.Layers
  ( ConstructionLayer (..),
    layerInputPoints,
    layerOutputPoints,
    pointDist,
  )
import Flag.Construction.Tree (ConstructionTree (..))
import Flag.Construction.Types (Point)
import Flag.Render.JSONHelpers
  ( fromList,
    jNum,
    jObj,
    jStr,
    jsonPoint,
    layerFillJson,
    layerGeomJson,
  )
import Numeric (showFFloat)
import System.Directory (copyFile, createDirectoryIfMissing)

-- ---------------------------------------------------------------------------
-- Tree-aware step numbering (inlined from Flag.Render.Debug)
-- ---------------------------------------------------------------------------

-- | A numbered entry in the construction tree.
-- Each leaf gets a sequential step number; groups carry their label and children.
data NumberedEntry
  = -- | step number, leaf label, layer
    NLeaf Int String ConstructionLayer
  | -- | group label, sub-entries
    NGroup String [NumberedEntry]
  deriving (Show)

-- | Assign sequential step numbers to the leaves of a 'ConstructionTree',
-- starting from the given counter. Returns the updated counter and the
-- numbered entries.
numberTree :: Int -> [ConstructionTree] -> (Int, [NumberedEntry])
numberTree n [] = (n, [])
numberTree n (TreeLayer l : rest) =
  let label = layerLabel l
      (n', rest') = numberTree (n + 1) rest
   in (n', NLeaf n label l : rest')
numberTree n (TreeGroup g children : rest) =
  let (n', numbered) = numberTree n children
      (n'', rest') = numberTree n' rest
   in (n'', NGroup g numbered : rest')

-- | Collect the leaves of a numbered tree in order.
numberedLeaves :: [NumberedEntry] -> [(Int, ConstructionLayer)]
numberedLeaves [] = []
numberedLeaves (NLeaf i _ l : rest) = (i, l) : numberedLeaves rest
numberedLeaves (NGroup _ cs : rest) = numberedLeaves cs ++ numberedLeaves rest

-- | Human-readable label for a construction layer.
layerLabel :: ConstructionLayer -> String
layerLabel LayerIntersectLL {} = "Intersect line\x2013line"
layerLabel LayerIntersectLC {} = "Intersect line\x2013circle"
layerLabel LayerIntersectCC {} = "Intersect circle\x2013circle"
layerLabel LayerNGonVertex {}  = "N-gon vertex"
layerLabel (LayerTriangle _ _ _ _) = "Fill triangle"
layerLabel (LayerCircle _ _ _)     = "Fill circle"
layerLabel (LayerMasked _ _ _)     = "Masked drawing"
layerLabel (LayerSVGOverlay p _ _) = "SVG overlay: " ++ p
layerLabel (LayerLabel name _)     = "Label: " ++ name

-- ---------------------------------------------------------------------------
-- Point helpers
-- ---------------------------------------------------------------------------

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
