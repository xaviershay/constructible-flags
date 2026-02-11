{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Render.DebugV2
    ( writeDebugViewer
    , writeConstructionJson
    ) where

import Data.Char (toLower)
import Data.List (nub, intercalate)
import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB, channelRed, channelGreen, channelBlue)
import Effectful (runPureEff)
import Numeric (showFFloat, showHex)
import System.Directory (createDirectoryIfMissing, copyFile)

import Flag.Construction.Radical (Radical, toDouble, toKaTeX)
import Flag.Construction.Types (Point)
import Flag.Construction.Layers (ConstructionLayer(..), layerInputPoints,
                                 layerOutputPoints, pointDist)
import Flag.Construction.Tree (evalTree)
import Flag.Source (Sourced, runSourcedPure)
import Flag.Definition (Flag(..))
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
writeConstructionJson :: Flag (Sourced : '[]) -> IO ()
writeConstructionJson flag = do
  createDirectoryIfMissing True "out/construction"

  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      input = ((0, 0), (1, 0)) :: (Point, Point)
      (_, tree) = evalTree flagArrow input
      (_, numbered) = numberTree 1 tree
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

  -- Compute live-after sets for dot rendering
  let nSteps = length allLayers
      liveAfter = [ nub $ concatMap layerInputPoints (drop i allLayers)
                  | i <- [0 .. nSteps] ]

  -- Build JSON
  let viewBox = showF vbMinX ++ " " ++ showF vbMinY
                ++ " " ++ showF vbW ++ " " ++ showF vbH
      json = jsonObj
        [ ("flagName", jsonStr (flagName flag))
        , ("viewBox", jsonStr viewBox)
        , ("initialPoints", jsonArr
            [ jsonPoint (fst input) "A"
            , jsonPoint (snd input) "B"
            ])
        , ("tree", treeToJson numbered allLayers initialPts liveAfter)
        ]

  let isoLower = map toLower (flagIsoCode flag)
      path = "out/construction/" ++ isoLower ++ ".json"
  writeFile path json
  putStrLn $ "  Wrote " ++ path

-- ---------------------------------------------------------------------------
-- Tree → JSON
-- ---------------------------------------------------------------------------

treeToJson :: [NumberedEntry] -> [ConstructionLayer] -> [Point] -> [[Point]] -> String
treeToJson entries allLayers initialPts liveAfter =
    jsonArr (map entryToJson entries)
  where
    entryToJson :: NumberedEntry -> String
    entryToJson (NLeaf idx label layer) =
      let layerIdx = idx - 1
          live = if layerIdx + 1 < length liveAfter
                   then liveAfter !! (layerIdx + 1)
                   else []
          prevOutputs = concatMap layerOutputPoints (take layerIdx allLayers)
          curOutputs = layerOutputPoints layer
          allKnown = initialPts ++ prevOutputs ++ curOutputs
          liveDots = filter (`elem` live) allKnown
      in jsonObj
        [ ("type", jsonStr "leaf")
        , ("index", show idx)
        , ("label", jsonStr label)
        , ("geomSvg", jsonStr (layerGeomSvg layer))
        , ("fillSvg", jsonStr (layerFillSvg layer))
        , ("dotsSvg", jsonStr (dotsSvg (curOutputs ++ liveDots)))
        , ("points", jsonArr (map (\p -> jsonPoint p "") curOutputs))
        , ("inputPoints", jsonArr (map (\p -> jsonPoint p "") (layerInputPoints layer)))
        ]

    entryToJson (NGroup label children) =
      jsonObj
        [ ("type", jsonStr "group")
        , ("label", jsonStr label)
        , ("children", jsonArr (map entryToJson children))
        ]

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

-- | Generate SVG for dot markers
dotsSvg :: [Point] -> String
dotsSvg pts =
    intercalate "\n"
      [ let (x, y) = toDP p
        in  "<circle cx=\"" ++ showF x ++ "\" cy=\"" ++ showF y
            ++ "\" r=\"0.04\" fill=\"black\" class=\"dot\" data-x=\""
            ++ showF x ++ "\" data-y=\"" ++ showF y ++ "\"/>"
      | p <- nub pts
      ]

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

-- ---------------------------------------------------------------------------
-- JSON helpers (hand-rolled, no aeson dependency)
-- ---------------------------------------------------------------------------

jsonObj :: [(String, String)] -> String
jsonObj pairs = "{" ++ intercalate "," (map (\(k, v) -> jsonStr k ++ ":" ++ v) pairs) ++ "}"

jsonArr :: [String] -> String
jsonArr items = "[" ++ intercalate "," items ++ "]"

jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap escapeJsonChar s ++ "\""

escapeJsonChar :: Char -> String
escapeJsonChar '"'  = "\\\""
escapeJsonChar '\\' = "\\\\"
escapeJsonChar '\n' = "\\n"
escapeJsonChar '\r' = "\\r"
escapeJsonChar '\t' = "\\t"
escapeJsonChar c
  | c < ' '   = "\\u" ++ padHex 4 (fromEnum c)
  | otherwise  = [c]

padHex :: Int -> Int -> String
padHex width n =
    let h = showHex n ""
    in replicate (width - length h) '0' ++ h

jsonPoint :: Point -> String -> String
jsonPoint (x, y) label = jsonObj
    [ ("x", showF (toDouble x))
    , ("y", showF (toDouble y))
    , ("exactX", jsonStr (toKaTeX x))
    , ("exactY", jsonStr (toKaTeX y))
    , ("label", jsonStr label)
    ]

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
