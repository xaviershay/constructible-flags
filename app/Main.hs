{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Lib
import FlagConstruction hiding (Point)
import qualified FlagConstruction as FC
import Diagrams.Prelude hiding (trace, radius)
import Diagrams.Backend.SVG
import Effectful
import System.Directory (createDirectoryIfMissing)
import Data.Char (toLower)
import Data.List (nub, sortOn, groupBy, intercalate)
import Data.Function (on)

main :: IO ()
main = do
  createDirectoryIfMissing True "out/debug"

  let input = ((0, 0), (1, 0)) :: (FC.Point, FC.Point)
      (result, layers) = evalLayers exampleDesign input
      stepNames = steps exampleDesign

  putStrLn $ "Construction has " ++ show (length stepNames) ++ " steps, " ++ show (length layers) ++ " layers:"
  mapM_ (\(i, s) -> putStrLn $ "  " ++ show i ++ ". " ++ show s) (zip [1::Int ..] stepNames)

  -- Render cumulative SVGs: step 0 is the initial points, then one per layer
  let initialDia = renderInitialPoints input
      cumulativeDias = scanl (\acc layer -> acc <> renderLayer layer) initialDia layers

  mapM_ (\(i, dia) -> do
      let path = "out/debug/step-" ++ padNum i ++ ".svg"
      renderSVG path (mkWidth 400) (dia # pad 1.3)
      putStrLn $ "  Wrote " ++ path
    ) (zip [0::Int ..] cumulativeDias)

  -- Generate index.html
  let numSteps = length cumulativeDias
      layerLabels = map layerLabel layers
      indexHtml = unlines
        [ "<!DOCTYPE html>"
        , "<html><head><title>Construction Steps</title>"
        , "<style>"
        , "  body { font-family: sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }"
        , "  .step { display: inline-block; margin: 10px; text-align: center; }"
        , "  .step img { border: 1px solid #ccc; }"
        , "  .step p { margin: 4px 0; font-size: 14px; color: #666; }"
        , "</style></head><body>"
        , "<h1>Construction Steps</h1>"
        , unlines [ "<div class=\"step\"><img src=\"step-" ++ padNum i ++ ".svg\" width=\"300\"><p>"
                    ++ caption i ++ "</p></div>"
                  | i <- [0 .. numSteps - 1] ]
        , "</body></html>"
        ]
      caption 0 = "Initial points"
      caption i = show i ++ ". " ++ (layerLabels !! (i - 1))
  writeFile "out/debug/index.html" indexHtml
  putStrLn "  Wrote out/debug/index.html"

  putStrLn $ "\nResult: " ++ show result

-- | Zero-pad a step number to 2 digits
padNum :: Int -> String
padNum n
  | n < 10    = "0" ++ show n
  | otherwise = show n

-- | Human-readable label for a construction layer
layerLabel :: ConstructionLayer -> String
layerLabel LayerIntersectLC {}    = "Intersect line-circle"
layerLabel LayerIntersectCC {}    = "Intersect circle-circle"
layerLabel (LayerTriangle _ _ _ _) = "Fill triangle"

-- | Render the two initial points as labeled black dots
renderInitialPoints :: (FC.Point, FC.Point) -> Diagram B
renderInitialPoints ((ax, ay), (bx, by)) =
    renderDot "a" (ax, ay) <> renderDot "b" (bx, by)
  where
    renderDot label (x, y) =
      (  circle 0.04 # fc black # lw none
      <> text label # fontSizeL 0.12 # fc black # translate (r2 (0, -0.12))
      ) # moveTo (p2 (x, y))

-- | Render a single construction layer as a diagram
renderLayer :: ConstructionLayer -> Diagram B
renderLayer (LayerIntersectLC (x1, y1) (x2, y2) (cx, cy) r pts) =
    renderLine (x1, y1) (x2, y2)
    <> renderCircle (cx, cy) r
    <> renderDots pts

renderLayer (LayerIntersectCC (c1x, c1y) r1 (c2x, c2y) r2 pts) =
    renderCircle (c1x, c1y) r1
    <> renderCircle (c2x, c2y) r2
    <> renderDots pts

renderLayer (LayerTriangle col (x1, y1) (x2, y2) (x3, y3)) =
    let offsets = [ r2 (x2-x1, y2-y1), r2 (x3-x2, y3-y2) ]
        tri = closeLine (fromOffsets offsets)
    in  strokeLoop tri
          # fcA (col `withOpacity` 0.6)
          # lc col
          # lwG 0.02
          # moveTo (p2 (x1, y1))

-- | Render a dotted construction line, extended beyond the defining points
renderLine :: (Double, Double) -> (Double, Double) -> Diagram B
renderLine (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
        ext = 0.5
        xa = x1 - ext * dx
        ya = y1 - ext * dy
        xb = x2 + ext * dx
        yb = y2 + ext * dy
    in fromVertices [p2 (xa, ya), p2 (xb, yb)]
         # dashingG [0.05, 0.05] 0
         # lc grey
         # lwG 0.02

-- | Render a dotted construction circle
renderCircle :: (Double, Double) -> Double -> Diagram B
renderCircle (cx, cy) r =
    circle r
      # moveTo (p2 (cx, cy))
      # dashingG [0.08, 0.05] 0
      # lc grey
      # lwG 0.02
      # fillColor transparent

-- | Render intersection points as black dots
renderDots :: [(Double, Double)] -> Diagram B
renderDots pts =
    mconcat [ circle 0.04 # fc black # lw none # moveTo (p2 (x, y))
            | (x, y) <- pts
            ]
  
buildHtml = do
  createDirectoryIfMissing True "out"
  
  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag allCountryFlags
  
  -- Generate index.html
  let html = generateIndex flagData
  writeFile "out/index.html" html
  
  putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s) and index.html"

-- | Process a single flag: render SVG and extract metadata
processFlag :: Flag (Sourced : Construction : '[]) -> IO (String, String, String, String, [SourcedElement], [ConstructionElement])
processFlag flag = do
  let isoLower = map toLower (flagIsoCode flag)
      svgFile = isoLower ++ ".svg"
      svgPath = "out/" ++ svgFile
      
  -- Render the SVG (run Sourced first since it's outer in the effect stack)
  let diagram = runPureEff $ runConstructionSVG $ runSourcedPure $ flagDesign flag
  renderSVG svgPath (mkWidth 300) diagram
  
  -- Get description (use runConstructionPure for non-Diagram results)
  let description = runPureEff $ runConstructionPure $ runSourcedPure $ flagDescription flag
  
  -- Collect sources from design and description
  let (_, designSources) = runPureEff $ runConstructionPure $ runSourcedCollect $ flagDesign flag
  let (_, descSources) = runPureEff $ runConstructionPure $ runSourcedCollect $ flagDescription flag
  let allSources = nub (designSources ++ descSources)
  
  -- Collect construction operations
  let (_, constructions) = runPureEff $ runConstructionCollect $ runSourcedPure $ flagDesign flag
  
  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"
  
  pure (svgFile, flagName flag, description, flagIsoCode flag, allSources, constructions)

-- | Generate the index.html content
generateIndex :: [(String, String, String, String, [SourcedElement], [ConstructionElement])] -> String
generateIndex flags = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>Constructible Flags</title>"
  , "  <style>"
  , "    body { font-family: sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }"
  , "    table { border-collapse: collapse; width: 100%; }"
  , "    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; vertical-align: top; }"
  , "    th { background-color: #f4f4f4; }"
  , "    img { max-width: 150px; height: auto; }"
  , "    ul { margin: 0; padding-left: 20px; }"
  , "    .elements { font-size: 0.9em; color: #666; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <h1>Constructible Flags</h1>"
  , "  <table>"
  , "    <thead>"
  , "      <tr>"
  , "        <th>Design</th>"
  , "        <th>Name</th>"
  , "        <th>Description</th>"
  , "        <th>Construction</th>"
  , "        <th>Sources</th>"
  , "      </tr>"
  , "    </thead>"
  , "    <tbody>"
  , concatMap flagRow flags
  , "    </tbody>"
  , "  </table>"
  , "</body>"
  , "</html>"
  ]
  where
    flagRow (svgFile, name, desc, _, sources, constructions) = unlines
      [ "      <tr>"
      , "        <td><a href=\"" ++ svgFile ++ "\"><img src=\"" ++ svgFile ++ "\" alt=\"" ++ escapeHtml name ++ " flag\"></a></td>"
      , "        <td>" ++ escapeHtml name ++ "</td>"
      , "        <td>" ++ escapeHtml desc ++ "</td>"
      , "        <td>" ++ formatConstructions constructions ++ "</td>"
      , "        <td>" ++ formatSources sources ++ "</td>"
      , "      </tr>"
      ]
    
    -- Format construction operations grouped by technique
    formatConstructions :: [ConstructionElement] -> String
    formatConstructions [] = "<em>None</em>"
    formatConstructions elems =
      let naturals = [n | ConstructionNatural n <- elems]
          rationals = [(num, denom) | ConstructionRational num denom <- elems]
          boxCenters = [dims | ConstructionBoxCenter dims <- elems]
          items = concat
            [ if null naturals then [] else [formatNaturals naturals]
            , if null rationals then [] else [formatRationals rationals]
            , if null boxCenters then [] else [formatBoxCenters boxCenters]
            ]
      in if null items
         then "<em>None</em>"
         else "<ul>" ++ concat items ++ "</ul>"
    
    formatNaturals :: [Int] -> String
    formatNaturals ns = 
      let unique = nub ns
      in "<li>Natural <span class=\"elements\">(" ++ intercalate ", " (map show unique) ++ ")</span></li>"
    
    formatRationals :: [(Int, Int)] -> String
    formatRationals rs = 
      let unique = nub rs
          formatRational (num, denom) = show num ++ "/" ++ show denom
      in "<li>Rational <span class=\"elements\">(" ++ intercalate ", " (map formatRational unique) ++ ")</span></li>"
    
    formatBoxCenters :: [(Double, Double)] -> String
    formatBoxCenters bs = 
      let unique = nub bs
          formatDouble :: Double -> String
          formatDouble x = 
            let rounded = fromIntegral (round x :: Int)
            in if abs (x - rounded) < 1e-10
               then show (round x :: Int)
               else show x
          formatBox (w, h) = "(" ++ formatDouble w ++ ", " ++ formatDouble h ++ ")"
      in "<li>Box Center <span class=\"elements\">(" ++ intercalate ", " (map formatBox unique) ++ ")</span></li>"
    
    -- Group elements by source and format
    formatSources :: [SourcedElement] -> String
    formatSources [] = "<em>None</em>"
    formatSources elements = 
      let grouped = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) elements
      in "<ul>" ++ concatMap formatSourceGroup grouped ++ "</ul>"
    
    -- Key for sorting sources (to group same sources together)
    sourceKey :: Source -> String
    sourceKey SourceHabitual = "0"
    sourceKey (SourceLaw title _) = "1" ++ title
    sourceKey (SourceAuthoritativeWebsite title _) = "2" ++ title
    sourceKey (SourcePublication title _) = "3" ++ title
    
    formatSourceGroup :: [SourcedElement] -> String
    formatSourceGroup [] = ""
    formatSourceGroup group@((_, src):_) = 
      let elementNames = map fst group
          elementsStr = "<span class=\"elements\">(" ++ escapeHtml (joinElements elementNames) ++ ")</span>"
      in formatSourceWithElements src elementsStr
    
    joinElements :: [String] -> String
    joinElements xs = intercalate ", " xs
    
    formatSourceWithElements :: Source -> String -> String
    formatSourceWithElements SourceHabitual elems = 
      "<li>Habitual practice " ++ elems ++ "</li>"
    formatSourceWithElements (SourceAuthoritativeWebsite title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> " ++ elems ++ "</li>"
    formatSourceWithElements (SourceLaw title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> (Law) " ++ elems ++ "</li>"
    formatSourceWithElements (SourcePublication title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> (Publication) " ++ elems ++ "</li>"
    
    escapeHtml :: String -> String
    escapeHtml = concatMap escapeChar
      where
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar '"' = "&quot;"
        escapeChar c   = [c]
