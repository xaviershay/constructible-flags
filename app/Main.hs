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

-- ---------------------------------------------------------------------------
-- Tree-aware step numbering
-- ---------------------------------------------------------------------------

-- | A numbered entry in the construction tree.
-- Each leaf gets a sequential step number; groups carry their label and children.
data NumberedEntry
  = NLeaf Int String ConstructionLayer   -- ^ step number, leaf label, layer
  | NGroup String [NumberedEntry]        -- ^ group label, sub-entries
  deriving (Show)

-- | Assign sequential step numbers to the leaves of a 'ConstructionTree',
-- starting from the given counter. Returns the updated counter and the
-- numbered entries.
numberTree :: Int -> [ConstructionTree] -> (Int, [NumberedEntry])
numberTree n [] = (n, [])
numberTree n (TreeLayer l : rest) =
    let label = layerLabel l
        (n', rest') = numberTree (n + 1) rest
    in  (n', NLeaf n label l : rest')
numberTree n (TreeGroup g children : rest) =
    let (n',  numbered)  = numberTree n  children
        (n'', rest')     = numberTree n' rest
    in  (n'', NGroup g numbered : rest')

-- | Collect the leaves of a numbered tree in order.
numberedLeaves :: [NumberedEntry] -> [(Int, ConstructionLayer)]
numberedLeaves [] = []
numberedLeaves (NLeaf i _ l : rest) = (i, l) : numberedLeaves rest
numberedLeaves (NGroup _ cs : rest) = numberedLeaves cs ++ numberedLeaves rest

main :: IO ()
main = do
  createDirectoryIfMissing True "out/debug"

  let input = ((0, 0), (1, 0)) :: (FC.Point, FC.Point)
      (result, tree) = evalTree exampleDesign input
      (_, numbered)  = numberTree 1 tree
      leaves         = numberedLeaves numbered
      allLayers      = map snd leaves
      initialPts     = [fst input, snd input]

  putStrLn $ "Construction has " ++ show (length leaves) ++ " steps:"
  printNumberedTree 0 numbered

  -- Compute the set of points that are consumed by step i or later.
  -- liveAfter !! i  =  inputs of steps i .. n-1
  let n = length allLayers
      liveAfter = [ nub $ concatMap layerInputPoints (drop i allLayers)
                  | i <- [0 .. n] ]
      -- liveAfter has n+1 entries: index 0 = all inputs, index n = empty

  -- Render step 0: initial points, but only those that are live
  let livePts0 = filter (`elem` (liveAfter !! 0)) initialPts
      step0Dia = renderLabelledDots (zip ["a","b"] initialPts) livePts0

  -- Render steps 1..n
  let stepDias = [ let layerIdx    = i - 1
                       layer       = allLayers !! layerIdx
                       live        = liveAfter !! i  -- points consumed from step i onwards
                       -- All points known so far: initial + outputs of steps 0..layerIdx
                       prevOutputs = concatMap layerOutputPoints (take layerIdx allLayers)
                       curOutputs  = layerOutputPoints layer
                       allKnown    = initialPts ++ prevOutputs ++ curOutputs
                       -- Show dots for known points that are still live
                       liveDots    = renderDots (filter (`elem` live) allKnown)
                       -- Construction geometry: only for this step
                       geom        = renderConstructionGeom layer
                       -- Fills are cumulative
                       fills       = mconcat [ renderFill l | l <- take i allLayers ]
                   in  liveDots <> geom <> fills
                 | i <- [1 .. n]
                 ]

  let allDias = step0Dia : stepDias

  mapM_ (\(i, dia) -> do
      let path = "out/debug/step-" ++ padNum i ++ ".svg"
      renderSVG path (mkWidth 400) (dia # pad 1.3)
      putStrLn $ "  Wrote " ++ path
    ) (zip [0::Int ..] allDias)

  -- Generate index.html reflecting the tree structure
  let indexHtml = generateDebugIndex numbered (length allDias)
  writeFile "out/debug/index.html" indexHtml
  putStrLn "  Wrote out/debug/index.html"

  putStrLn $ "\nResult: " ++ show result

-- | Print the numbered tree to stdout with indentation
printNumberedTree :: Int -> [NumberedEntry] -> IO ()
printNumberedTree _ [] = return ()
printNumberedTree depth (NLeaf i label _ : rest) = do
  putStrLn $ replicate (depth * 2) ' ' ++ show i ++ ". " ++ label
  printNumberedTree depth rest
printNumberedTree depth (NGroup g cs : rest) = do
  putStrLn $ replicate (depth * 2) ' ' ++ "[" ++ g ++ "]"
  printNumberedTree (depth + 1) cs
  printNumberedTree depth rest

-- | Generate index.html with group headings and nested step images
generateDebugIndex :: [NumberedEntry] -> Int -> String
generateDebugIndex entries _ = unlines
  [ "<!DOCTYPE html>"
  , "<html><head><title>Construction Steps</title>"
  , "<style>"
  , "  body { font-family: sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }"
  , "  .step { display: inline-block; margin: 10px; text-align: center; vertical-align: top; }"
  , "  .step img { border: 1px solid #ccc; }"
  , "  .step p { margin: 4px 0; font-size: 14px; color: #666; }"
  , "  .group { margin: 16px 0 8px 0; padding: 12px; border-left: 3px solid #4a90d9; background: #f7f9fc; }"
  , "  .group > h3 { margin: 0 0 8px 0; color: #4a90d9; font-size: 16px; }"
  , "  .group .group { border-left-color: #7ab648; }"
  , "  .group .group > h3 { color: #7ab648; }"
  , "</style></head><body>"
  , "<h1>Construction Steps</h1>"
  , "<div class=\"step\"><img src=\"step-00.svg\" width=\"300\"><p>Initial points</p></div>"
  , renderEntries entries
  , "</body></html>"
  ]

-- | Render the numbered tree entries as HTML
renderEntries :: [NumberedEntry] -> String
renderEntries = concatMap renderEntry

renderEntry :: NumberedEntry -> String
renderEntry (NLeaf i label _) = unlines
  [ "<div class=\"step\">"
  , "  <img src=\"step-" ++ padNum i ++ ".svg\" width=\"300\">"
  , "  <p>" ++ show i ++ ". " ++ escapeHtml label ++ "</p>"
  , "</div>"
  ]
renderEntry (NGroup g cs) = unlines
  [ "<div class=\"group\">"
  , "  <h3>" ++ escapeHtml g ++ "</h3>"
  , renderEntries cs
  , "</div>"
  ]

-- | Zero-pad a step number to 2 digits
padNum :: Int -> String
padNum n
  | n < 10    = "0" ++ show n
  | otherwise = show n

-- | Human-readable label for a construction layer
layerLabel :: ConstructionLayer -> String
layerLabel LayerIntersectLC {}    = "Intersect line–circle"
layerLabel LayerIntersectCC {}    = "Intersect circle–circle"
layerLabel (LayerTriangle _ _ _ _) = "Fill triangle"

-- | Render the two initial points as labeled black dots
renderInitialPoints :: (FC.Point, FC.Point) -> Diagram B
renderInitialPoints ((ax, ay), (bx, by)) =
    renderLabelledDots [("a", (ax, ay)), ("b", (bx, by))] [(ax, ay), (bx, by)]

-- | Render labelled dots, but only for points in the live set
renderLabelledDots :: [(String, FC.Point)] -> [FC.Point] -> Diagram B
renderLabelledDots labelled live =
    mconcat [ renderDot label pt | (label, pt) <- labelled, pt `elem` live ]
  where
    renderDot label (x, y) =
      (  circle 0.04 # fc black # lw none
      <> text label # fontSizeL 0.12 # fc black # translate (r2 (0, -0.12))
      ) # moveTo (p2 (x, y))

-- | Render a complete layer (used only by non-debug code paths)
renderLayer :: ConstructionLayer -> Diagram B
renderLayer l = renderConstructionGeom l <> renderFill l

-- | Render the ephemeral construction geometry (dotted lines/circles)
-- and result-point dots for a single step
renderConstructionGeom :: ConstructionLayer -> Diagram B
renderConstructionGeom (LayerIntersectLC (x1, y1) (x2, y2) cc ce pts) =
    renderLine (x1, y1) (x2, y2)
    <> renderCircle cc (pointDist cc ce)
    <> renderDots pts

renderConstructionGeom (LayerIntersectCC c1 e1 c2 e2 pts) =
    renderCircle c1 (pointDist c1 e1)
    <> renderCircle c2 (pointDist c2 e2)
    <> renderDots pts

renderConstructionGeom (LayerTriangle _ _ _ _) = mempty

-- | Render the persistent fill for a layer (only triangles produce fills)
renderFill :: ConstructionLayer -> Diagram B
renderFill (LayerTriangle col (x1, y1) (x2, y2) (x3, y3)) =
    let offsets = [ r2 (x2-x1, y2-y1), r2 (x3-x2, y3-y2) ]
        tri = closeLine (fromOffsets offsets)
    in  strokeLoop tri
          # fcA (col `withOpacity` 0.6)
          # lc col
          # lwG 0.02
          # moveTo (p2 (x1, y1))
renderFill _ = mempty

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

-- | Escape special HTML characters
escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar c   = [c]

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
