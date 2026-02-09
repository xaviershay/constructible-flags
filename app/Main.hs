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
-- Drawing → Diagram B
-- ---------------------------------------------------------------------------

-- | Convert a 'FlagConstruction.Drawing' to a renderable 'Diagram B'.
drawingToDiagram :: Drawing -> Diagram B
drawingToDiagram EmptyDrawing = mempty
drawingToDiagram (Overlay a b) = drawingToDiagram a <> drawingToDiagram b
drawingToDiagram (DrawTriangle col (x1, y1) (x2, y2) (x3, y3)) =
    let offsets = [ r2 (x2 - x1, y2 - y1), r2 (x3 - x2, y3 - y2) ]
        tri = closeLine (fromOffsets offsets)
    in  strokeLoop tri
          # fcA (col `withOpacity` 1.0)
          # lc col
          # lwG 0.02
          # moveTo (p2 (x1, y1))
drawingToDiagram (DrawPath col pts@((x0, y0):_)) =
    let offsets = zipWith (\(ax, ay) (bx, by) -> r2 (bx - ax, by - ay))
                          pts (tail pts)
        path = closeLine (fromOffsets offsets)
    in  strokeLoop path
          # fcA (col `withOpacity` 1.0)
          # lwG 0
          # moveTo (p2 (x0, y0))
drawingToDiagram (DrawPath _ []) = mempty
drawingToDiagram (DrawCircle col (cx, cy) r) =
    circle r
      # fcA (col `withOpacity` 1.0)
      # lwG 0
      # moveTo (p2 (cx, cy))

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
--main = buildHtml
main = buildDebug
  
  
buildDebug = do
  createDirectoryIfMissing True "out/debug"

  let flagArrow = runPureEff $ runSourcedPure $ flagDesign japan
      input = ((0, 0), (1, 0)) :: (FC.Point, FC.Point)
      (result, tree) = evalTree flagArrow input
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
      step0Dia = renderDots livePts0

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

  -- Compute a uniform bounding box from the union of all diagrams,
  -- so every SVG renders on the same canvas size and position.
  let combined  = mconcat allDias :: Diagram B
      frameDia  = phantom combined :: Diagram B   -- invisible, same envelope

  mapM_ (\(i, dia) -> do
      let path = "out/debug/step-" ++ padNum i ++ ".svg"
      renderSVG path (mkWidth 400) ((dia <> frameDia) # pad 1.3)
      putStrLn $ "  Wrote " ++ path
    ) (zip [0::Int ..] allDias)

  -- Render the final flag using drawingToDiagram
  let finalDia = drawingToDiagram (optimize result)
  renderSVG "out/debug/final.svg" (mkWidth 400) (finalDia)
  putStrLn "  Wrote out/debug/final.svg"

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
  , "<html><head>"
  , "<meta charset=\"UTF-8\">"
  , "<title>Construction Steps</title>"
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
  , "<div style='text-align: center; margin-bottom: 24px;'>"
  , "  <h2>Final Result</h2>"
  , "  <img src=\"final.svg\" width=\"400\" style=\"border: 1px solid #ccc;\">"
  , "</div>"
  , "<hr style='margin-bottom: 24px;'>"
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
layerLabel LayerIntersectLL {}    = "Intersect line–line"
layerLabel LayerIntersectLC {}    = "Intersect line–circle"
layerLabel LayerIntersectCC {}    = "Intersect circle–circle"
layerLabel (LayerTriangle _ _ _ _) = "Fill triangle"
layerLabel (LayerCircle _ _ _)     = "Fill circle"

-- | Render a complete layer (used only by non-debug code paths)
renderLayer :: ConstructionLayer -> Diagram B
renderLayer l = renderConstructionGeom l <> renderFill l

-- | Render the ephemeral construction geometry (dotted lines/circles)
-- and result-point dots for a single step
renderConstructionGeom :: ConstructionLayer -> Diagram B
renderConstructionGeom (LayerIntersectLL (x1, y1) (x2, y2) (x3, y3) (x4, y4) pts) =
    renderLine (x1, y1) (x2, y2)
    <> renderLine (x3, y3) (x4, y4)
    <> renderDots pts

renderConstructionGeom (LayerIntersectLC (x1, y1) (x2, y2) cc ce pts) =
    renderLine (x1, y1) (x2, y2)
    <> renderCircle cc (pointDist cc ce)
    <> renderDots pts

renderConstructionGeom (LayerIntersectCC c1 e1 c2 e2 pts) =
    renderCircle c1 (pointDist c1 e1)
    <> renderCircle c2 (pointDist c2 e2)
    <> renderDots pts

renderConstructionGeom (LayerTriangle _ _ _ _) = mempty
renderConstructionGeom (LayerCircle _ center edge) =
    renderCircle center (pointDist center edge)
    <> renderDots [center, edge]

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
renderFill (LayerCircle col (cx, cy) (ex, ey)) =
    let r = pointDist (cx, cy) (ex, ey)
    in  circle r
          # fcA (col `withOpacity` 0.6)
          # lc col
          # lwG 0.02
          # moveTo (p2 (cx, cy))
renderFill _ = mempty

-- | Render a dotted construction line connecting two points
renderLine :: (Double, Double) -> (Double, Double) -> Diagram B
renderLine (x1, y1) (x2, y2) =
    fromVertices [p2 (x1, y1), p2 (x2, y2)]
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

buildHtml :: IO ()
buildHtml = do
  createDirectoryIfMissing True "out"
  
  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag allCountryFlags
  
  -- Generate index.html
  let html = generateIndex flagData
  writeFile "out/index.html" html
  
  putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s) and index.html"

-- | Process a single flag: render SVG and extract metadata
processFlag :: Flag (Sourced : '[]) -> IO (String, String, String, String, [SourcedElement], [Step])
processFlag flag = do
  let isoLower = map toLower (flagIsoCode flag)
      svgFile = isoLower ++ ".svg"
      svgPath = "out/" ++ svgFile
      
  -- Resolve the FlagA arrow (sources colours etc.)
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
  
  -- Evaluate the arrow on a unit input to get the Drawing
  let flagInput = ((0, 0), (1, 0)) :: (FC.Point, FC.Point)
      drawing = eval flagArrow flagInput
      diagram = drawingToDiagram (optimize drawing)
  renderSVG svgPath (mkWidth 300) diagram
  
  -- Get description
  let description = runPureEff $ runSourcedPure $ flagDescription flag
  
  -- Collect sources from design and description
  let (_, designSources) = runPureEff $ runSourcedCollect $ flagDesign flag
  let (_, descSources) = runPureEff $ runSourcedCollect $ flagDescription flag
  let allSources = nub (designSources ++ descSources)
  
  -- Extract construction steps from the FlagA arrow
  let constructionSteps = steps flagArrow
  
  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"
  
  pure (svgFile, flagName flag, description, flagIsoCode flag, allSources, constructionSteps)

-- | Generate the index.html content
generateIndex :: [(String, String, String, String, [SourcedElement], [Step])] -> String
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
    flagRow (svgFile, name, desc, _, sources, constructionSteps) = unlines
      [ "      <tr>"
      , "        <td><a href=\"" ++ svgFile ++ "\"><img src=\"" ++ svgFile ++ "\" alt=\"" ++ escapeHtml name ++ " flag\"></a></td>"
      , "        <td>" ++ escapeHtml name ++ "</td>"
      , "        <td>" ++ escapeHtml desc ++ "</td>"
      , "        <td>" ++ formatSteps constructionSteps ++ "</td>"
      , "        <td>" ++ formatSources sources ++ "</td>"
      , "      </tr>"
      ]
    
    -- Format construction steps grouped by kind
    formatSteps :: [Step] -> String
    formatSteps [] = "<em>None</em>"
    formatSteps ss =
      let llCount = length [() | StepIntersectLL <- ss]
          lcCount = length [() | StepIntersectLC <- ss]
          ccCount = length [() | StepIntersectCC <- ss]
          ftCount = length [() | StepFillTriangle <- ss]
          fcCount = length [() | StepFillCircle <- ss]
          items = concat
            [ if llCount > 0 then ["<li>Intersect line\8211line \215" ++ show llCount ++ "</li>"] else []
            , if lcCount > 0 then ["<li>Intersect line\8211circle \215" ++ show lcCount ++ "</li>"] else []
            , if ccCount > 0 then ["<li>Intersect circle\8211circle \215" ++ show ccCount ++ "</li>"] else []
            , if ftCount > 0 then ["<li>Fill triangle \215" ++ show ftCount ++ "</li>"] else []
            , if fcCount > 0 then ["<li>Fill circle \215" ++ show fcCount ++ "</li>"] else []
            ]
      in if null items
         then "<em>None</em>"
         else "<ul>" ++ concat items ++ "</ul>"
    
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
    formatSourceGroup grp@((_, src):_) = 
      let elementNames = map fst grp
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
