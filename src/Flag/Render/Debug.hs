{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Render.Debug
    ( NumberedEntry(..)
    , numberTree
    , numberedLeaves
    , printNumberedTree
    , layerLabel
    , padNum
    , buildDebug
    ) where

import Data.List (nub)
import Diagrams.Prelude (Diagram, pad, phantom, (#), mkWidth)
import Diagrams.Backend.SVG (B, renderSVG)
import Effectful (runPureEff)
import System.Directory (createDirectoryIfMissing)

import Flag.Construction.Types (Point)
import Flag.Construction.Layers (ConstructionLayer(..), layerInputPoints, layerOutputPoints)
import Flag.Construction.Tree (ConstructionTree(..), evalTree)
import Flag.Construction.Optimize (optimize)
import Flag.Construction.Radical (toDouble)
import Flag.Source (Sourced, runSourcedPure)
import Flag.Definition (Flag(..))
import Flag.Render.Diagram (drawingToDiagram, renderConstructionGeom, renderFill, renderDots)

-- | Convert a Point (Radical, Radical) to (Double, Double) for diagrams.
toDP :: Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

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

-- | Human-readable label for a construction layer
layerLabel :: ConstructionLayer -> String
layerLabel LayerIntersectLL {}     = "Intersect line–line"
layerLabel LayerIntersectLC {}     = "Intersect line–circle"
layerLabel LayerIntersectCC {}     = "Intersect circle–circle"
layerLabel LayerNGonVertex {}      = "N-gon vertex"
layerLabel (LayerTriangle _ _ _ _) = "Fill triangle"
layerLabel (LayerCircle _ _ _)     = "Fill circle"
layerLabel (LayerSVGOverlay p _ _) = "SVG overlay: " ++ p

-- | Zero-pad a step number to 2 digits
padNum :: Int -> String
padNum n
  | n < 10    = "0" ++ show n
  | otherwise = show n

-- ---------------------------------------------------------------------------
-- Build debug output
-- ---------------------------------------------------------------------------

-- | Build the debug visualisation for a single flag.
buildDebug :: Flag (Sourced : '[]) -> IO ()
buildDebug flag = do
  createDirectoryIfMissing True "out/debug"

  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      input = ((0, 0), (1, 0)) :: (Point, Point)
      (result, tree) = evalTree flagArrow input
      (_, numbered)  = numberTree 1 tree
      leaves         = numberedLeaves numbered
      allLayers      = map snd leaves
      initialPts     = [fst input, snd input]

  putStrLn $ "Construction has " ++ show (length leaves) ++ " steps:"
  printNumberedTree 0 numbered

  -- Compute the set of points that are consumed by step i or later.
  -- liveAfter !! i  =  inputs of steps i .. n-1
  let nSteps = length allLayers
      liveAfter = [ nub $ concatMap layerInputPoints (drop i allLayers)
                  | i <- [0 .. nSteps] ]

  -- Render step 0: initial points, but only those that are live
  let livePts0 = filter (`elem` (liveAfter !! 0)) initialPts
      step0Dia = renderDots (map toDP livePts0)

  -- Render steps 1..n
  let stepDias = [ let layerIdx    = i - 1
                       layer       = allLayers !! layerIdx
                       live        = liveAfter !! i
                       prevOutputs = concatMap layerOutputPoints (take layerIdx allLayers)
                       curOutputs  = layerOutputPoints layer
                       allKnown    = initialPts ++ prevOutputs ++ curOutputs
                       liveDots    = renderDots (map toDP (filter (`elem` live) allKnown))
                       geom        = renderConstructionGeom layer
                       fills       = mconcat [ renderFill l | l <- take i allLayers ]
                   in  liveDots <> geom <> fills
                 | i <- [1 .. nSteps]
                 ]

  let allDias = step0Dia : stepDias

  -- Compute a uniform bounding box from the union of all diagrams,
  -- so every SVG renders on the same canvas size and position.
  let combined  = mconcat allDias :: Diagram B
      frameDia  = phantom combined :: Diagram B

  mapM_ (\(i, dia) -> do
      let path = "out/debug/step-" ++ padNum i ++ ".svg"
      renderSVG path (mkWidth 400) ((dia <> frameDia) # pad 1.3)
      putStrLn $ "  Wrote " ++ path
    ) (zip [0::Int ..] allDias)

  -- Render the final flag using drawingToDiagram
  let finalDia = drawingToDiagram (optimize result)
  renderSVG "out/debug/final.svg" (mkWidth 400) finalDia
  putStrLn "  Wrote out/debug/final.svg"

  -- Generate index.html reflecting the tree structure
  let indexHtml = generateDebugIndexHtml numbered (length allDias)
  writeFile "out/debug/index.html" indexHtml
  putStrLn "  Wrote out/debug/index.html"

  putStrLn $ "\nResult: " ++ show result

-- | Internal: generate the HTML (calls into Flag.Render.Html, but we
-- inline it here to avoid a circular dependency since Html imports us).
generateDebugIndexHtml :: [NumberedEntry] -> Int -> String
generateDebugIndexHtml entries _ = unlines
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
  where
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

    escapeHtml :: String -> String
    escapeHtml = concatMap escapeChar
      where
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar '"' = "&quot;"
        escapeChar c   = [c]
