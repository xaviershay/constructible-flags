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
import Data.Maybe (mapMaybe)
import Effectful (runPureEff)
import System.Directory (createDirectoryIfMissing)

import Flag.Construction.Types (Point)
import Flag.Construction.Layers (ConstructionLayer(..), layerInputPoints, layerOutputPoints, pointDist)
import Flag.Construction.Tree (ConstructionTree(..), evalTree)
import Flag.Construction.Optimize (optimize)
import Flag.Construction.FieldNumber (toDouble)
import Flag.Source (Sourced, runSourcedPure)
import Flag.Definition (Flag(..))
import Flag.Render.Diagram (drawingToElement, renderConstructionGeom, renderFill, renderDots)
import Flag.Render.SVGOverlay (writeSVG, drawingBounds)

-- | Convert a Point (Radical, Radical) to (Double, Double) for rendering.
toDP :: Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

-- ---------------------------------------------------------------------------
-- Bounding box helpers (for uniform canvas across debug steps)
-- ---------------------------------------------------------------------------

type BBox = (Double, Double, Double, Double)  -- (minX, minY, maxX, maxY)

unionBBox :: BBox -> BBox -> BBox
unionBBox (ax1, ay1, ax2, ay2) (bx1, by1, bx2, by2) =
    (min ax1 bx1, min ay1 by1, max ax2 bx2, max ay2 by2)

-- | Expand a bounding box uniformly by a scale factor (e.g. 1.3 for 30% padding).
applyPadding :: Double -> BBox -> BBox
applyPadding factor (minX, minY, maxX, maxY) =
    let w  = maxX - minX
        h  = maxY - minY
        cx = (minX + maxX) / 2
        cy = (minY + maxY) / 2
        hw = w * factor / 2
        hh = h * factor / 2
    in (cx - hw, cy - hh, cx + hw, cy + hh)

-- | Compute the bounding box of a 'ConstructionLayer' from its geometry.
layerBounds :: ConstructionLayer -> Maybe BBox
layerBounds (LayerIntersectLL p1 p2 p3 p4 pts) =
    let allPts = map toDP (p1 : p2 : p3 : p4 : pts)
    in Just (minimum (map fst allPts), minimum (map snd allPts),
             maximum (map fst allPts), maximum (map snd allPts))
layerBounds (LayerIntersectLC p1 p2 cc ce pts) =
    let r = toDouble (pointDist cc ce)
        (ccx, ccy) = toDP cc
        linePts = map toDP [p1, p2] ++ map toDP pts
        xs = map fst linePts ++ [ccx - r, ccx + r]
        ys = map snd linePts ++ [ccy - r, ccy + r]
    in Just (minimum xs, minimum ys, maximum xs, maximum ys)
layerBounds (LayerIntersectCC c1 e1 c2 e2 pts) =
    let r1 = toDouble (pointDist c1 e1)
        r2 = toDouble (pointDist c2 e2)
        (c1x, c1y) = toDP c1
        (c2x, c2y) = toDP c2
        pts' = map toDP pts
        xs = map fst pts' ++ [c1x - r1, c1x + r1, c2x - r2, c2x + r2]
        ys = map snd pts' ++ [c1y - r1, c1y + r1, c2y - r2, c2y + r2]
    in Just (minimum xs, minimum ys, maximum xs, maximum ys)
layerBounds (LayerNGonVertex c e pts) =
    let r = toDouble (pointDist c e)
        (cx, cy) = toDP c
        pts' = map toDP pts
        xs = map fst pts' ++ [cx - r, cx + r]
        ys = map snd pts' ++ [cy - r, cy + r]
    in Just (minimum xs, minimum ys, maximum xs, maximum ys)
layerBounds (LayerTriangle _ p1 p2 p3) =
    let pts = map toDP [p1, p2, p3]
    in Just (minimum (map fst pts), minimum (map snd pts),
             maximum (map fst pts), maximum (map snd pts))
layerBounds (LayerCircle _ center edge) =
    let r = toDouble (pointDist center edge)
        (cx, cy) = toDP center
    in Just (cx - r, cy - r, cx + r, cy + r)
layerBounds (LayerCrescent _ oc oe _ _) =
    let r = toDouble (pointDist oc oe)
        (cx, cy) = toDP oc
    in Just (cx - r, cy - r, cx + r, cy + r)
layerBounds (LayerSVGOverlay _ center edge) =
    let (cx, cy) = toDP center
        (ex, ey) = toDP edge
        r = sqrt ((ex - cx) ^ (2 :: Int) + (ey - cy) ^ (2 :: Int))
    in Just (cx - r, cy - r, cx + r, cy + r)

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
layerLabel LayerCrescent {}        = "Fill crescent"
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

  -- Compute a uniform bounding box from all layers (replacing Diagrams' phantom).
  -- We also include the initial points with a small epsilon to avoid zero-height boxes.
  let allBBoxes  = mapMaybe layerBounds allLayers
      baseBBox   = (0.0, -0.05, 1.0, 0.05) :: BBox  -- covers the two initial points
      unionBB    = foldl unionBBox baseBBox allBBoxes
      padded     = applyPadding 1.3 unionBB

  -- Compute the set of points that are consumed by step i or later.
  -- liveAfter !! i  =  inputs of steps i .. n-1
  let nSteps = length allLayers
      liveAfter = [ nub $ concatMap layerInputPoints (drop i allLayers)
                  | i <- [0 .. nSteps] ]

  -- Render step 0: initial points, but only those that are live
  let livePts0  = filter (`elem` (liveAfter !! 0)) initialPts
      step0Elem = renderDots (map toDP livePts0)

  -- Render steps 1..n
  let stepElems = [ let layerIdx    = i - 1
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

  let allElems = step0Elem : stepElems

  mapM_ (\(i, el) -> do
      let path = "out/debug/step-" ++ padNum i ++ ".svg"
      writeSVG path 400 padded el
      putStrLn $ "  Wrote " ++ path
    ) (zip [0::Int ..] allElems)

  -- Render the final flag using drawingToElement
  let finalEl = drawingToElement (optimize result)
      finalBB = case drawingBounds (optimize result) of
                  Just bb -> bb
                  Nothing -> padded
  writeSVG "out/debug/final.svg" 400 finalBB finalEl
  putStrLn "  Wrote out/debug/final.svg"

  -- Generate index.html reflecting the tree structure
  let indexHtml = generateDebugIndexHtml numbered (length allElems)
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
