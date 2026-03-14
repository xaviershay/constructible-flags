{-# LANGUAGE GADTs #-}

module Flag.Construction.Tree
  ( ConstructionTree (..),
    evalTree,
    flattenTree,
    pruneTree,
    prunedSteps,
  )
where

import qualified Data.IntSet as IS
import Flag.Construction.Geometry
import Flag.Construction.Interpreter (Step, layerStep)
import Flag.Construction.Layers (ConstructionLayer (..), pruneLayers)
import Flag.Construction.Types

-- | A tree of construction output that preserves group structure.
-- Groups form interior nodes; leaves are individual construction layers.
data ConstructionTree
  = -- | A single construction step
    TreeLayer ConstructionLayer
  | -- | A named group of sub-steps
    TreeGroup String [ConstructionTree]
  deriving (Show)

-- | Evaluate a construction arrow into a tree that preserves group labels.
evalTree :: FlagA a b -> a -> (b, [ConstructionTree])
evalTree (Arr _ f) x = let b = f x in b `seq` (b, [])
evalTree (Compose f g) x =
  let (mid, t1) = evalTree f x
      (res, t2) = evalTree g mid
   in (res, t1 ++ t2)
evalTree (First f) (a, c) =
  let (b, ts) = evalTree f a
   in ((b, c), ts)
evalTree (Par f g) (a, c) =
  let (b, t1) = evalTree f a
      (d, t2) = evalTree g c
   in ((b, d), t1 ++ t2)
evalTree IntersectLL inp@((lp1, lp2), (lp3, lp4)) =
  let p = evalIntersectLL' inp
   in (p, [TreeLayer (LayerIntersectLL lp1 lp2 lp3 lp4 [p])])
evalTree IntersectLC inp@((lp1, lp2), (cc, ce)) =
  let (p1, p2) = evalIntersectLC' inp
   in ((p1, p2), [TreeLayer (LayerIntersectLC lp1 lp2 cc ce [p1, p2])])
evalTree IntersectCC inp@((c1, e1), (c2, e2)) =
  let (p1, p2) = evalIntersectCC' inp
   in ((p1, p2), [TreeLayer (LayerIntersectCC c1 e1 c2 e2 [p1, p2])])
evalTree (NGonVertex n k) inp@(c, e) =
  let p = evalNGonVertex n k inp
   in (p, [TreeLayer (LayerNGonVertex c e [p])])
evalTree (FillTriangle col) (p1, p2, p3) =
  (DrawTriangle col p1 p2 p3, [TreeLayer (LayerTriangle col p1 p2 p3)])
evalTree (FillCircle col) (center, edge) =
  (DrawCircle col center (dist center edge), [TreeLayer (LayerCircle col center edge)])
evalTree (MaskDrawing mode) (content, maskD) =
  (DrawMasked mode content maskD, [TreeLayer (LayerMasked mode content maskD)])
evalTree (OverlaySVG path) (center, edge) =
  (DrawSVGOverlay path center edge, [TreeLayer (LayerSVGOverlay path center edge)])
evalTree (Group label f) x =
  let (res, children) = evalTree f x
   in (res, [TreeGroup label children])
evalTree (LabelPoint _) p = (p, [])

-- | Flatten a construction tree back to a list of layers (for rendering).
flattenTree :: ConstructionTree -> [ConstructionLayer]
flattenTree (TreeLayer l) = [l]
flattenTree (TreeGroup _ children) = concatMap flattenTree children

-- | The canonical cost calculation: prune the forest, flatten to layers,
-- and return only the geometric construction steps (no drawing primitives,
-- no labels).  This is the single source of truth for the cost figure
-- shown on flag pages and checked in regression tests.
prunedSteps :: [ConstructionTree] -> [Step]
prunedSteps forest =
  [s | l <- concatMap flattenTree (pruneTree forest), Just s <- [layerStep l]]

-- | Prune a forest of 'ConstructionTree' nodes using the same two-pass
-- logic as 'pruneLayers' (duplicate-output elimination followed by
-- dead-computation elimination), but preserving the group structure.
--
-- Pruning crosses group boundaries: a layer inside one group may be
-- removed because its output is only consumed by a layer in a different
-- group, and vice versa.  The decision of what to keep is made globally
-- on the flattened layer list; the surviving layers are then used to
-- reconstruct the tree, dropping any groups that become entirely empty.
pruneTree :: [ConstructionTree] -> [ConstructionTree]
pruneTree forest = snd (walkForest 0 forest)
  where
    -- The set of 0-based leaf indices (in document order) that survive
    -- pruneLayers.  We recover these by aligning the pruned output against
    -- the original flat list: whenever the heads match by show we record
    -- the current index and advance both pointers; otherwise we advance
    -- only the original pointer.
    keptIndices :: IS.IntSet
    keptIndices = IS.fromList (align 0 prunedLayers flatLayers)
      where
        flatLayers = concatMap flattenTree forest
        prunedLayers = pruneLayers flatLayers

        align _ [] _ = []
        align _ _ [] = []
        align i (p : ps) (o : os)
          | show p == show o = i : align (i + 1) ps os
          | otherwise = align (i + 1) (p : ps) os

    -- Walk a forest left-to-right, threading a leaf counter through.
    -- Returns the updated counter and the pruned forest.
    walkForest :: Int -> [ConstructionTree] -> (Int, [ConstructionTree])
    walkForest i [] = (i, [])
    walkForest i (t : ts) =
      let (i', mt) = walkTree i t
          (i'', rest) = walkForest i' ts
       in (i'', maybe rest (: rest) mt)

    -- Walk a single tree node, returning the updated counter and
    -- either the (possibly trimmed) node or Nothing if it was pruned.
    walkTree :: Int -> ConstructionTree -> (Int, Maybe ConstructionTree)
    walkTree i (TreeLayer l)
      | IS.member i keptIndices = (i + 1, Just (TreeLayer l))
      | otherwise = (i + 1, Nothing)
    walkTree i (TreeGroup label children) =
      let (i', children') = walkForest i children
       in case children' of
            [] -> (i', Nothing)
            _ -> (i', Just (TreeGroup label children'))
