{-# LANGUAGE GADTs #-}

module Flag.Construction.Tree
    ( ConstructionTree(..)
    , evalTree
    , flattenTree
    ) where

import Flag.Construction.Types
import Flag.Construction.Geometry
import Flag.Construction.Layers (ConstructionLayer(..))

-- | A tree of construction output that preserves group structure.
-- Groups form interior nodes; leaves are individual construction layers.
data ConstructionTree
  = TreeLayer ConstructionLayer       -- ^ A single construction step
  | TreeGroup String [ConstructionTree]  -- ^ A named group of sub-steps
  deriving (Show)

-- | Evaluate a construction arrow into a tree that preserves group labels.
evalTree :: FlagA a b -> a -> (b, [ConstructionTree])
evalTree (Arr _ f)        x     = (f x, [])
evalTree (Compose f g)    x     = let (mid, t1) = evalTree f x
                                      (res, t2) = evalTree g mid
                                  in  (res, t1 ++ t2)
evalTree (First f)        (a,c) = let (b, ts) = evalTree f a
                                  in  ((b, c), ts)
evalTree (Par f g)        (a,c) = let (b, t1) = evalTree f a
                                      (d, t2) = evalTree g c
                                  in  ((b, d), t1 ++ t2)
evalTree IntersectLL      inp@((lp1, lp2), (lp3, lp4)) =
    let p = evalIntersectLL' inp
    in  (p, [TreeLayer (LayerIntersectLL lp1 lp2 lp3 lp4 [p])])
evalTree IntersectLC      inp@((lp1, lp2), (cc, ce)) =
    let (p1, p2) = evalIntersectLC' inp
    in  ((p1, p2), [TreeLayer (LayerIntersectLC lp1 lp2 cc ce [p1, p2])])
evalTree IntersectCC      inp@((c1, e1), (c2, e2)) =
    let (p1, p2) = evalIntersectCC' inp
    in  ((p1, p2), [TreeLayer (LayerIntersectCC c1 e1 c2 e2 [p1, p2])])
evalTree (NGonVertex n k) inp@(c, e) =
    let p = evalNGonVertex n k inp
    in  (p, [TreeLayer (LayerNGonVertex c e [p])])
evalTree (FillTriangle col) (p1, p2, p3) =
    (DrawTriangle col p1 p2 p3, [TreeLayer (LayerTriangle col p1 p2 p3)])
evalTree (FillCircle col) (center, edge) =
    (DrawCircle col center (dist center edge), [TreeLayer (LayerCircle col center edge)])
evalTree (OverlaySVG path) (center, edge) =
    (DrawSVGOverlay path center edge, [TreeLayer (LayerSVGOverlay path center edge)])
evalTree (Group label f) x =
    let (res, children) = evalTree f x
    in  (res, [TreeGroup label children])

-- | Flatten a construction tree back to a list of layers (for rendering).
flattenTree :: ConstructionTree -> [ConstructionLayer]
flattenTree (TreeLayer l)       = [l]
flattenTree (TreeGroup _ children) = concatMap flattenTree children
