{-# LANGUAGE GADTs #-}

module Flag.Construction.Layers
    ( ConstructionLayer(..)
    , layerInputPoints
    , layerOutputPoints
    , pointDist
    , evalLayers
    ) where

import Data.Colour

import Flag.Construction.Radical (Radical)
import Flag.Construction.Types
import Flag.Construction.Geometry

-- | A single layer of construction output, capturing what was built at each step.
-- This is decoupled from any rendering backend. Each intersection step stores
-- its defining points (so data dependencies can be traced) and result points.
data ConstructionLayer
  = LayerIntersectLL
      Point Point       -- ^ First line defining points
      Point Point       -- ^ Second line defining points
      [Point]           -- ^ Result point
  | LayerIntersectLC
      Point Point       -- ^ Line defining points
      Point Point       -- ^ Circle defining points (center, edge)
      [Point]           -- ^ Result points
  | LayerIntersectCC
      Point Point       -- ^ First circle (center, edge)
      Point Point       -- ^ Second circle (center, edge)
      [Point]           -- ^ Result points
  | LayerTriangle (Colour Double) Point Point Point  -- ^ A filled triangle
  | LayerCircle (Colour Double) Point Point  -- ^ A filled circle (center, edge)
  deriving (Show)

-- | The points consumed as inputs by a construction layer.
layerInputPoints :: ConstructionLayer -> [Point]
layerInputPoints (LayerIntersectLL lp1 lp2 lp3 lp4 _) = [lp1, lp2, lp3, lp4]
layerInputPoints (LayerIntersectLC lp1 lp2 cc ce _) = [lp1, lp2, cc, ce]
layerInputPoints (LayerIntersectCC c1 e1 c2 e2 _)   = [c1, e1, c2, e2]
layerInputPoints (LayerTriangle _ p1 p2 p3)          = [p1, p2, p3]
layerInputPoints (LayerCircle _ center edge)         = [center, edge]

-- | The points produced as outputs by a construction layer.
layerOutputPoints :: ConstructionLayer -> [Point]
layerOutputPoints (LayerIntersectLL _ _ _ _ pts) = pts
layerOutputPoints (LayerIntersectLC _ _ _ _ pts) = pts
layerOutputPoints (LayerIntersectCC _ _ _ _ pts) = pts
layerOutputPoints (LayerTriangle _ _ _ _)        = []
layerOutputPoints (LayerCircle _ _ _)            = []

-- | Euclidean distance (exported for rendering code that derives radii).
pointDist :: Point -> Point -> Radical
pointDist = dist

-- | Evaluate a construction arrow, producing the result and a list of
-- construction layers (one per construction step).
evalLayers :: FlagA a b -> a -> (b, [ConstructionLayer])
evalLayers (Arr _ f)        x     = (f x, [])
evalLayers (Compose f g)    x     = let (mid, l1) = evalLayers f x
                                        (res, l2) = evalLayers g mid
                                    in  (res, l1 ++ l2)
evalLayers (First f)        (a,c) = let (b, ls) = evalLayers f a
                                    in  ((b, c), ls)
evalLayers (Par f g)        (a,c) = let (b, l1) = evalLayers f a
                                        (d, l2) = evalLayers g c
                                    in  ((b, d), l1 ++ l2)
evalLayers IntersectLL      inp@((lp1, lp2), (lp3, lp4)) =
    let p = evalIntersectLL' inp
    in  (p, [LayerIntersectLL lp1 lp2 lp3 lp4 [p]])
evalLayers IntersectLC      inp@((lp1, lp2), (cc, ce)) =
    let (p1, p2) = evalIntersectLC' inp
    in  ((p1, p2), [LayerIntersectLC lp1 lp2 cc ce [p1, p2]])
evalLayers IntersectCC      inp@((c1, e1), (c2, e2)) =
    let (p1, p2) = evalIntersectCC' inp
    in  ((p1, p2), [LayerIntersectCC c1 e1 c2 e2 [p1, p2]])
evalLayers (FillTriangle col) (p1, p2, p3) =
    (DrawTriangle col p1 p2 p3, [LayerTriangle col p1 p2 p3])
evalLayers (FillCircle col) (center, edge) =
    (DrawCircle col center (dist center edge), [LayerCircle col center edge])
evalLayers (Group _ f)        x = evalLayers f x
