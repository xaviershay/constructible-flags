{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module FlagConstruction
    ( -- * Core types
      FlagA(..)
    , Point
    , Drawing(..)

      -- * Geometric primitives
    , intersectLC
    , intersectCC

      -- * Drawing primitives
    , fillTriangle

      -- * Grouping
    , group

      -- * Composite constructors
    , fillRectangle

      -- * Interpreters
    , Step(..)
    , steps
    , eval
    , ConstructionLayer(..)
    , layerInputPoints
    , layerOutputPoints
    , pointDist
    , evalLayers
    , ConstructionTree(..)
    , evalTree
    , flattenTree

      -- * Example
    , exampleDesign
    ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Colour
import Data.Colour.SRGB (sRGB24)
import qualified Prelude

-- | A 2D point
type Point = (Double, Double)

-- | An abstract drawing instruction
data Drawing
  = DrawTriangle (Colour Double) Point Point Point
  | Overlay Drawing Drawing
  | EmptyDrawing
  deriving (Show)

instance Semigroup Drawing where
  EmptyDrawing <> d = d
  d <> EmptyDrawing = d
  a <> b = Overlay a b

instance Monoid Drawing where
  mempty = EmptyDrawing

-- ---------------------------------------------------------------------------
-- Free Arrow GADT
-- ---------------------------------------------------------------------------

-- | A free arrow encoding geometric constructions as an inspectable data
-- structure. Each constructor represents either a geometric primitive or
-- an arrow combinator, so the entire computation can be traversed as a DAG.
--
-- Intersection primitives take their defining points directly:
--
--   * 'IntersectLC' takes ((line-point1, line-point2), (circle-center, circle-edge))
--   * 'IntersectCC' takes ((center1, edge1), (center2, edge2))
--
-- The 'Arr' constructor contains an opaque function used for tuple wiring
-- (projections, repackaging) inserted by @proc@ notation desugaring.
-- All meaningful geometric steps are visible as named constructors.
data FlagA a b where
  -- Arrow combinators (structural wiring)
  Arr     :: String -> (a -> b) -> FlagA a b
  Compose :: FlagA a b -> FlagA b c -> FlagA a c
  First   :: FlagA a b -> FlagA (a, c) (b, c)
  Par     :: FlagA a b -> FlagA c d -> FlagA (a, c) (b, d)

  -- Geometric primitives (take defining points directly)
  IntersectLC  :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
  IntersectCC  :: FlagA ((Point, Point), (Point, Point)) (Point, Point)

  -- Drawing primitives
  FillTriangle :: Colour Double -> FlagA (Point, Point, Point) Drawing

  -- Grouping (label a sub-computation for documentation / debugging)
  Group :: String -> FlagA a b -> FlagA a b

instance Category FlagA where
  id  = Arr "id" Prelude.id
  (.) = flip Compose

instance Arrow FlagA where
  arr    = Arr "arr"
  first  = First
  (***)  = Par
  f &&& g = Arr "dup" (\a -> (a, a)) >>> Par f g

instance Show (FlagA a b) where
  show = showFlagA 0

-- | Pretty-print the construction tree with indentation
showFlagA :: Int -> FlagA a b -> String
showFlagA n fa = indent n ++ case fa of
  Arr label _     -> "Arr " ++ show label
  Compose f g     -> ">>>\n" ++ showFlagA (n+2) f ++ "\n" ++ showFlagA (n+2) g
  First f         -> "First\n" ++ showFlagA (n+2) f
  Par f g         -> "***\n" ++ showFlagA (n+2) f ++ "\n" ++ showFlagA (n+2) g
  IntersectLC     -> "IntersectLC"
  IntersectCC     -> "IntersectCC"
  FillTriangle _  -> "FillTriangle"
  Group label f   -> "Group " ++ show label ++ "\n" ++ showFlagA (n+2) f
  where
    indent i = replicate i ' '

-- ---------------------------------------------------------------------------
-- Smart constructors (for use in proc blocks)
-- ---------------------------------------------------------------------------

-- | Intersect a line (defined by two points) with a circle (center, edge point)
intersectLC :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
intersectLC = IntersectLC

-- | Intersect two circles, each defined by (center, edge point)
intersectCC :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
intersectCC = IntersectCC

fillTriangle :: Colour Double -> FlagA (Point, Point, Point) Drawing
fillTriangle = FillTriangle

-- | Group a sub-computation under a label for documentation / debugging
group :: String -> FlagA a b -> FlagA a b
group = Group

-- ---------------------------------------------------------------------------
-- Interpreters
-- ---------------------------------------------------------------------------

-- | A labeled construction step for introspection
data Step
  = StepIntersectLC
  | StepIntersectCC
  | StepFillTriangle
  deriving (Show, Eq)

-- | Extract the flat list of geometric construction steps, in order.
-- Ignores structural wiring ('Arr', 'First', etc.) and only reports
-- meaningful geometric operations.
steps :: FlagA a b -> [Step]
steps (Arr _ _)        = []
steps (Compose f g)    = steps f ++ steps g
steps (First f)        = steps f
steps (Par f g)        = steps f ++ steps g
steps IntersectLC      = [StepIntersectLC]
steps IntersectCC      = [StepIntersectCC]
steps (FillTriangle _) = [StepFillTriangle]
steps (Group _ f)      = steps f

-- | Evaluate a construction arrow to produce a concrete function.
-- This is one possible interpreter; others could generate SVG, trace
-- steps, validate, etc.
eval :: FlagA a b -> a -> b
eval (Arr _ f)        = f
eval (Compose f g)    = eval g . eval f
eval (First f)        = \(a, c) -> (eval f a, c)
eval (Par f g)        = \(a, c) -> (eval f a, eval g c)
eval IntersectLC      = evalIntersectLC'
eval IntersectCC      = evalIntersectCC'
eval (FillTriangle c) = \(p1, p2, p3) -> DrawTriangle c p1 p2 p3
eval (Group _ f)      = eval f

-- | Line-circle intersection from defining points
evalIntersectLC' :: ((Point, Point), (Point, Point)) -> (Point, Point)
evalIntersectLC' ((lp1, lp2), (cc, ce)) =
  let r  = dist cc ce
      (x1, y1) = lp1
      (x2, y2) = lp2
      dx = x2 - x1
      dy = y2 - y1
      fx = x1 - fst cc
      fy = y1 - snd cc
      a  = dx*dx + dy*dy
      b  = 2*(fx*dx + fy*dy)
      c  = fx*fx + fy*fy - r*r
      disc = b*b - 4*a*c
      sd = sqrt (max 0 disc)
      t1 = (-b - sd) / (2*a)
      t2 = (-b + sd) / (2*a)
  in ( (x1 + t1*dx, y1 + t1*dy)
     , (x1 + t2*dx, y1 + t2*dy)
     )

-- | Circle-circle intersection from defining points
evalIntersectCC' :: ((Point, Point), (Point, Point)) -> (Point, Point)
evalIntersectCC' ((c1, e1), (c2, e2)) =
  let r1 = dist c1 e1
      r2 = dist c2 e2
      d  = dist c1 c2
      a  = (r1*r1 - r2*r2 + d*d) / (2*d)
      h  = sqrt (max 0 (r1*r1 - a*a))
      (x1, y1) = c1
      (x2, y2) = c2
      mx = x1 + a*(x2 - x1)/d
      my = y1 + a*(y2 - y1)/d
  in ( (mx + h*(y2 - y1)/d, my - h*(x2 - x1)/d)
     , (mx - h*(y2 - y1)/d, my + h*(x2 - x1)/d)
     )

-- | Euclidean distance between two points
dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^(2::Int) + (y2 - y1)^(2::Int))

-- | A single layer of construction output, capturing what was built at each step.
-- This is decoupled from any rendering backend. Each intersection step stores
-- its defining points (so data dependencies can be traced) and result points.
data ConstructionLayer
  = LayerIntersectLC
      Point Point       -- ^ Line defining points
      Point Point       -- ^ Circle defining points (center, edge)
      [Point]           -- ^ Result points
  | LayerIntersectCC
      Point Point       -- ^ First circle (center, edge)
      Point Point       -- ^ Second circle (center, edge)
      [Point]           -- ^ Result points
  | LayerTriangle (Colour Double) Point Point Point  -- ^ A filled triangle
  deriving (Show)

-- | The points consumed as inputs by a construction layer.
layerInputPoints :: ConstructionLayer -> [Point]
layerInputPoints (LayerIntersectLC lp1 lp2 cc ce _) = [lp1, lp2, cc, ce]
layerInputPoints (LayerIntersectCC c1 e1 c2 e2 _)   = [c1, e1, c2, e2]
layerInputPoints (LayerTriangle _ p1 p2 p3)          = [p1, p2, p3]

-- | The points produced as outputs by a construction layer.
layerOutputPoints :: ConstructionLayer -> [Point]
layerOutputPoints (LayerIntersectLC _ _ _ _ pts) = pts
layerOutputPoints (LayerIntersectCC _ _ _ _ pts) = pts
layerOutputPoints (LayerTriangle _ _ _ _)        = []

-- | Euclidean distance (exported for rendering code that derives radii).
pointDist :: Point -> Point -> Double
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
evalLayers IntersectLC      inp@((lp1, lp2), (cc, ce)) =
    let (p1, p2) = evalIntersectLC' inp
    in  ((p1, p2), [LayerIntersectLC lp1 lp2 cc ce [p1, p2]])
evalLayers IntersectCC      inp@((c1, e1), (c2, e2)) =
    let (p1, p2) = evalIntersectCC' inp
    in  ((p1, p2), [LayerIntersectCC c1 e1 c2 e2 [p1, p2]])
evalLayers (FillTriangle col) (p1, p2, p3) =
    (DrawTriangle col p1 p2 p3, [LayerTriangle col p1 p2 p3])
evalLayers (Group _ f)        x = evalLayers f x

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
evalTree IntersectLC      inp@((lp1, lp2), (cc, ce)) =
    let (p1, p2) = evalIntersectLC' inp
    in  ((p1, p2), [TreeLayer (LayerIntersectLC lp1 lp2 cc ce [p1, p2])])
evalTree IntersectCC      inp@((c1, e1), (c2, e2)) =
    let (p1, p2) = evalIntersectCC' inp
    in  ((p1, p2), [TreeLayer (LayerIntersectCC c1 e1 c2 e2 [p1, p2])])
evalTree (FillTriangle col) (p1, p2, p3) =
    (DrawTriangle col p1 p2 p3, [TreeLayer (LayerTriangle col p1 p2 p3)])
evalTree (Group label f) x =
    let (res, children) = evalTree f x
    in  (res, [TreeGroup label children])

-- | Flatten a construction tree back to a list of layers (for rendering).
flattenTree :: ConstructionTree -> [ConstructionLayer]
flattenTree (TreeLayer l)       = [l]
flattenTree (TreeGroup _ children) = concatMap flattenTree children

-- ---------------------------------------------------------------------------
-- Example: the design from the highlighted pseudocode
-- ---------------------------------------------------------------------------

red :: Colour Double
red = sRGB24 255 0 0

blue :: Colour Double
blue = sRGB24 0 0 255

fillRectangle :: Colour Double -> FlagA (Point, Point, Point, Point) Drawing
fillRectangle c = group "Fill rectangle" $ proc (v1, v2, v3, v4) -> do
    t1 <- fillTriangle c -< (v1, v2, v3)
    t2 <- fillTriangle c -< (v3, v4, v1)
    returnA -< t1 <> t2

perpendicular :: FlagA (Point, Point) (Point, Point)
perpendicular = group "Perpendicular points" $ proc (a, b) -> do
    (c, _) <- intersectLC -< ((a, b), (a, b))
    (d, e) <- intersectCC -< ((c, b), (b, c))
    (p, p') <- intersectLC -< ((d, e), (a, b))

    returnA -< (p, p')

exampleDesign :: FlagA (Point, Point) Drawing
exampleDesign = proc (a, b) -> do
    (_, p) <- perpendicular -< (a, b)

    fillRectangle red -< (a, b, p, a)