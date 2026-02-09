{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module FlagConstruction
    ( -- * Core types
      FlagA(..)
    , Point
    , Line(..)
    , Circle(..)
    , Drawing(..)

      -- * Geometric primitives
    , mkLine
    , mkCircle
    , intersectLC
    , intersectCC

      -- * Drawing primitives
    , fillTriangle

      -- * Interpreters
    , Step(..)
    , steps
    , eval
    , ConstructionLayer(..)
    , evalLayers

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

-- | A line defined by two points
data Line = Line Point Point
  deriving (Show, Eq)

-- | A circle defined by center and a point on its circumference
data Circle = Circle Point Point
  deriving (Show, Eq)

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
-- The 'Arr' constructor contains an opaque function used for tuple wiring
-- (projections, repackaging) inserted by @proc@ notation desugaring.
-- All meaningful geometric steps are visible as named constructors.
data FlagA a b where
  -- Arrow combinators (structural wiring)
  Arr     :: String -> (a -> b) -> FlagA a b
  Compose :: FlagA a b -> FlagA b c -> FlagA a c
  First   :: FlagA a b -> FlagA (a, c) (b, c)
  Par     :: FlagA a b -> FlagA c d -> FlagA (a, c) (b, d)

  -- Geometric primitives
  MkLine       :: FlagA (Point, Point) Line
  MkCircle     :: FlagA (Point, Point) Circle
  IntersectLC  :: FlagA (Line, Circle) (Point, Point)
  IntersectCC  :: FlagA (Circle, Circle) (Point, Point)

  -- Drawing primitives
  FillTriangle :: Colour Double -> FlagA (Point, Point, Point) Drawing

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
  MkLine          -> "MkLine"
  MkCircle        -> "MkCircle"
  IntersectLC     -> "IntersectLC"
  IntersectCC     -> "IntersectCC"
  FillTriangle _  -> "FillTriangle"
  where
    indent i = replicate i ' '

-- ---------------------------------------------------------------------------
-- Smart constructors (for use in proc blocks)
-- ---------------------------------------------------------------------------

mkLine :: FlagA (Point, Point) Line
mkLine = MkLine

mkCircle :: FlagA (Point, Point) Circle
mkCircle = MkCircle

intersectLC :: FlagA (Line, Circle) (Point, Point)
intersectLC = IntersectLC

intersectCC :: FlagA (Circle, Circle) (Point, Point)
intersectCC = IntersectCC

fillTriangle :: Colour Double -> FlagA (Point, Point, Point) Drawing
fillTriangle = FillTriangle

-- ---------------------------------------------------------------------------
-- Interpreters
-- ---------------------------------------------------------------------------

-- | A labeled construction step for introspection
data Step
  = StepMkLine
  | StepMkCircle
  | StepIntersectLC
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
steps MkLine           = [StepMkLine]
steps MkCircle         = [StepMkCircle]
steps IntersectLC      = [StepIntersectLC]
steps IntersectCC      = [StepIntersectCC]
steps (FillTriangle _) = [StepFillTriangle]

-- | Evaluate a construction arrow to produce a concrete function.
-- This is one possible interpreter; others could generate SVG, trace
-- steps, validate, etc.
eval :: FlagA a b -> a -> b
eval (Arr _ f)        = f
eval (Compose f g)    = eval g . eval f
eval (First f)        = \(a, c) -> (eval f a, c)
eval (Par f g)        = \(a, c) -> (eval f a, eval g c)
eval MkLine           = uncurry Line
eval MkCircle         = uncurry Circle
eval IntersectLC      = \(l, c) -> evalIntersectLC l c
eval IntersectCC      = \(c1, c2) -> evalIntersectCC c1 c2
eval (FillTriangle c) = \(p1, p2, p3) -> DrawTriangle c p1 p2 p3

-- | Line-circle intersection (real geometry)
evalIntersectLC :: Line -> Circle -> (Point, Point)
evalIntersectLC (Line (x1, y1) (x2, y2)) (Circle (cx, cy) (rx, ry)) =
  let r  = sqrt ((rx - cx)^(2::Int) + (ry - cy)^(2::Int))
      dx = x2 - x1
      dy = y2 - y1
      fx = x1 - cx
      fy = y1 - cy
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

-- | Circle-circle intersection (real geometry)
evalIntersectCC :: Circle -> Circle -> (Point, Point)
evalIntersectCC (Circle (x1, y1) (rx1, ry1)) (Circle (x2, y2) (rx2, ry2)) =
  let r1 = sqrt ((rx1 - x1)^(2::Int) + (ry1 - y1)^(2::Int))
      r2 = sqrt ((rx2 - x2)^(2::Int) + (ry2 - y2)^(2::Int))
      d  = sqrt ((x2 - x1)^(2::Int) + (y2 - y1)^(2::Int))
      a  = (r1*r1 - r2*r2 + d*d) / (2*d)
      h  = sqrt (max 0 (r1*r1 - a*a))
      mx = x1 + a*(x2 - x1)/d
      my = y1 + a*(y2 - y1)/d
  in ( (mx + h*(y2 - y1)/d, my - h*(x2 - x1)/d)
     , (mx - h*(y2 - y1)/d, my + h*(x2 - x1)/d)
     )

-- | A single layer of construction output, capturing what was built at each step.
-- This is decoupled from any rendering backend.
data ConstructionLayer
  = LayerLine Point Point           -- ^ A line through two points
  | LayerCircle Point Double        -- ^ A circle at center with radius
  | LayerIntersections [Point]      -- ^ Intersection point(s)
  | LayerTriangle (Colour Double) Point Point Point  -- ^ A filled triangle
  deriving (Show)

-- | Evaluate a construction arrow, producing the result and a list of
-- construction layers (one per geometric primitive). Each layer represents
-- a single construction step. To render cumulative step-by-step diagrams,
-- take successive prefixes of the layer list.
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
evalLayers MkLine           (p1, p2) =
    (Line p1 p2, [LayerLine p1 p2])
evalLayers MkCircle         (center, edge) =
    let c = Circle center edge
        r = sqrt ((fst edge - fst center)^(2::Int) + (snd edge - snd center)^(2::Int))
    in  (c, [LayerCircle center r])
evalLayers IntersectLC      (l, c) =
    let pts@(p1, p2) = evalIntersectLC l c
    in  (pts, [LayerIntersections [p1, p2]])
evalLayers IntersectCC      (c1, c2) =
    let pts@(p1, p2) = evalIntersectCC c1 c2
    in  (pts, [LayerIntersections [p1, p2]])
evalLayers (FillTriangle col) (p1, p2, p3) =
    (DrawTriangle col p1 p2 p3, [LayerTriangle col p1 p2 p3])

-- ---------------------------------------------------------------------------
-- Example: the design from the highlighted pseudocode
-- ---------------------------------------------------------------------------

red :: Colour Double
red = sRGB24 255 0 0

blue :: Colour Double
blue = sRGB24 0 0 255

-- | The highlighted example, translated to proc notation.
--
-- Given two initial points (a, b):
--   1. Construct the line through a and b, and the circle centered at a through b
--   2. Intersect them to get point c (discard the second intersection)
--   3. Construct two circles: one centered at c through b, one at b through c
--   4. Intersect those circles to get points d and e
--   5. Fill two triangles: (d, b, e) in red and (d, c, e) in blue
--
-- Original pseudocode:
-- @
-- design a b = do
--     (c, _) <- intersectLineCircle (a, b) (a, b)
--     (d, e) <- intersectCircleCircle (c, b) (b, c)
--     fillTriangle red d b e <> fillTriangle blue d c e
-- @
exampleDesign :: FlagA (Point, Point) Drawing
exampleDesign = proc (a, b) -> do
    lab  <- mkLine   -< (a, b)
    cab  <- mkCircle -< (a, b)
    (c, _) <- intersectLC -< (lab, cab)

    ccb  <- mkCircle -< (c, b)
    cbc  <- mkCircle -< (b, c)
    (d, e) <- intersectCC -< (ccb, cbc)

    t1 <- fillTriangle red  -< (d, b, e)
    t2 <- fillTriangle blue -< (d, c, e)

    returnA -< t1 <> t2
