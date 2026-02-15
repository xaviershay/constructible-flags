{-# LANGUAGE GADTs #-}

module Flag.Construction.Types
    ( -- * Core types
      Point
    , Drawing(..)
    , FlagA(..)
    , showFlagA
    , drawingRadicals
    ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Colour
import qualified Prelude

import Flag.Construction.Radical (Radical)

-- | A 2D point with exact radical coordinates
type Point = (Radical, Radical)

-- | An abstract drawing instruction
data Drawing
  = DrawTriangle (Colour Double) Point Point Point
  | DrawPath (Colour Double) [Point]  -- ^ A closed polygon as an ordered list of vertices
  | DrawCircle (Colour Double) Point Radical  -- ^ A filled circle: colour, center, radius
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
  IntersectLL  :: FlagA ((Point, Point), (Point, Point)) Point
  IntersectLC  :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
  IntersectCC  :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
  
  -- | The k-th vertex of a regular n-gon inscribed in the given circle.
  -- Input: (center, firstVertex). Output: the k-th vertex (0-indexed).
  NGonVertex   :: !Int -> !Int -> FlagA (Point, Point) Point

  -- Drawing primitives
  FillTriangle :: Colour Double -> FlagA (Point, Point, Point) Drawing
  FillCircle   :: Colour Double -> FlagA (Point, Point) Drawing  -- ^ (center, edgePoint)

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
  IntersectLL     -> "IntersectLL"
  IntersectLC     -> "IntersectLC"
  IntersectCC     -> "IntersectCC"
  NGonVertex _ _  -> "NGonVertex"
  FillTriangle _  -> "FillTriangle"
  FillCircle _    -> "FillCircle"
  Group label f   -> "Group " ++ show label ++ "\n" ++ showFlagA (n+2) f
  where
    indent i = replicate i ' '

-- ---------------------------------------------------------------------------
-- Radical extraction
-- ---------------------------------------------------------------------------

-- | Collect all 'Radical' values from a 'Drawing' (coordinates and radii).
drawingRadicals :: Drawing -> [Radical]
drawingRadicals EmptyDrawing = []
drawingRadicals (DrawTriangle _ (x1,y1) (x2,y2) (x3,y3)) =
  [x1, y1, x2, y2, x3, y3]
drawingRadicals (DrawPath _ pts) =
  concatMap (\(x, y) -> [x, y]) pts
drawingRadicals (DrawCircle _ (cx, cy) r) =
  [cx, cy, r]
drawingRadicals (Overlay a b) =
  drawingRadicals a ++ drawingRadicals b
