{-# LANGUAGE GADTs #-}

module Flag.Construction.Types
  ( -- * Core types
    Number,
    Point,
    MaskMode (..),
    Drawing (..),
    FlagA (..),
    showFlagA,
    drawingNumbers,
  )
where

import Control.Arrow
import Control.Category
import Data.Colour
import Flag.Construction.FieldNumber (FieldNumber, toDouble)
import Numeric (showFFloat)
import Prelude hiding (id, (.))
import qualified Prelude

-- | The numeric type used for all coordinates and radii.
type Number = FieldNumber

-- | A 2D point with coordinates
type Point = (Number, Number)

-- | Controls how the mask drawing is interpreted in 'DrawMasked'.
data MaskMode
  = -- | Shapes in the mask drawing become transparent (cut-out) areas.
    Mask
  | -- | Shapes in the mask drawing become the visible area; everything else is hidden.
    Clip
  deriving (Eq, Show)

-- | An abstract drawing instruction
data Drawing
  = DrawTriangle (Colour Double) Point Point Point
  | -- | A closed polygon as an ordered list of vertices
    DrawPath (Colour Double) [Point]
  | -- | A filled circle: colour, center, radius
    DrawCircle (Colour Double) Point Number
  | -- | (mode, content, mask): render content with mask applied.
    -- In Mask mode, coloured shapes in the mask drawing punch transparent holes in the content.
    -- In Clip mode, coloured shapes in the mask drawing define the visible region of the content.
    DrawMasked MaskMode Drawing Drawing
  | -- | An external SVG overlay: file path, center, edge
    DrawSVGOverlay FilePath Point Point
  | Overlay Drawing Drawing
  | EmptyDrawing

-- | Pretty-print a 'Drawing' with one operation per line, indenting
-- according to overlay depth and showing numerical approximations
-- (Doubles) for point coordinates and radii.
formatDrawing :: Int -> Drawing -> String
formatDrawing n d = unlines (go n d)
  where
    indent _ = ""
    showDbl r = showFFloat (Just 6) (toDouble r) ""
    showPoint (x, y) = "(" ++ showDbl x ++ ", " ++ showDbl y ++ ")"
    go :: Int -> Drawing -> [String]
    go _ (DrawTriangle col p1 p2 p3) = ["DrawTriangle " ++ show col ++ " " ++ showPoint p1 ++ " " ++ showPoint p2 ++ " " ++ showPoint p3]
    go _ (DrawPath col pts) = ["DrawPath " ++ show col ++ " " ++ show (map showPoint pts)]
    go _ (DrawCircle col center r) = ["DrawCircle " ++ show col ++ " " ++ showPoint center ++ " r=" ++ showDbl r]
    go _ (DrawMasked mode content mask) = ["DrawMasked " ++ show mode ++ " content=" ++ show content ++ " mask=" ++ show mask]
    go _ (DrawSVGOverlay path center edge) = ["DrawSVGOverlay " ++ show path ++ " " ++ showPoint center ++ " " ++ showPoint edge]
    go _ (Overlay a b) = go 0 a ++ go 0 b
    go _ EmptyDrawing = ["EmptyDrawing"]

instance Show Drawing where
  show = formatDrawing 0

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
  Arr :: String -> (a -> b) -> FlagA a b
  Compose :: FlagA a b -> FlagA b c -> FlagA a c
  First :: FlagA a b -> FlagA (a, c) (b, c)
  Par :: FlagA a b -> FlagA c d -> FlagA (a, c) (b, d)
  -- Geometric primitives (take defining points directly)
  IntersectLL :: FlagA ((Point, Point), (Point, Point)) Point
  IntersectLC :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
  IntersectCC :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
  -- | The k-th vertex of a regular n-gon inscribed in the given circle.
  -- Input: (center, firstVertex). Output: the k-th vertex (0-indexed).
  NGonVertex :: !Int -> !Int -> FlagA (Point, Point) Point
  -- Drawing primitives
  FillTriangle :: Colour Double -> FlagA (Point, Point, Point) Drawing
  FillCircle ::
    Colour Double ->
    -- | (center, edgePoint)
    FlagA (Point, Point) Drawing
  MaskDrawing ::
    MaskMode ->
    -- | (content, mask): render content drawing with mask drawing applied according to MaskMode
    FlagA (Drawing, Drawing) Drawing
  OverlaySVG ::
    FilePath ->
    -- | (center, edgePoint)
    FlagA (Point, Point) Drawing
  -- Grouping (label a sub-computation for documentation / debugging)
  Group :: String -> FlagA a b -> FlagA a b
  -- Point labelling (attach a human-readable name to a point for the debug viewer)
  LabelPoint :: String -> FlagA Point Point

instance Category FlagA where
  id = Arr "id" Prelude.id
  (.) = flip Compose

instance Arrow FlagA where
  arr = Arr "arr"
  first = First
  (***) = Par
  f &&& g = Arr "dup" (\a -> (a, a)) >>> Par f g

instance Show (FlagA a b) where
  show = showFlagA 0

-- | Pretty-print the construction tree with indentation
showFlagA :: Int -> FlagA a b -> String
showFlagA n fa =
  indent n ++ case fa of
    Arr label _ -> "Arr " ++ show label
    Compose f g -> ">>>\n" ++ showFlagA (n + 2) f ++ "\n" ++ showFlagA (n + 2) g
    First f -> "First\n" ++ showFlagA (n + 2) f
    Par f g -> "***\n" ++ showFlagA (n + 2) f ++ "\n" ++ showFlagA (n + 2) g
    IntersectLL -> "IntersectLL"
    IntersectLC -> "IntersectLC"
    IntersectCC -> "IntersectCC"
    NGonVertex _ _ -> "NGonVertex"
    FillTriangle _ -> "FillTriangle"
    FillCircle _ -> "FillCircle"
    MaskDrawing mode -> "MaskDrawing " ++ show mode
    OverlaySVG p -> "OverlaySVG " ++ show p
    Group label f -> "Group " ++ show label ++ "\n" ++ showFlagA (n + 2) f
    LabelPoint name -> "LabelPoint " ++ show name
  where
    indent i = replicate i ' '

-- ---------------------------------------------------------------------------
-- Number extraction
-- ---------------------------------------------------------------------------

-- | Collect all 'Number' values from a 'Drawing' (coordinates and radii).
drawingNumbers :: Drawing -> [Number]
drawingNumbers EmptyDrawing = []
drawingNumbers (DrawTriangle _ (x1, y1) (x2, y2) (x3, y3)) =
  [x1, y1, x2, y2, x3, y3]
drawingNumbers (DrawPath _ pts) =
  concatMap (\(x, y) -> [x, y]) pts
drawingNumbers (DrawCircle _ (cx, cy) r) =
  [cx, cy, r]
drawingNumbers (DrawMasked _ a b) =
  drawingNumbers a ++ drawingNumbers b
drawingNumbers (DrawSVGOverlay _ (cx, cy) (ex, ey)) =
  [cx, cy, ex, ey]
drawingNumbers (Overlay a b) =
  drawingNumbers a ++ drawingNumbers b
