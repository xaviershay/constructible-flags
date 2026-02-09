{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module FlagConstruction
    ( -- * Core types
      FlagA(..)
    , Point
    , Drawing(..)

      -- * Geometric primitives
    , intersectLL
    , intersectLC
    , intersectCC

      -- * Drawing primitives
    , fillTriangle
    , fillCircle

      -- * Grouping
    , group

      -- * Composite constructors
    , perpendicular
    , parallel
    , naturalMult
    , rationalMult
    , midpoint
    , quad
    , boxNatural
    , fillRectangle
    , fillBox

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

      -- * Optimization
    , optimize
    ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Colour
import Data.Colour.SRGB (sRGB24, toSRGB, channelRed, channelGreen, channelBlue)
import qualified Prelude

-- | A 2D point
type Point = (Double, Double)

-- | An abstract drawing instruction
data Drawing
  = DrawTriangle (Colour Double) Point Point Point
  | DrawPath (Colour Double) [Point]  -- ^ A closed polygon as an ordered list of vertices
  | DrawCircle (Colour Double) Point Double  -- ^ A filled circle: colour, center, radius
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
  FillTriangle _  -> "FillTriangle"
  FillCircle _    -> "FillCircle"
  Group label f   -> "Group " ++ show label ++ "\n" ++ showFlagA (n+2) f
  where
    indent i = replicate i ' '

-- ---------------------------------------------------------------------------
-- Smart constructors (for use in proc blocks)
-- ---------------------------------------------------------------------------

-- | Intersect two lines, each defined by two points
intersectLL :: FlagA ((Point, Point), (Point, Point)) Point
intersectLL = IntersectLL

-- | Intersect a line (defined by two points) with a circle (center, edge point)
intersectLC :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
intersectLC = IntersectLC

-- | Intersect two circles, each defined by (center, edge point)
intersectCC :: FlagA ((Point, Point), (Point, Point)) (Point, Point)
intersectCC = IntersectCC

fillTriangle :: Colour Double -> FlagA (Point, Point, Point) Drawing
fillTriangle = FillTriangle

-- | Fill a circle defined by its center and an edge point
fillCircle :: Colour Double -> FlagA (Point, Point) Drawing
fillCircle = FillCircle

-- | Group a sub-computation under a label for documentation / debugging
group :: String -> FlagA a b -> FlagA a b
group = Group

-- ---------------------------------------------------------------------------
-- Interpreters
-- ---------------------------------------------------------------------------

-- | A labeled construction step for introspection
data Step
  = StepIntersectLL
  | StepIntersectLC
  | StepIntersectCC
  | StepFillTriangle
  | StepFillCircle
  deriving (Show, Eq)

-- | Extract the flat list of geometric construction steps, in order.
-- Ignores structural wiring ('Arr', 'First', etc.) and only reports
-- meaningful geometric operations.
steps :: FlagA a b -> [Step]
steps (Arr _ _)        = []
steps (Compose f g)    = steps f ++ steps g
steps (First f)        = steps f
steps (Par f g)        = steps f ++ steps g
steps IntersectLL      = [StepIntersectLL]
steps IntersectLC      = [StepIntersectLC]
steps IntersectCC      = [StepIntersectCC]
steps (FillTriangle _) = [StepFillTriangle]
steps (FillCircle _)   = [StepFillCircle]
steps (Group _ f)      = steps f

-- | Evaluate a construction arrow to produce a concrete function.
-- This is one possible interpreter; others could generate SVG, trace
-- steps, validate, etc.
eval :: FlagA a b -> a -> b
eval (Arr _ f)        = f
eval (Compose f g)    = eval g . eval f
eval (First f)        = \(a, c) -> (eval f a, c)
eval (Par f g)        = \(a, c) -> (eval f a, eval g c)
eval IntersectLL      = evalIntersectLL'
eval IntersectLC      = evalIntersectLC'
eval IntersectCC      = evalIntersectCC'
eval (FillTriangle c) = \(p1, p2, p3) -> DrawTriangle c p1 p2 p3
eval (FillCircle c)   = \(center, edge) -> DrawCircle c center (dist center edge)
eval (Group _ f)      = eval f

-- | Line-line intersection from defining points
evalIntersectLL' :: ((Point, Point), (Point, Point)) -> Point
evalIntersectLL' ((p1, p2), (p3, p4)) =
  let (x1, y1) = p1
      (x2, y2) = p2
      (x3, y3) = p3
      (x4, y4) = p4
      denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
      t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
  in (x1 + t * (x2 - x1), y1 + t * (y2 - y1))

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
evalTree (FillTriangle col) (p1, p2, p3) =
    (DrawTriangle col p1 p2 p3, [TreeLayer (LayerTriangle col p1 p2 p3)])
evalTree (FillCircle col) (center, edge) =
    (DrawCircle col center (dist center edge), [TreeLayer (LayerCircle col center edge)])
evalTree (Group label f) x =
    let (res, children) = evalTree f x
    in  (res, [TreeGroup label children])

-- | Flatten a construction tree back to a list of layers (for rendering).
flattenTree :: ConstructionTree -> [ConstructionLayer]
flattenTree (TreeLayer l)       = [l]
flattenTree (TreeGroup _ children) = concatMap flattenTree children

-- ---------------------------------------------------------------------------
-- Optimization: merge same-colour triangles into paths
-- ---------------------------------------------------------------------------

-- | Optimize a 'Drawing' by merging adjacent same-colour triangles that
-- share an edge into a single 'DrawPath'.  This reduces the number of SVG
-- elements and eliminates visible seams between triangles.
optimize :: Drawing -> Drawing
optimize = buildDrawing . mergeAll . flatten
  where
    -- Flatten Overlay tree into a list preserving order
    flatten :: Drawing -> [Drawing]
    flatten EmptyDrawing  = []
    flatten (Overlay a b) = flatten a ++ flatten b
    flatten d             = [d]

    -- Rebuild a Drawing from a list
    buildDrawing :: [Drawing] -> Drawing
    buildDrawing = foldr (<>) EmptyDrawing

    -- Repeatedly try to merge adjacent same-colour primitives
    mergeAll :: [Drawing] -> [Drawing]
    mergeAll ds =
      let ds' = mergePass ds
      in  if length ds' == length ds then ds' else mergeAll ds'

    -- One pass: try to merge each element with the next
    mergePass :: [Drawing] -> [Drawing]
    mergePass [] = []
    mergePass [x] = [x]
    mergePass (x:y:rest) = case tryMerge x y of
      Just merged -> mergePass (merged : rest)
      Nothing     -> x : mergePass (y : rest)

    -- Try to merge two same-colour primitives that share an edge
    tryMerge :: Drawing -> Drawing -> Maybe Drawing
    tryMerge a b = do
      (colA, ptsA) <- toVertices a
      (colB, ptsB) <- toVertices b
      if colourEq colA colB
        then mergePolygons colA ptsA ptsB
        else Nothing

    -- Extract colour and vertex list from a triangle or path
    toVertices :: Drawing -> Maybe (Colour Double, [Point])
    toVertices (DrawTriangle c p1 p2 p3) = Just (c, [p1, p2, p3])
    toVertices (DrawPath c pts)          = Just (c, pts)
    toVertices _                         = Nothing  -- DrawCircle etc. cannot merge

    -- Compare colours by their sRGB channel values
    colourEq :: Colour Double -> Colour Double -> Bool
    colourEq c1 c2 =
      let (r1, g1, b1) = colourToTriple c1
          (r2, g2, b2) = colourToTriple c2
      in  r1 == r2 && g1 == g2 && b1 == b2

    colourToTriple :: Colour Double -> (Double, Double, Double)
    colourToTriple c =
      let rgb = toSRGB c
      in  (channelRed rgb, channelGreen rgb, channelBlue rgb)

    -- Try to merge two polygons that share an edge.
    -- Finds a shared edge (consecutive vertices appearing in both polygons
    -- in reverse order) and splices the two vertex lists together.
    mergePolygons :: Colour Double -> [Point] -> [Point] -> Maybe Drawing
    mergePolygons col ptsA ptsB = tryEdges (edges ptsA) ptsA ptsB
      where
        tryEdges [] _ _ = Nothing
        tryEdges ((i, pA1, pA2):es) as bs =
          case findSharedEdge pA1 pA2 bs of
            Just j  -> Just (DrawPath col (splicePolygons as i bs j))
            Nothing -> tryEdges es as bs

    -- All edges of a polygon as (index, point, nextPoint)
    edges :: [Point] -> [(Int, Point, Point)]
    edges pts = [ (i, pts !! i, pts !! ((i + 1) `mod` length pts))
                | i <- [0 .. length pts - 1] ]

    -- Find edge (p2, p1) — reversed — in polygon, returning its start index
    findSharedEdge :: Point -> Point -> [Point] -> Maybe Int
    findSharedEdge p1 p2 pts =
      let n = length pts
          matches = [ j | j <- [0 .. n - 1]
                        , ptEq (pts !! j) p2
                        , ptEq (pts !! ((j + 1) `mod` n)) p1 ]
      in  case matches of
            (j:_) -> Just j
            []    -> Nothing

    ptEq :: Point -> Point -> Bool
    ptEq (x1, y1) (x2, y2) = abs (x1 - x2) < 1e-10 && abs (y1 - y2) < 1e-10

    -- Splice two polygons along a shared edge.
    -- Edge i..(i+1) in A is shared with edge j..(j+1) in B (reversed).
    -- Result: walk A up to i, then insert B's non-shared vertices, then
    -- continue A after i+1.
    splicePolygons :: [Point] -> Int -> [Point] -> Int -> [Point]
    splicePolygons as ai bs bj =
      let na = length as
          nb = length bs
          -- A vertices: keep all except skip edge endpoint at (ai+1)
          aBefore = take (ai + 1) as
          aAfter  = drop (ai + 2) as ++ take ai as  -- wrap around, skip shared
          -- B vertices: start after the shared edge, walk around
          bInsert = [ bs !! ((bj + 2 + k) `mod` nb) | k <- [0 .. nb - 3] ]
      in  aBefore ++ bInsert ++ aAfter

-- ---------------------------------------------------------------------------
-- Example: the design from the highlighted pseudocode
-- ---------------------------------------------------------------------------

red :: Colour Double
red = sRGB24 255 0 0

blue :: Colour Double
blue = sRGB24 0 0 255

white :: Colour Double
white = sRGB24 255 255 255

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

-- | Given a line (two points) and a point, return a line parallel to the
-- given line passing through the point.  The construction forms a
-- parallelogram: circle centred at @p@ through @a@, circle centred at @b@
-- with radius @dist(b, a)@, and one of the intersections gives the fourth
-- vertex @q@ such that @(p, q)@ is parallel to @(a, b)@.
parallel :: FlagA ((Point, Point), Point) (Point, Point)
parallel = group "Parallel line" $ proc ((a, b), p) -> do
    (q, _) <- intersectCC -< ((p, a), (b, a))
    returnA -< (p, q)

-- | Given three points @(a, b, c)@, return the fourth point @d@ at the
-- intersection of the line perpendicular to @(a, b)@ through @b@ and the
-- line perpendicular to @(a, c)@ through @c@.
--
-- Construction: for each endpoint, reflect the opposite point through it
-- (circle-line intersection) to get a symmetric pair, then use
-- circle-circle intersection on that pair to obtain two points on the
-- perpendicular.  Finally intersect the two perpendicular lines.
quad :: FlagA (Point, Point, Point) Point
quad = group "Quad" $ proc (a, b, c) -> do
    -- Perpendicular to (a,b) through b:
    -- reflect a through b → a'
    (_, a') <- intersectLC -< ((a, b), (b, a))
    -- perpendicular bisector of (a, a') passes through b
    (e, f)  <- intersectCC -< ((a, a'), (a', a))

    -- Perpendicular to (a,c) through c:
    -- reflect a through c → a''
    (_, a'') <- intersectLC -< ((a, c), (c, a))
    -- perpendicular bisector of (a, a'') passes through c
    (g, h)   <- intersectCC -< ((a, a''), (a'', a))

    -- Intersect the two perpendicular lines
    d <- intersectLL -< ((e, f), (g, h))
    returnA -< d

-- | Given a segment @(a, b)@ representing one unit, return the point @n@
-- units from @a@ in the direction of @b@, using the classic straightedge-
-- and-compass method of stepping off unit distances along the line.
naturalMult :: Int -> FlagA (Point, Point) Point
naturalMult n
  | n <= 0    = group ("×" ++ show n) $ Arr "fst" fst
  | n == 1    = group "×1" $ Arr "snd" snd
  | otherwise = group ("×" ++ show n) $ proc (a, b) -> do
      (_, p) <- intersectLC -< ((a, b), (b, a))
      markOff (n - 2) -< (a, b, p, b)

  where
    -- | Mark off additional unit distances along line @(a, b)@.
    -- Takes @(a, b, current, previous)@ where @current@ and @previous@ are
    -- consecutive points one unit apart on the line.  Marks off @remaining@
    -- more units and returns the final point.
    markOff :: Int -> FlagA (Point, Point, Point, Point) Point
    markOff 0 = Arr "result" (\(_, _, cur, _) -> cur)
    markOff k = proc (a, b, cur, prev) -> do
        (_, next) <- intersectLC -< ((a, b), (cur, prev))
        markOff (k - 1) -< (a, b, next, cur)

-- | Construct the midpoint of a segment @(a, b)@ using compass and
-- straightedge.  Uses circle-circle intersection on circles of equal
-- radius centred at each endpoint, then intersects that perpendicular
-- bisector with the original line.
midpoint :: FlagA (Point, Point) Point
midpoint = group "Midpoint" $ proc (a, b) -> do
    -- Two circles of equal radius (|ab|) centered at a and b
    (p, q) <- intersectCC -< ((a, b), (b, a))
    -- The line (p, q) is the perpendicular bisector; intersect with (a, b)
    m <- intersectLL -< ((p, q), (a, b))
    returnA -< m

-- | Given a segment @(a, b)@ representing one unit, return the point
-- @p/q@ units from @a@ in the direction of @b@, using the classic
-- Thales' theorem construction.  Marks @q@ units on an auxiliary line,
-- connects the @q@th mark to @b@, then constructs a parallel through
-- the @p@th mark to find the desired point on @(a, b)@.
rationalMult :: Int -> Int -> FlagA (Point, Point) Point
rationalMult p q
  | p == q    = group (show p ++ "/" ++ show q) $ Arr "snd" snd
  | otherwise = group (show p ++ "/" ++ show q) $ proc (a, b) -> do
      -- Get a perpendicular direction for the auxiliary line
      (c, _) <- perpendicular -< (a, b)
      -- Mark q units along auxiliary line (a, c)
      qPt <- naturalMult q -< (a, c)
      -- Mark p units along auxiliary line (a, c)
      pPt <- naturalMult p -< (a, c)
      -- Connect qPt to b
      -- Construct a parallel to (qPt, b) through pPt
      (_, target) <- parallel -< ((qPt, b), pPt)
      -- Intersect that parallel with the original line (a, b)
      result <- intersectLL -< ((pPt, target), (a, b))
      returnA -< result

boxNatural :: Int -> Int -> FlagA (Point, Point) (Point, Point, Point, Point)
boxNatural w h = proc (tl, b) -> do
    tr <- naturalMult w -< (tl, b)
    (c, _) <- perpendicular -< (tl, b)
    bl <- naturalMult h -< (tl, c)
    br <- quad -< (tl, tr, bl)

    returnA -< (tl, tr, br, bl)

fillBox :: Colour Double -> Int -> Int -> FlagA (Point, Point) Drawing
fillBox c w h = proc (tl, b) -> do
    (tl, tr, br, bl) <- boxNatural w h -< (tl, b)

    fillRectangle c -< (tl, tr, br, bl)