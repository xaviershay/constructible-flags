{-# LANGUAGE Arrows #-}

module Flag.Constructions
    ( -- * Geometric primitives
      intersectLL
    , intersectLC
    , intersectCC

      -- * Drawing primitives
    , fillTriangle
    , fillCircle
    , fillFiveStar
    , fillStar7x2
    , fillStar7x3
    , ngonVertex

      -- * Grouping
    , group

      -- * Composite constructors
    , perpendicular
    , translate
    , bisectAngle
    , parallel
    , naturalMult
    , rationalMult
    , midpoint
    , quad
    , boxNatural
    , fillRectangle
    , fillBox
    , horizontalStripes
    ) where

import Control.Arrow (returnA, arr)
import Data.Colour

import Flag.Construction.Types

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

-- | Inscribe a regular five-pointed star (pentagram) in the given circle
-- (centre, edge point) and fill it with the given colour.
fillFiveStar :: Colour Double -> FlagA (Point, Point) Drawing
fillFiveStar col = group "Fill five star" $ proc (o, a) -> do
    -- Opposite point on the circle (diametrically opposite a)
    (_, b)    <- intersectLC -< ((o, a), (o, a))
    m         <- midpoint -< (o, b)

    -- Circle centred at M through A intersects original
    -- circle at two helpers w and v
    (w, v)    <- intersectCC -< ((m, a), (o, a))
    -- Circles centred at w and v with radius OA give four pentagon vertices
    (p1, p2)  <- intersectCC -< ((w, a), (o, a))
    (p3, p4)  <- intersectCC -< ((v, a), (o, a))

    -- Label outer vertices (use the provided A as the fifth)
    let v0 = a
        v1 = p1
        v2 = p3
        v3 = p4
        v4 = p2

    -- Inner pentagon vertices: intersections of diagonals
    i0 <- intersectLL -< ((v0, v2), (v1, v3))
    i1 <- intersectLL -< ((v1, v3), (v2, v4))
    i2 <- intersectLL -< ((v2, v4), (v3, v0))
    i3 <- intersectLL -< ((v3, v0), (v4, v1))
    i4 <- intersectLL -< ((v4, v1), (v0, v2))

    -- Fill star as five triangles (outer vertex with two adjacent inner points)
    t0 <- fillTriangle col -< (v0, i4, i0)
    t1 <- fillTriangle col -< (v1, i0, i1)
    t2 <- fillTriangle col -< (v2, i1, i2)
    t3 <- fillTriangle col -< (v3, i2, i3)
    t4 <- fillTriangle col -< (v4, i3, i4)

    returnA -< t0 <> t1 <> t2 <> t3 <> t4

-- | Smart constructor for a single n-gon vertex.
ngonVertex :: Int -> Int -> FlagA (Point, Point) Point
ngonVertex = NGonVertex

-- | Inscribe a regular seven-pointed star {7/2} in the given circle
-- (centre, edge point) and fill it with the given colour.
--
-- Generates 7 outer vertices via NGonVertex, computes 7 inner vertices
-- by intersecting adjacent star edges, then fills 14 triangles
-- (7 outer spikes + 7 inner triangles forming the heptagonal core).
fillStar7x2 :: Colour Double -> FlagA (Point, Point) Drawing
fillStar7x2 col = group "Fill seven star" $ proc (o, a) -> do
    -- Generate 7 outer vertices of the regular heptagon
    let v0 = a
    v1 <- ngonVertex 7 1 -< (o, a)
    v2 <- ngonVertex 7 2 -< (o, a)
    v3 <- ngonVertex 7 3 -< (o, a)
    v4 <- ngonVertex 7 4 -< (o, a)
    v5 <- ngonVertex 7 5 -< (o, a)
    v6 <- ngonVertex 7 6 -< (o, a)

    -- Inner vertices: intersection of adjacent {7/2} star edges.
    -- Star edge k connects v_k to v_{(k+2) mod 7}.
    -- Inner point k is the intersection of edge k and edge (k+1).
    -- Edge k: v_k -- v_{k+2}
    -- Edge k+1: v_{k+1} -- v_{k+3}
    i0 <- intersectLL -< ((v0, v2), (v1, v3))
    i1 <- intersectLL -< ((v1, v3), (v2, v4))
    i2 <- intersectLL -< ((v2, v4), (v3, v5))
    i3 <- intersectLL -< ((v3, v5), (v4, v6))
    i4 <- intersectLL -< ((v4, v6), (v5, v0))
    i5 <- intersectLL -< ((v5, v0), (v6, v1))
    i6 <- intersectLL -< ((v6, v1), (v0, v2))

    -- Fill 7 outer spike triangles (outer vertex + two nearest inner points).
    -- Vertex v_k sits on star edges (k-2 mod 7) and k.  The nearest inner
    -- points along those edges are i_{k-2 mod 7} and i_{k-1 mod 7}.
    s0 <- fillTriangle col -< (v0, i5, i6)
    s1 <- fillTriangle col -< (v1, i6, i0)
    s2 <- fillTriangle col -< (v2, i0, i1)
    s3 <- fillTriangle col -< (v3, i1, i2)
    s4 <- fillTriangle col -< (v4, i2, i3)
    s5 <- fillTriangle col -< (v5, i3, i4)
    s6 <- fillTriangle col -< (v6, i4, i5)

    -- Fill 7 inner triangles forming the heptagonal core
    -- Fan from i0 to all other inner points
    c0 <- fillTriangle col -< (i0, i1, i2)
    c1 <- fillTriangle col -< (i0, i2, i3)
    c2 <- fillTriangle col -< (i0, i3, i4)
    c3 <- fillTriangle col -< (i0, i4, i5)
    c4 <- fillTriangle col -< (i0, i5, i6)

    returnA -< s0 <> s1 <> s2 <> s3 <> s4 <> s5 <> s6
            <> c0 <> c1 <> c2 <> c3 <> c4

-- | Inscribe a regular seven-pointed star {7/3} in the given circle
-- (centre, edge point) and fill it with the given colour.
--
-- Generates 7 outer vertices via NGonVertex, computes 7 inner vertices
-- by intersecting adjacent star edges, then fills 14 triangles
-- (7 outer spikes + 7 inner triangles forming the heptagonal core).
fillStar7x3 :: Colour Double -> FlagA (Point, Point) Drawing
fillStar7x3 col = group "Fill seven star" $ proc (o, a) -> do
    -- Generate 7 outer vertices of the regular heptagon
    let v0 = a
    v1 <- ngonVertex 7 1 -< (o, a)
    v2 <- ngonVertex 7 2 -< (o, a)
    v3 <- ngonVertex 7 3 -< (o, a)
    v4 <- ngonVertex 7 4 -< (o, a)
    v5 <- ngonVertex 7 5 -< (o, a)
    v6 <- ngonVertex 7 6 -< (o, a)

    -- Inner vertices: intersection of adjacent {7/2} star edges.
    -- Star edge k connects v_k to v_{(k+2) mod 7}.
    -- Inner point k is the intersection of edge k and edge (k+1).
    -- Edge k: v_k -- v_{k+2}
    -- Edge k+1: v_{k+1} -- v_{k+3}
    i0 <- intersectLL -< ((v0, v3), (v1, v4))
    i1 <- intersectLL -< ((v1, v4), (v2, v5))
    i2 <- intersectLL -< ((v2, v5), (v3, v6))
    i3 <- intersectLL -< ((v3, v6), (v4, v0))
    i4 <- intersectLL -< ((v4, v0), (v5, v1))
    i5 <- intersectLL -< ((v5, v1), (v6, v2))
    i6 <- intersectLL -< ((v6, v2), (v0, v3))

    -- Fill 7 outer spike triangles (outer vertex + two nearest inner points).
    -- Vertex v_k sits on star edges (k-2 mod 7) and k.  The nearest inner
    -- points along those edges are i_{k-2 mod 7} and i_{k-1 mod 7}.
    s0 <- fillTriangle col -< (v0, i5, i6)
    s1 <- fillTriangle col -< (v1, i6, i0)
    s2 <- fillTriangle col -< (v2, i0, i1)
    s3 <- fillTriangle col -< (v3, i1, i2)
    s4 <- fillTriangle col -< (v4, i2, i3)
    s5 <- fillTriangle col -< (v5, i3, i4)
    s6 <- fillTriangle col -< (v6, i4, i5)

    -- Fill 7 inner triangles forming the heptagonal core
    -- Fan from i0 to all other inner points
    c0 <- fillTriangle col -< (i0, i1, i2)
    c1 <- fillTriangle col -< (i0, i2, i3)
    c2 <- fillTriangle col -< (i0, i3, i4)
    c3 <- fillTriangle col -< (i0, i4, i5)
    c4 <- fillTriangle col -< (i0, i5, i6)

    returnA -< s0 <> s1 <> s2 <> s3 <> s4 <> s5 <> s6
            <> c0 <> c1 <> c2 <> c3 <> c4

-- | Fill a circle defined by its center and an edge point
fillCircle :: Colour Double -> FlagA (Point, Point) Drawing
fillCircle = FillCircle

-- | Group a sub-computation under a label for documentation / debugging
group :: String -> FlagA a b -> FlagA a b
group = Group

-- ---------------------------------------------------------------------------
-- Composite constructions
-- ---------------------------------------------------------------------------

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

-- | Given an angle with vertex @o@ and arm points @a@ and @b@ (i.e. rays
-- @o→a@ and @o→b@), construct the internal angle bisector. Returns a
-- pair @(o, p)@ where the line through @o@ and @p@ bisects the angle.
bisectAngle :: FlagA (Point, (Point, Point)) (Point, Point)
bisectAngle = group "Angle bisector" $ proc (o, (a, b)) -> do
    -- point on ray o->b at same distance from o as a
    (b1, b2) <- intersectLC -< ((o, b), (o, a))
    let b' = b2
    -- intersections of circles centred at a and b' with radius |a - o|
    (s1, _s2) <- intersectCC -< ((a, o), (b', o))
    returnA -< (o, s1)

-- | Translate a vector defined by @(a, b)@ so that its origin is at point
-- @p@. Returns the pair @(p, q)@ where @q = p + (b - a)@.
--
-- The construction forms a parallelogram @a-b-q-p@ where @p→q@ is
-- parallel to @a→b@. Method: the diagonals of a parallelogram bisect
-- each other, so @midpoint(b, p) = midpoint(a, q)@. We find
-- @M = midpoint(b, p)@, then reflect @a@ through @M@ to obtain
-- @q = 2M − a = p + (b − a)@.
translate :: FlagA ((Point, Point), Point) (Point, Point)
translate = group "Translate vector" $ proc ((a, b), p) -> do
  m <- midpoint -< (b, p)
  -- Degenerate case: midpoint(b,p) == a would pass a zero-length line to
  -- IntersectLC.  Detect it here and raise an explicit (unimplemented)
  -- error so callers / tests observe the limitation instead of getting
  -- a lower-level division-by-zero.
  -- evaluate check inside an `arr` that receives `m` and `a` so the
  -- ArrowProc-bound values are visible to the pure function
  _ <- arr (\(m', a') -> if m' == a'
                          then error "translate: degenerate midpoint (midpoint == a) - unimplemented"
                          else ()) -< (m, a)

  (_, q) <- intersectLC -< ((a, m), (m, a))
  returnA -< (p, q)

-- | Given a line (two points) and a point, return a line parallel to the
-- given line passing through the point.
parallel :: FlagA ((Point, Point), Point) (Point, Point)
parallel = group "Parallel line" translate

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
-- units from @a@ in the direction of @b@.
--
-- Ref: https://blog.xaviershay.com/articles/optimal-construction-of-integers-with-straightedge-and-compass.html
naturalMult :: Int -> FlagA (Point, Point) Point
naturalMult n
  | n <= 0    = Arr "fst" fst
  | n == 1    = Arr "snd" snd
  | otherwise = group ("×" ++ show n) $ proc (a, b) -> do
      applySteps (computeSteps n) -< (a, b, b)

  where
    -- | Compute the reconstruction step sequence for @k@.
    -- Reduces @k@ to 1 by halving, accumulating @False@ (exact halve, i.e.
    -- @k@ was even) or @True@ (rounded-up halve, i.e. @k@ was odd) at each
    -- stage.  The list is ordered so that applying the steps left-to-right
    -- starting from @b@ (= position 1) reconstructs position @k@.
    computeSteps :: Int -> [Bool]
    computeSteps 1 = []
    computeSteps k
      | even k    = computeSteps (k `div` 2) ++ [False]
      | otherwise = computeSteps ((k + 1) `div` 2) ++ [True]

    -- | Apply reconstruction steps to the current point @p@, threading
    -- the origin @a@ and unit point @b@ through unchanged.
    applySteps :: [Bool] -> FlagA (Point, Point, Point) Point
    applySteps []           = Arr "result" (\(_, _, p) -> p)
    applySteps (False:rest) = proc (a, b, p) -> do
        -- Double: construct 2p − a
        (_, next) <- intersectLC -< ((a, p), (p, a))
        applySteps rest -< (a, b, next)
    applySteps (True:rest)  = proc (a, b, p) -> do
        -- Double-minus-one: construct 2p − b
        (_, next) <- intersectLC -< ((b, p), (p, b))
        applySteps rest -< (a, b, next)

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
      qPt <- naturalMult q -< (a, c)
      pPt <- naturalMult p -< (a, c)

      (_, target) <- translate -< ((qPt, b), pPt)

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
    (tl', tr, br, bl) <- boxNatural w h -< (tl, b)

    fillRectangle c -< (tl', tr, br, bl)


-- | Create horizontal stripes across the flag area.  The first
-- parameter is the horizontal width in units, and the list contains
-- pairs of @(height, colour)@ where each height is in the same unit.
-- The total flag height is the sum of all stripe heights.
--
-- Computes the full-width top-right once, then threads @(tl, tr, unitRef)@
-- through the iteration so that each stripe reuses @bl@ and @br@ from
-- 'quad' as the next row's @tl@ and @tr@, avoiding a redundant
-- @naturalMult width@ per stripe.
horizontalStripes :: Int -> [(Int, Colour Double)] -> FlagA (Point, Point) Drawing
horizontalStripes width specs = group "Horizontal stripes" $ proc (tl, b) -> do
    tr <- naturalMult width -< (tl, b)
    drawStripes specs -< (tl, tr, b)
  where
    drawStripes :: [(Int, Colour Double)] -> FlagA (Point, Point, Point) Drawing
    drawStripes [] = Arr "empty" (\_ -> mempty)
    -- Special case to optimise final row. Not necessary for correctness, but
    -- avoids calculating newUnitRef.
    drawStripes [(h, col)] = proc (tl, tr, unitRef) -> do
        (down, _) <- perpendicular -< (tl, unitRef)
        bl        <- naturalMult h -< (tl, down)
        br        <- quad -< (tl, tr, bl)
        fillRectangle col -< (tl, tr, br, bl)
    drawStripes ((h, col):rest) = proc (tl, tr, unitRef) -> do
        (down, _)      <- perpendicular -< (tl, unitRef)
        bl             <- naturalMult h -< (tl, down)
        br             <- quad -< (tl, tr, bl)
        stripe         <- fillRectangle col -< (tl, tr, br, bl)
        (_, newUnitRef) <- translate -< ((tl, unitRef), bl)
        restD          <- drawStripes rest -< (bl, br, newUnitRef)
        returnA -< stripe <> restD
