{-# LANGUAGE Arrows #-}

module Flag.Constructions
    ( -- * Geometric primitives
      intersectLL
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
    , horizontalStripes
    ) where

import Control.Arrow (returnA)
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

-- | Given a line (two points) and a point, return a line parallel to the
-- given line passing through the point.  The construction forms a
-- parallelogram @a-b-q-p@ where @p→q@ is parallel to @a→b@.
--
-- Method: the diagonals of a parallelogram bisect each other, so
-- @midpoint(b, p) = midpoint(a, q)@.  We find @M = midpoint(b, p)@,
-- then reflect @a@ through @M@ to obtain @q = 2M − a = p + (b − a)@.
parallel :: FlagA ((Point, Point), Point) (Point, Point)
parallel = group "Parallel line" $ proc ((a, b), p) -> do
    m <- midpoint -< (b, p)
    -- Reflect a through m: intersect line (a, m) with circle at m
    -- through a; the far intersection is the reflection.
    (_, q) <- intersectLC -< ((a, m), (m, a))
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
        (_, newUnitRef) <- parallel -< ((tl, unitRef), bl)
        restD          <- drawStripes rest -< (bl, br, newUnitRef)
        returnA -< stripe <> restD
