{-# LANGUAGE Arrows #-}

module Flag.Constructions
  ( -- * Geometric primitives
    intersectLL,
    intersectLC,
    intersectCC,

    -- * Drawing primitives
    fillTriangle,
    fillCircle,
    fillCrescent,
    maskDrawing,
    clipDrawing,
    fillStar7x2,
    fillStar7x3,
    fillStar5,
    fillStar5Inner,
    fillStar7Inner,
    fillStar12InnerC,
    fillStar16InnerC,
    ngonVertex,

    -- * SVG overlay
    overlaySVG,

    -- * Grouping
    group,
    label,
    labelFirst,
    labelSecond,
    labelPair,

    -- * Composite constructors
    perpendicular,
    translate,
    bisectAngle,
    parallel,
    naturalMult,
    rationalMult,
    midpoint,
    quad,
    boxNatural,
    fillRectangle,
    fillBox,
    horizontalStripes,
  )
where

import Control.Arrow (arr, first, returnA, second, (***), (>>>))
import Data.Colour
import Data.Ratio (Ratio, denominator, numerator)
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
fillStar7x2 col = group "Fill {7/2} star" $ proc (o, a) -> do
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

  returnA
    -<
      s0
        <> s1
        <> s2
        <> s3
        <> s4
        <> s5
        <> s6
        <> c0
        <> c1
        <> c2
        <> c3
        <> c4

-- | Inscribe a regular seven-pointed star {7/3} in the given circle
-- (centre, edge point) and fill it with the given colour.
--
-- Generates 7 outer vertices via NGonVertex, computes 7 inner vertices
-- by intersecting adjacent star edges, then fills 14 triangles
-- (7 outer spikes + 7 inner triangles forming the heptagonal core).
fillStar7x3 :: Colour Double -> FlagA (Point, Point) Drawing
fillStar7x3 col = group "Fill {7/3} star" $ proc (o, a) -> do
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
  i0 <- intersectLL -< ((v0, v3), (v1, v5))
  i1 <- intersectLL -< ((v1, v4), (v2, v6))
  i2 <- intersectLL -< ((v2, v5), (v3, v0))
  i3 <- intersectLL -< ((v3, v6), (v4, v1))
  i4 <- intersectLL -< ((v4, v0), (v5, v2))
  i5 <- intersectLL -< ((v5, v1), (v6, v3))
  i6 <- intersectLL -< ((v6, v2), (v0, v4))

  -- Fill 7 outer spike triangles (outer vertex + two nearest inner points).
  -- Vertex v_k sits on star edges (k-2 mod 7) and k.  The nearest inner
  -- points along those edges are i_{k-2 mod 7} and i_{k-1 mod 7}.
  s0 <- fillTriangle col -< (v0, i6, i0)
  s1 <- fillTriangle col -< (v1, i0, i1)
  s2 <- fillTriangle col -< (v2, i1, i2)
  s3 <- fillTriangle col -< (v3, i2, i3)
  s4 <- fillTriangle col -< (v4, i3, i4)
  s5 <- fillTriangle col -< (v5, i4, i5)
  s6 <- fillTriangle col -< (v6, i5, i6)

  -- Fill 7 inner triangles forming the heptagonal core
  -- Fan from i0 to all other inner points
  c0 <- fillTriangle col -< (i0, i1, i2)
  c1 <- fillTriangle col -< (i0, i2, i3)
  c2 <- fillTriangle col -< (i0, i3, i4)
  c3 <- fillTriangle col -< (i0, i4, i5)
  c4 <- fillTriangle col -< (i0, i5, i6)

  returnA
    -<
      s0
        <> s1
        <> s2
        <> s3
        <> s4
        <> s5
        <> s6
        <> c0
        <> c1
        <> c2
        <> c3
        <> c4

-- | Inscribe a simple seven-pointed star in the given circle
-- (centre, edge point) and fill it with the given colour.
--
-- The outer vertices are the same 7 heptagon points as 'fillStar7x3'.
-- The inner vertices sit on a concentric circle of radius @scale * R@,
-- rotated by π/7 (half a heptagon step) so they fall between adjacent
-- outer vertices.  Adjacent outer and inner vertices are connected in
-- sequence to form 7 spike triangles; the inner heptagon is then filled
-- with a 5-triangle fan.
fillStar7Inner :: Ratio Int -> Colour Double -> FlagA (Point, Point) Drawing
fillStar7Inner scale col = group "Fill 7-point inner star" $ proc (o, a) -> do
  -- Generate 7 outer vertices of the regular heptagon
  let v0 = a
  v1 <- ngonVertex 7 1 -< (o, a)
  v2 <- ngonVertex 7 2 -< (o, a)
  v3 <- ngonVertex 7 3 -< (o, a)
  v4 <- ngonVertex 7 4 -< (o, a)
  v5 <- ngonVertex 7 5 -< (o, a)
  v6 <- ngonVertex 7 6 -< (o, a)

  -- Invert (o, a) to get the antipodal edge point on the outer circle (at angle π).
  -- This places the inner vertices at the half-step offsets between outer vertices.
  (opp, _) <- intersectLC -< ((o, a), (o, a))
  -- Scale to the inner circle radius
  innerEdge <- rationalMult scale -< (o, opp)

  -- Generate 7 inner vertices on the inner circle, evenly spaced
  -- starting from innerEdge (at angle π, between v3 and v4).
  -- Inner vertex k sits between outer vertices (k+3) and (k+4) mod 7.
  let i0 = innerEdge
  i1 <- ngonVertex 7 1 -< (o, innerEdge)
  i2 <- ngonVertex 7 2 -< (o, innerEdge)
  i3 <- ngonVertex 7 3 -< (o, innerEdge)
  i4 <- ngonVertex 7 4 -< (o, innerEdge)
  i5 <- ngonVertex 7 5 -< (o, innerEdge)
  i6 <- ngonVertex 7 6 -< (o, innerEdge)

  -- Fill 7 spike triangles: each outer vertex with the two flanking inner points.
  -- i0 sits between v3 and v4, so going around: vj is flanked CW by i_{j+3} and CCW by i_{j+4} (mod 7).
  s0 <- fillTriangle col -< (v0, i3, i4)
  s1 <- fillTriangle col -< (v1, i4, i5)
  s2 <- fillTriangle col -< (v2, i5, i6)
  s3 <- fillTriangle col -< (v3, i6, i0)
  s4 <- fillTriangle col -< (v4, i0, i1)
  s5 <- fillTriangle col -< (v5, i1, i2)
  s6 <- fillTriangle col -< (v6, i2, i3)

  -- Fill inner heptagonal core with a fan from i0
  c0 <- fillTriangle col -< (i0, i1, i2)
  c1 <- fillTriangle col -< (i0, i2, i3)
  c2 <- fillTriangle col -< (i0, i3, i4)
  c3 <- fillTriangle col -< (i0, i4, i5)
  c4 <- fillTriangle col -< (i0, i5, i6)

  returnA
    -<
      s0
        <> s1
        <> s2
        <> s3
        <> s4
        <> s5
        <> s6
        <> c0
        <> c1
        <> c2
        <> c3
        <> c4

-- | Inscribe a pentagram {5/2} in the given circle (centre, edge point)
-- and fill it with the given colour.
--
-- Uses the same straightedge & compass pentagon construction as
-- 'fillStar5Inner' to get the 5 outer vertices, then finds the 5 inner
-- pentagon vertices by intersecting adjacent star edges (edge k connects
-- v_k to v_{k+2 mod 5}; inner vertex i_k = edge k ∩ edge (k+1 mod 5)).
-- Fills 5 spike triangles plus a 3-triangle fan for the pentagonal core.
fillStar5 :: Colour Double -> FlagA (Point, Point) Drawing
fillStar5 col = group "Fill {5/2} star" $ proc (o, a) -> do
  -- === Outer pentagon via straightedge & compass ===
  (q, _) <- perpendicular -< (o, a)

  mq <- midpoint -< (o, q)
  (n1, n2) <- intersectLC -< ((o, q), (mq, a))

  (_, f1) <- intersectLC -< ((o, a), (o, n1))
  g1 <- midpoint -< (o, f1)

  (f2, _) <- intersectLC -< ((o, a), (o, n2))
  g2 <- midpoint -< (o, f2)

  (_, g1q) <- translate -< ((o, q), g1)
  (_, g2q) <- translate -< ((o, q), g2)

  let v0 = a
  (v1, v4) <- intersectLC -< ((g1, g1q), (o, a)) -- 72° and 288°
  (v2, v3) <- intersectLC -< ((g2, g2q), (o, a)) -- 144° and 216°

  -- === Inner pentagon via star edge intersections ===
  -- Edge k: v_k → v_{k+2 mod 5}.  i_k = edge k ∩ edge (k+1 mod 5).
  i0 <- intersectLL -< ((v0, v2), (v1, v3))
  i1 <- intersectLL -< ((v1, v3), (v2, v4))
  i2 <- intersectLL -< ((v2, v4), (v3, v0))
  i3 <- intersectLL -< ((v3, v0), (v4, v1))
  i4 <- intersectLL -< ((v4, v1), (v0, v2))

  -- === Fill triangles ===
  -- 5 spike triangles: v_k flanked by i_{k-2 mod 5} and i_{k-1 mod 5}
  s0 <- fillTriangle col -< (v0, i3, i4)
  s1 <- fillTriangle col -< (v1, i4, i0)
  s2 <- fillTriangle col -< (v2, i0, i1)
  s3 <- fillTriangle col -< (v3, i1, i2)
  s4 <- fillTriangle col -< (v4, i2, i3)

  -- Inner pentagonal core: fan from i0
  c0 <- fillTriangle col -< (i0, i1, i2)
  c1 <- fillTriangle col -< (i0, i2, i3)
  c2 <- fillTriangle col -< (i0, i3, i4)

  returnA
    -<
      s0
        <> s1
        <> s2
        <> s3
        <> s4
        <> c0
        <> c1
        <> c2

-- | Inscribe a simple five-pointed star in the given circle
-- (centre, edge point) and fill it with the given colour.
--
-- Uses a straightedge & compass construction for the
-- regular pentagon.  The key idea: the perpendicular half-radius gives
-- a segment of length R√5/2 (from the midpoint of a perpendicular
-- radius to the starting vertex).  Intersecting this circle with the
-- perpendicular diameter yields two golden-ratio reference distances.
-- Projecting these onto the original diameter and bisecting produces
-- the x-coordinates of the pentagon vertex pairs, which are then found
-- by erecting perpendiculars and intersecting with the circle.
--
-- The inner vertices sit on a concentric circle of radius @scale * R@,
-- rotated by π so they fall between adjacent outer vertices.  The same
-- construction is applied to the inner circle.
fillStar5Inner :: Ratio Int -> Colour Double -> FlagA (Point, Point) Drawing
fillStar5Inner scale col = group "Fill 5-point inner star" $ proc (o, a) -> do
  -- === Outer pentagon via straightedge & compass ===
  -- Antipodal point and perpendicular reference on outer circle
  (b, _) <- intersectLC -< ((o, a), (o, a))
  (q, _) <- perpendicular -< (o, a)

  -- M = midpoint of perpendicular radius OQ.
  -- \|MA| = R√5/2, so circle(M, A) encodes the golden ratio.
  mq <- midpoint -< (o, q)
  (n1, n2) <- intersectLC -< ((o, q), (mq, a))
  -- n1 (smaller t): |A,n1| = pentagon side length
  -- n2 (larger t):  |A,n2| = pentagon diagonal length

  -- Project golden-ratio distances onto line OA.
  -- circle(O, n1) ∩ line(O,A) gives points at ±|On1| along OA.
  -- The second (larger t, same direction as A) is what we want.
  (_, f1) <- intersectLC -< ((o, a), (o, n1))
  g1 <- midpoint -< (o, f1)
  -- g1 is the x-coordinate of V1 and V4 (at ±72° from A)

  -- Similarly for the diagonal distance → x-coordinate of V2 and V3.
  -- The first (smaller t, opposite to A) is what we want.
  (f2, _) <- intersectLC -< ((o, a), (o, n2))
  g2 <- midpoint -< (o, f2)
  -- g2 is the x-coordinate of V2 and V3 (at ±144° from A)

  -- Erect perpendiculars at g1 and g2 (parallel to OQ)
  (_, g1q) <- translate -< ((o, q), g1)
  (_, g2q) <- translate -< ((o, q), g2)

  -- Intersect with outer circle to get pentagon vertices.
  -- intersectLC orders by t along the directed line (going in q direction):
  -- first = vertex above OA, second = vertex below OA.
  let v0 = a
  (v1, v4) <- intersectLC -< ((g1, g1q), (o, a)) -- 72° and 288°
  (v2, v3) <- intersectLC -< ((g2, g2q), (o, a)) -- 144° and 216°

  -- === Inner pentagon on scaled concentric circle ===
  let opp = b
  innerEdge <- rationalMult scale -< (o, opp)

  -- Get a perpendicular reference on the inner circle in the same
  -- direction as q (to preserve vertex ordering).
  (_, iq) <- intersectLC -< ((o, q), (o, innerEdge))

  -- Same golden-ratio construction on the inner circle
  imq <- midpoint -< (o, iq)
  (in1, in2) <- intersectLC -< ((o, iq), (imq, innerEdge))

  (_, iF1) <- intersectLC -< ((o, innerEdge), (o, in1))
  iG1 <- midpoint -< (o, iF1)

  (iF2, _) <- intersectLC -< ((o, innerEdge), (o, in2))
  iG2 <- midpoint -< (o, iF2)

  (_, iG1q) <- translate -< ((o, iq), iG1)
  (_, iG2q) <- translate -< ((o, iq), iG2)

  let i0 = innerEdge -- 180°
  (i4, i1) <- intersectLC -< ((iG1, iG1q), (o, innerEdge)) -- 108° and 252°
  (i3, i2) <- intersectLC -< ((iG2, iG2q), (o, innerEdge)) -- 36° and 324°

  -- === Fill triangles ===
  -- 5 spike triangles: vj flanked by i_{(j+2) mod 5} and i_{(j+3) mod 5}.
  s0 <- fillTriangle col -< (v0, i2, i3)
  s1 <- fillTriangle col -< (v1, i3, i4)
  s2 <- fillTriangle col -< (v2, i4, i0)
  s3 <- fillTriangle col -< (v3, i0, i1)
  s4 <- fillTriangle col -< (v4, i1, i2)

  -- Inner pentagonal core: fan from i0
  c0 <- fillTriangle col -< (i0, i1, i2)
  c1 <- fillTriangle col -< (i0, i2, i3)
  c2 <- fillTriangle col -< (i0, i3, i4)

  returnA
    -<
      s0
        <> s1
        <> s2
        <> s3
        <> s4
        <> c0
        <> c1
        <> c2

fillStar16InnerC :: Colour Double -> FlagA (Point, Point, Point) Drawing
fillStar16InnerC col = group "Fill 16-point star" $ proc (o, innerEdge, v0) -> do
  -- TODO: This logic isn't right yet
  (v4, v12, v11, v10, v9, v3, v2, v1) <- quarterPoints -< (o, v0)
  (v8, _, v7, v6, v5, v15, v14, v13) <- quarterPoints -< (o, v12)

  iv0' <- midpoint -< (v0, v1)
  (_, iv0) <- intersectLC >>> labelSecond "IV0" -< ((o, iv0'), (o, innerEdge))

  (iv4, iv12, iv11, iv10, iv9, iv3, iv2, iv1) <- quarterPoints -< (o, iv0)
  (iv8, _, iv7, iv6, iv5, iv15, iv14, iv13) <- quarterPoints -< (o, iv12)

  s1 <- fillTriangle col -< (iv0, v1, iv1)
  s2 <- fillTriangle col -< (iv1, v2, iv2)
  s3 <- fillTriangle col -< (iv2, v3, iv3)
  s4 <- fillTriangle col -< (iv3, v4, iv4)
  s5 <- fillTriangle col -< (iv4, v5, iv5)
  s6 <- fillTriangle col -< (iv5, v6, iv6)
  s7 <- fillTriangle col -< (iv6, v7, iv7)
  s8 <- fillTriangle col -< (iv7, v8, iv8)
  s9 <- fillTriangle col -< (iv8, v9, iv9)
  s10 <- fillTriangle col -< (iv9, v10, iv10)
  s11 <- fillTriangle col -< (iv10, v11, iv11)
  s12 <- fillTriangle col -< (iv11, v12, iv12)
  s13 <- fillTriangle col -< (iv12, v13, iv13)
  s14 <- fillTriangle col -< (iv13, v14, iv14)
  s15 <- fillTriangle col -< (iv14, v15, iv15)
  s16 <- fillTriangle col -< (iv15, v0, iv0)
  let s = s1 <> s2 <> s3 <> s4 <> s5 <> s6 <> s7 <> s8 <> s9 <> s10 <> s11 <> s12 <> s13 <> s14 <> s15 <> s16

  i1 <- fillTriangle col -< (o, iv0, iv1)
  i2 <- fillTriangle col -< (o, iv1, iv2)
  i3 <- fillTriangle col -< (o, iv2, iv3)
  i4 <- fillTriangle col -< (o, iv3, iv4)
  i5 <- fillTriangle col -< (o, iv4, iv5)
  i6 <- fillTriangle col -< (o, iv5, iv6)
  i7 <- fillTriangle col -< (o, iv6, iv7)
  i8 <- fillTriangle col -< (o, iv7, iv8)
  i9 <- fillTriangle col -< (o, iv8, iv9)
  i10 <- fillTriangle col -< (o, iv9, iv10)
  i11 <- fillTriangle col -< (o, iv10, iv11)
  i12 <- fillTriangle col -< (o, iv11, iv12)
  i13 <- fillTriangle col -< (o, iv12, iv13)
  i14 <- fillTriangle col -< (o, iv13, iv14)
  i15 <- fillTriangle col -< (o, iv14, iv15)
  i16 <- fillTriangle col -< (o, iv15, iv0)
  let i = i1 <> i2 <> i3 <> i4 <> i5 <> i6 <> i7 <> i8 <> i9 <> i10 <> i11 <> i12 <> i13 <> i14 <> i15 <> i16

  returnA -< s <> i
  where
    -- \| Given a center and an axis point, constructs the quarter point and the
    -- 3 intermediate points in the arc from axis to quarter, plus their antipodals.
    -- Returns (quarter, antiAxis, anti3, anti2, anti1, p3, p2, p1)
    quarterPoints = proc (ctr, axis) -> do
      (quarter, anti) <- perpendicular -< (ctr, axis)
      p2' <- midpoint -< (quarter, axis)
      p1' <- midpoint -< (p2', axis)
      p3' <- midpoint -< (p2', quarter)
      (anti3, p3) <- intersectLC -< ((ctr, p3'), (ctr, axis))
      (anti2, p2) <- intersectLC -< ((ctr, p2'), (ctr, axis))
      (anti1, p1) <- intersectLC -< ((ctr, p1'), (ctr, axis))
      returnA -< (quarter, anti, anti3, anti2, anti1, p3, p2, p1)

fillStar12InnerC :: Colour Double -> FlagA (Point, Point, Point) Drawing
fillStar12InnerC col = group "Fill 12-point inner star inner" $ proc (o, ivEdge, ovEdge) -> do
  (ov0, ov1, ov2, ov3, ov4, ov5, ov6, ov7, ov8, ov9, ov10, ov11) <- fan12 -< (o, ovEdge)
  iv0' <- midpoint -< (ov0, ov1)
  (_, iv0) <- intersectLC -< ((o, iv0'), (o, ivEdge))

  (_, iv1, iv2, iv3, iv4, iv5, iv6, iv7, iv8, iv9, iv10, iv11) <- fan12 -< (o, iv0)

  -- Spikes
  s1 <- fillTriangle col -< (iv0, ov1, iv1)
  s2 <- fillTriangle col -< (iv1, ov2, iv2)
  s3 <- fillTriangle col -< (iv2, ov3, iv3)
  s4 <- fillTriangle col -< (iv3, ov4, iv4)
  s5 <- fillTriangle col -< (iv4, ov5, iv5)
  s6 <- fillTriangle col -< (iv5, ov6, iv6)
  s7 <- fillTriangle col -< (iv6, ov7, iv7)
  s8 <- fillTriangle col -< (iv7, ov8, iv8)
  s9 <- fillTriangle col -< (iv8, ov9, iv9)
  s10 <- fillTriangle col -< (iv9, ov10, iv10)
  s11 <- fillTriangle col -< (iv10, ov11, iv11)
  s12 <- fillTriangle col -< (iv11, ov0, iv0)

  -- Inner
  i1 <- fillTriangle col -< (o, iv0, iv1)
  i2 <- fillTriangle col -< (o, iv1, iv2)
  i3 <- fillTriangle col -< (o, iv2, iv3)
  i4 <- fillTriangle col -< (o, iv3, iv4)
  i5 <- fillTriangle col -< (o, iv4, iv5)
  i6 <- fillTriangle col -< (o, iv5, iv6)
  i7 <- fillTriangle col -< (o, iv6, iv7)
  i8 <- fillTriangle col -< (o, iv7, iv8)
  i9 <- fillTriangle col -< (o, iv8, iv9)
  i10 <- fillTriangle col -< (o, iv9, iv10)
  i11 <- fillTriangle col -< (o, iv10, iv11)
  i12 <- fillTriangle col -< (o, iv11, iv0)

  let s = s1 <> s2 <> s3 <> s4 <> s5 <> s6 <> s7 <> s8 <> s9 <> s10 <> s11 <> s12
  let i = i1 <> i2 <> i3 <> i4 <> i5 <> i6 <> i7 <> i8 <> i9 <> i10 <> i11 <> i12

  returnA -< i <> s
  where
    fan = proc (o, ov0) -> do
      (ov2, ov10) <- intersectCC -< ((o, ov0), (ov0, o))
      ov1' <- midpoint -< (ov0, ov2)
      (_, ov1) <- intersectLC -< ((o, ov1'), (o, ov0))
      ov11' <- midpoint -< (ov0, ov10)
      (_, ov11) <- intersectLC -< ((o, ov11'), (o, ov0))
      returnA -< (ov10, ov11, ov0, ov1, ov2)
    fan12 = proc (o, ov0) -> do
      (ov10, ov11, _, ov1, ov2) <- fan -< (o, ov0)
      (ov6, _) <- intersectLC -< ((o, ov0), (o, ov0))
      (ov4, ov5, _, ov7, ov8) <- fan -< (o, ov6)
      (ov3, ov9) <- perpendicular -< (o, ov0)
      returnA -< (ov0, ov1, ov2, ov3, ov4, ov5, ov6, ov7, ov8, ov9, ov10, ov11)

-- | Fill a circle defined by its center and an edge point
fillCircle :: Colour Double -> FlagA (Point, Point) Drawing
fillCircle = FillCircle

-- | Mask a drawing by another drawing.
-- Shapes in the mask drawing become transparent (cut-out) areas in the content drawing.
-- Uses an SVG mask with a white background and black shapes.
maskDrawing :: FlagA (Drawing, Drawing) Drawing
maskDrawing = MaskDrawing Mask

-- | Clip a drawing by another drawing.
-- Shapes in the mask drawing define the visible area of the content drawing.
-- Everything outside the shapes is hidden. Uses an SVG clipPath.
clipDrawing :: FlagA (Drawing, Drawing) Drawing
clipDrawing = MaskDrawing Clip

-- | Fill a crescent: an outer filled disc with an inner disc cut out.
-- Takes ((outerCenter, outerEdge), (innerCenter, innerEdge)).
-- Implemented via maskDrawing: the inner circle punches a transparent hole
-- in the outer circle.
fillCrescent :: Colour Double -> FlagA ((Point, Point), (Point, Point)) Drawing
fillCrescent col = proc ((outerCenter, outerEdge), (innerCenter, innerEdge)) -> do
  outer <- fillCircle col -< (outerCenter, outerEdge)
  inner <- fillCircle col -< (innerCenter, innerEdge)
  maskDrawing -< (outer, inner)

-- | Overlay an external SVG file, scaled to fit within a bounding circle
-- defined by its center and an edge point. The SVG's native aspect ratio
-- is preserved.
overlaySVG :: FilePath -> FlagA (Point, Point) Drawing
overlaySVG = OverlaySVG

-- | Group a sub-computation under a label for documentation / debugging
group :: String -> FlagA a b -> FlagA a b
group = Group

-- | Attach a human-readable label to a point for display in the debug viewer.
-- The label travels with the point through the construction pipeline and is
-- shown as a persistent annotation in the debug viewer rather than requiring
-- a hover. This is a pure pass-through: the point value is unchanged.
label :: String -> FlagA Point Point
label = LabelPoint

-- | Label the first component of a pair, passing the second through unchanged.
-- Equivalent to @first (label name)@. Useful with arrows that return @(Point, Point)@
-- when only the first component is kept:
-- @(a, _) <- someArrow >>> labelFirst "A" -< input@
labelFirst :: String -> FlagA (Point, Point) (Point, Point)
labelFirst name = first (label name)

-- | Label the second component of a pair, passing the first through unchanged.
-- Equivalent to @second (label name)@. Useful with arrows that return @(Point, Point)@
-- when only the second component is kept:
-- @(_, b) <- someArrow >>> labelSecond "B" -< input@
labelSecond :: String -> FlagA (Point, Point) (Point, Point)
labelSecond name = second (label name)

-- | Label both components of a pair independently.
-- Equivalent to @label n1 *** label n2@.
labelPair :: String -> String -> FlagA (Point, Point) (Point, Point)
labelPair n1 n2 = label n1 *** label n2

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
  -- Both circles pass through o, so one intersection is always o.
  -- Pick the other one.
  (s1, s2) <- intersectCC -< ((a, o), (b', o))
  let s = if s1 == o then s2 else s1
  returnA -< (o, s)

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
  _ <-
    arr
      ( \(m', a') ->
          if m' == a'
            then error "translate: degenerate midpoint (midpoint == a) - unimplemented"
            else ()
      )
      -<
        (m, a)

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
  (e, f) <- intersectCC -< ((a, a'), (a', a))

  -- Perpendicular to (a,c) through c:
  -- reflect a through c → a''
  (_, a'') <- intersectLC -< ((a, c), (c, a))
  -- perpendicular bisector of (a, a'') passes through c
  (g, h) <- intersectCC -< ((a, a''), (a'', a))

  -- Intersect the two perpendicular lines
  d <- intersectLL -< ((e, f), (g, h))
  returnA -< d

-- | Given a segment @(a, b)@ representing one unit, return the point @n@
-- units from @a@ in the direction of @b@.
--
-- Ref: https://blog.xaviershay.com/articles/optimal-construction-of-integers-with-straightedge-and-compass.html
naturalMult :: Int -> FlagA (Point, Point) Point
naturalMult n
  | n <= 0 = Arr "fst" fst
  | n == 1 = Arr "snd" snd
  | otherwise = group ("×" ++ show n) $ proc (a, b) -> do
      applySteps (computeSteps n) -< (a, b, b)
  where
    -- \| Compute the reconstruction step sequence for @k@.
    -- Reduces @k@ to 1 by halving, accumulating @False@ (exact halve, i.e.
    -- @k@ was even) or @True@ (rounded-up halve, i.e. @k@ was odd) at each
    -- stage.  The list is ordered so that applying the steps left-to-right
    -- starting from @b@ (= position 1) reconstructs position @k@.
    computeSteps :: Int -> [Bool]
    computeSteps 1 = []
    computeSteps k
      | even k = computeSteps (k `div` 2) ++ [False]
      | otherwise = computeSteps ((k + 1) `div` 2) ++ [True]

    -- \| Apply reconstruction steps to the current point @p@, threading
    -- the origin @a@ and unit point @b@ through unchanged.
    applySteps :: [Bool] -> FlagA (Point, Point, Point) Point
    applySteps [] = Arr "result" (\(_, _, p) -> p)
    applySteps (False : rest) = proc (a, b, p) -> do
      -- Double: construct 2p − a
      (_, next) <- intersectLC -< ((a, p), (p, a))
      applySteps rest -< (a, b, next)
    applySteps (True : rest) = proc (a, b, p) -> do
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
rationalMult :: Ratio Int -> FlagA (Point, Point) Point
rationalMult r
  | r == 1 =
      let p = numerator r
          q = denominator r
       in group (show p ++ "/" ++ show q) $ Arr "snd" snd
  | otherwise =
      let p = numerator r
          q = denominator r
       in group (show p ++ "/" ++ show q) $ proc (a, b) -> do
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
      bl <- naturalMult h -< (tl, down)
      br <- quad -< (tl, tr, bl)
      fillRectangle col -< (tl, tr, br, bl)
    drawStripes ((h, col) : rest) = proc (tl, tr, unitRef) -> do
      (down, _) <- perpendicular -< (tl, unitRef)
      bl <- naturalMult h -< (tl, down)
      br <- quad -< (tl, tr, bl)
      stripe <- fillRectangle col -< (tl, tr, br, bl)
      (_, newUnitRef) <- translate -< ((tl, unitRef), bl)
      restD <- drawStripes rest -< (bl, br, newUnitRef)
      returnA -< stripe <> restD
