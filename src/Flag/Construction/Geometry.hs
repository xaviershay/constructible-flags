module Flag.Construction.Geometry
    ( evalIntersectLL'
    , evalIntersectLC'
    , evalIntersectCC'
    , evalNGonVertex
    , dist
    ) where

import Flag.Construction.Types (Point, Number)
import Flag.Construction.FieldNumber (isZero)

-- | Line-line intersection from defining points
evalIntersectLL' :: ((Point, Point), (Point, Point)) -> Point
evalIntersectLL' ((p1, p2), (p3, p4)) =
  let (x1, y1) = p1
      (x2, y2) = p2
      (x3, y3) = p3
      (x4, y4) = p4
      denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
      t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
  in if isZero ((x2-x1)^(2::Int) + (y2-y1)^(2::Int))
       then error "evalIntersectLL': first line is degenerate (both points are equal)"
     else if isZero ((x4-x3)^(2::Int) + (y4-y3)^(2::Int))
       then error "evalIntersectLL': second line is degenerate (both points are equal)"
     else if isZero denom
       then error "cannot intersect parallel lines"
     else (x1 + t * (x2 - x1), y1 + t * (y2 - y1))

-- | Line-circle intersection from defining points.
--
-- Uses r² directly (avoiding sqrt for the radius) and divides by
-- the rational-friendly 2a instead of computing sqrt(disc)/(2a).
evalIntersectLC' :: ((Point, Point), (Point, Point)) -> (Point, Point)
evalIntersectLC' ((lp1, lp2), (cc, ce)) =
  let r2 = (fst ce - fst cc)^(2::Int) + (snd ce - snd cc)^(2::Int)
      (x1, y1) = lp1
      (x2, y2) = lp2
      dx = x2 - x1
      dy = y2 - y1
      fx = x1 - fst cc
      fy = y1 - snd cc
      a  = dx*dx + dy*dy
      b  = 2*(fx*dx + fy*dy)
      c  = fx*fx + fy*fy - r2
      disc = b*b - 4*a*c
      sd = sqrt disc
      t1 = (-b - sd) / (2*a)
      t2 = (-b + sd) / (2*a)
  in if isZero a
       then error "evalIntersectLC': line is degenerate (both points are equal)"
     else if isZero r2
       then error "evalIntersectLC': circle is degenerate (zero radius: center equals edge point)"
     else ( (x1 + t1*dx, y1 + t1*dy)
          , (x1 + t2*dx, y1 + t2*dy)
          )

-- | Circle-circle intersection from defining points.
--
-- Optimised to divide by d² (a sum of squares) instead of d (a sqrt).
--
-- Derivation: let ad = a/d, hd = h/d.  Then
--   ad = (r1² - r2² + d²) / (2·d²)
--   hd = √(r1²/d² - ad²)
-- and the intersection points are:
--   (x1 + ad·dx + hd·dy,  y1 + ad·dy - hd·dx)
--   (x1 + ad·dx - hd·dy,  y1 + ad·dy + hd·dx)
evalIntersectCC' :: ((Point, Point), (Point, Point)) -> (Point, Point)
evalIntersectCC' ((c1, e1), (c2, e2)) =
  let (x1, y1) = c1
      (x2, y2) = c2
      dx = x2 - x1
      dy = y2 - y1
      d2 = dx*dx + dy*dy          -- d² (no sqrt needed)
      r1sq = let (ex,ey) = e1 in (ex - x1)^(2::Int) + (ey - y1)^(2::Int)
      r2sq = let (ex,ey) = e2 in (ex - x2)^(2::Int) + (ey - y2)^(2::Int)
      -- a/d and h/d, computed without dividing by d
      ad = (r1sq - r2sq + d2) / (2 * d2)
      hd = sqrt (r1sq / d2 - ad * ad)
      mx = x1 + ad * dx
      my = y1 + ad * dy
  in if isZero r1sq
       then error "evalIntersectCC': first circle is degenerate (zero radius: center equals edge point)"
     else if isZero r2sq
       then error "evalIntersectCC': second circle is degenerate (zero radius: center equals edge point)"
     else ( (mx + hd * dy, my - hd * dx)
          , (mx - hd * dy, my + hd * dx)
          )

-- | Euclidean distance between two points
dist :: Point -> Point -> Number
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^(2::Int) + (y2 - y1)^(2::Int))


-- | Evaluate the k-th vertex of a regular n-gon given (center, firstVertex).
-- Uses trigonometry via the Floating instance.
evalNGonVertex :: Int -> Int -> (Point, Point) -> Point
evalNGonVertex _ 0 ((_, _), (vx, vy)) = (vx, vy)
evalNGonVertex n k ((cx, cy), (vx, vy)) =
  let theta = 2 * pi * fromIntegral k / fromIntegral n
      cosT = cos theta
      sinT = sin theta
      dx = vx - cx
      dy = vy - cy
      newx = cx + dx * cosT - dy * sinT
      newy = cy + dx * sinT + dy * cosT
  in (newx, newy)
