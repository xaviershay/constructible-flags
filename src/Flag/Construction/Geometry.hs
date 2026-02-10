module Flag.Construction.Geometry
    ( evalIntersectLL'
    , evalIntersectLC'
    , evalIntersectCC'
    , dist
    ) where

import Flag.Construction.Types (Point)
import Flag.Construction.Radical (Radical)

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
      sd = sqrt disc
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
      h  = sqrt (r1*r1 - a*a)
      (x1, y1) = c1
      (x2, y2) = c2
      mx = x1 + a*(x2 - x1)/d
      my = y1 + a*(y2 - y1)/d
  in ( (mx + h*(y2 - y1)/d, my - h*(x2 - x1)/d)
     , (mx - h*(y2 - y1)/d, my + h*(x2 - x1)/d)
     )

-- | Euclidean distance between two points (exact)
dist :: Point -> Point -> Radical
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^(2::Int) + (y2 - y1)^(2::Int))
