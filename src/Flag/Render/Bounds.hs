module Flag.Render.Bounds
    ( BBox
    , unionBBox
    , applyPadding
    , drawingBounds
    , layerBounds
    ) where

import Flag.Construction.FieldNumber (toDouble)
import Flag.Construction.Layers (ConstructionLayer(..), pointDist)
import Flag.Construction.Types (Drawing(..), Point)

-- | Axis-aligned bounding box: (minX, minY, maxX, maxY) in diagram (y-up) coordinates.
type BBox = (Double, Double, Double, Double)

-- | Union of two bounding boxes.
unionBBox :: BBox -> BBox -> BBox
unionBBox (ax1, ay1, ax2, ay2) (bx1, by1, bx2, by2) =
    (min ax1 bx1, min ay1 by1, max ax2 bx2, max ay2 by2)

-- | Expand a bounding box uniformly by a scale factor around its centre.
-- E.g. @applyPadding 1.3@ adds 15% padding on every side.
applyPadding :: Double -> BBox -> BBox
applyPadding factor (minX, minY, maxX, maxY) =
    let w  = maxX - minX
        h  = maxY - minY
        cx = (minX + maxX) / 2
        cy = (minY + maxY) / 2
        hw = w * factor / 2
        hh = h * factor / 2
    in (cx - hw, cy - hh, cx + hw, cy + hh)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

toDP :: Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

pointBBox :: [(Double, Double)] -> BBox
pointBBox pts =
    ( minimum (map fst pts), minimum (map snd pts)
    , maximum (map fst pts), maximum (map snd pts)
    )

circleBBox :: (Double, Double) -> Double -> BBox
circleBBox (cx, cy) r = (cx - r, cy - r, cx + r, cy + r)

-- ---------------------------------------------------------------------------
-- Drawing bounds
-- ---------------------------------------------------------------------------

-- | Compute the bounding box of a 'Drawing'.
-- Returns 'Nothing' for 'EmptyDrawing' and 'DrawSVGOverlay' (overlays are
-- injected as post-processing and do not affect the viewBox).
drawingBounds :: Drawing -> Maybe BBox
drawingBounds EmptyDrawing = Nothing
drawingBounds (Overlay a b) =
    case (drawingBounds a, drawingBounds b) of
        (Nothing, x)       -> x
        (x,       Nothing) -> x
        (Just ba, Just bb) -> Just (unionBBox ba bb)
drawingBounds (DrawTriangle _ p1 p2 p3) =
    Just (pointBBox (map toDP [p1, p2, p3]))
drawingBounds (DrawPath _ []) = Nothing
drawingBounds (DrawPath _ pts) =
    Just (pointBBox (map toDP pts))
drawingBounds (DrawCircle _ center rd) =
    Just (circleBBox (toDP center) (toDouble rd))
drawingBounds (DrawCrescent _ outerCenter outerR _ _) =
    Just (circleBBox (toDP outerCenter) (toDouble outerR))
drawingBounds (DrawSVGOverlay _ _ _) = Nothing

-- ---------------------------------------------------------------------------
-- ConstructionLayer bounds
-- ---------------------------------------------------------------------------

-- | Compute the bounding box of a 'ConstructionLayer' from its geometry.
-- Returns 'Nothing' for layers with no geometric extent.
layerBounds :: ConstructionLayer -> Maybe BBox
layerBounds (LayerIntersectLL p1 p2 p3 p4 pts) =
    Just (pointBBox (map toDP (p1 : p2 : p3 : p4 : pts)))
layerBounds (LayerIntersectLC p1 p2 cc ce pts) =
    let r        = toDouble (pointDist cc ce)
        (ccx, ccy) = toDP cc
        linePts  = map toDP [p1, p2] ++ map toDP pts
        xs = map fst linePts ++ [ccx - r, ccx + r]
        ys = map snd linePts ++ [ccy - r, ccy + r]
    in Just (minimum xs, minimum ys, maximum xs, maximum ys)
layerBounds (LayerIntersectCC c1 e1 c2 e2 pts) =
    let r1 = toDouble (pointDist c1 e1)
        r2 = toDouble (pointDist c2 e2)
        (c1x, c1y) = toDP c1
        (c2x, c2y) = toDP c2
        pts' = map toDP pts
        xs = map fst pts' ++ [c1x - r1, c1x + r1, c2x - r2, c2x + r2]
        ys = map snd pts' ++ [c1y - r1, c1y + r1, c2y - r2, c2y + r2]
    in Just (minimum xs, minimum ys, maximum xs, maximum ys)
layerBounds (LayerNGonVertex c e pts) =
    let r = toDouble (pointDist c e)
        (cx, cy) = toDP c
        pts' = map toDP pts
        xs = map fst pts' ++ [cx - r, cx + r]
        ys = map snd pts' ++ [cy - r, cy + r]
    in Just (minimum xs, minimum ys, maximum xs, maximum ys)
layerBounds (LayerTriangle _ p1 p2 p3) =
    Just (pointBBox (map toDP [p1, p2, p3]))
layerBounds (LayerCircle _ center edge) =
    Just (circleBBox (toDP center) (toDouble (pointDist center edge)))
layerBounds (LayerCrescent _ oc oe _ _) =
    Just (circleBBox (toDP oc) (toDouble (pointDist oc oe)))
layerBounds (LayerSVGOverlay _ center edge) =
    let (cx, cy) = toDP center
        (ex, ey) = toDP edge
        r = sqrt ((ex - cx) ^ (2 :: Int) + (ey - cy) ^ (2 :: Int))
    in Just (circleBBox (cx, cy) r)
