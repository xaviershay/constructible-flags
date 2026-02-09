module Flag.Render.Diagram
    ( drawingToDiagram
    , renderConstructionGeom
    , renderFill
    , renderLayer
    , renderLine
    , renderCircle
    , renderDots
    ) where

import Diagrams.Prelude hiding (trace, radius)
import Diagrams.Backend.SVG

import Flag.Construction.Types (Drawing(..))
import Flag.Construction.Layers (ConstructionLayer(..), pointDist)

-- ---------------------------------------------------------------------------
-- Drawing → Diagram B
-- ---------------------------------------------------------------------------

-- | Convert a 'Drawing' to a renderable 'Diagram B'.
drawingToDiagram :: Drawing -> Diagram B
drawingToDiagram EmptyDrawing = mempty
drawingToDiagram (Overlay a b) = drawingToDiagram b <> drawingToDiagram a
drawingToDiagram (DrawTriangle col (x1, y1) (x2, y2) (x3, y3)) =
    let offsets = [ r2 (x2 - x1, y2 - y1), r2 (x3 - x2, y3 - y2) ]
        tri = closeLine (fromOffsets offsets)
    in  strokeLoop tri
          # fcA (col `withOpacity` 1.0)
          # lc col
          # lwG 0.02
          # moveTo (p2 (x1, y1))
drawingToDiagram (DrawPath col pts@((x0, y0):_)) =
    let offsets = zipWith (\(ax, ay) (bx, by) -> r2 (bx - ax, by - ay))
                          pts (drop 1 pts)
        path = closeLine (fromOffsets offsets)
    in  strokeLoop path
          # fcA (col `withOpacity` 1.0)
          # lwG 0
          # moveTo (p2 (x0, y0))
drawingToDiagram (DrawPath _ []) = mempty
drawingToDiagram (DrawCircle col (cx, cy) r) =
    circle r
      # fcA (col `withOpacity` 1.0)
      # lwG 0
      # moveTo (p2 (cx, cy))

-- ---------------------------------------------------------------------------
-- Construction geometry rendering
-- ---------------------------------------------------------------------------

-- | Render a complete layer (construction geometry + fill)
renderLayer :: ConstructionLayer -> Diagram B
renderLayer l = renderConstructionGeom l <> renderFill l

-- | Render the ephemeral construction geometry (dotted lines/circles)
-- and result-point dots for a single step
renderConstructionGeom :: ConstructionLayer -> Diagram B
renderConstructionGeom (LayerIntersectLL (x1, y1) (x2, y2) (x3, y3) (x4, y4) pts) =
    renderLine (x1, y1) (x2, y2)
    <> renderLine (x3, y3) (x4, y4)
    <> renderDots pts

renderConstructionGeom (LayerIntersectLC (x1, y1) (x2, y2) cc ce pts) =
    renderLine (x1, y1) (x2, y2)
    <> renderCircle cc (pointDist cc ce)
    <> renderDots pts

renderConstructionGeom (LayerIntersectCC c1 e1 c2 e2 pts) =
    renderCircle c1 (pointDist c1 e1)
    <> renderCircle c2 (pointDist c2 e2)
    <> renderDots pts

renderConstructionGeom (LayerTriangle _ _ _ _) = mempty
renderConstructionGeom (LayerCircle _ center edge) =
    renderCircle center (pointDist center edge)
    <> renderDots [center, edge]

-- | Render the persistent fill for a layer (only triangles and circles produce fills)
renderFill :: ConstructionLayer -> Diagram B
renderFill (LayerTriangle col (x1, y1) (x2, y2) (x3, y3)) =
    let offsets = [ r2 (x2-x1, y2-y1), r2 (x3-x2, y3-y2) ]
        tri = closeLine (fromOffsets offsets)
    in  strokeLoop tri
          # fcA (col `withOpacity` 0.6)
          # lc col
          # lwG 0.02
          # moveTo (p2 (x1, y1))
renderFill (LayerCircle col (cx, cy) (ex, ey)) =
    let r = pointDist (cx, cy) (ex, ey)
    in  circle r
          # fcA (col `withOpacity` 0.6)
          # lc col
          # lwG 0.02
          # moveTo (p2 (cx, cy))
renderFill _ = mempty

-- | Render a dotted construction line connecting two points
renderLine :: (Double, Double) -> (Double, Double) -> Diagram B
renderLine (x1, y1) (x2, y2) =
    fromVertices [p2 (x1, y1), p2 (x2, y2)]
         # dashingG [0.05, 0.05] 0
         # lc grey
         # lwG 0.02

-- | Render a dotted construction circle
renderCircle :: (Double, Double) -> Double -> Diagram B
renderCircle (cx, cy) r =
    circle r
      # moveTo (p2 (cx, cy))
      # dashingG [0.08, 0.05] 0
      # lc grey
      # lwG 0.02
      # fillColor transparent

-- | Render intersection points as black dots
renderDots :: [(Double, Double)] -> Diagram B
renderDots pts =
    mconcat [ circle 0.04 # fc black # lw none # moveTo (p2 (x, y))
            | (x, y) <- pts
            ]
