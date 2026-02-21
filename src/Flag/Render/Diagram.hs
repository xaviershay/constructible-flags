module Flag.Render.Diagram
    ( drawingToDiagram
    , drawingToDiagramWith
    , renderConstructionGeom
    , renderFill
    , renderLayer
    , renderLine
    , renderCircle
    , renderDots
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Diagrams.Prelude hiding (trace, radius)
import Diagrams.Backend.SVG

import Flag.Construction.Types (Drawing(..))
import qualified Flag.Construction.Types as T
import Flag.Construction.Layers (ConstructionLayer(..), pointDist)
import Flag.Construction.Radical (Radical, toDouble)

-- | Convert a Point (Radical, Radical) to (Double, Double) for diagrams.
toDP :: T.Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

-- | Convert a Radical to Double for diagrams.
toD :: Radical -> Double
toD = toDouble

-- ---------------------------------------------------------------------------
-- Drawing → Diagram B
-- ---------------------------------------------------------------------------

-- | Convert a 'Drawing' to a renderable 'Diagram B', without any SVG overlays.
drawingToDiagram :: Drawing -> Diagram B
drawingToDiagram = drawingToDiagramWith Map.empty

-- | Convert a 'Drawing' to a renderable 'Diagram B', compositing any
-- 'DrawSVGOverlay' elements from the supplied pre-loaded diagram map.
drawingToDiagramWith :: Map FilePath (Diagram B) -> Drawing -> Diagram B
drawingToDiagramWith _ EmptyDrawing = mempty
drawingToDiagramWith m (Overlay a b) = drawingToDiagramWith m b <> drawingToDiagramWith m a
drawingToDiagramWith _ (DrawTriangle col pt1 pt2 pt3) =
    let (x1, y1) = toDP pt1
        (x2, y2) = toDP pt2
        (x3, y3) = toDP pt3
        offsets = [ r2 (x2 - x1, y2 - y1), r2 (x3 - x2, y3 - y2) ]
        tri = closeLine (fromOffsets offsets)
    in  strokeLoop tri
          # fcA (col `withOpacity` 1.0)
          # lc col
          # lwG 0.02
          # moveTo (p2 (x1, y1))
drawingToDiagramWith _ (DrawPath col pts@(_:_)) =
    let dpts = map toDP pts
        (x0, y0) = head dpts
        offsets = zipWith (\(ax, ay) (bx, by) -> r2 (bx - ax, by - ay))
                          dpts (drop 1 dpts)
        path = closeLine (fromOffsets offsets)
    in  strokeLoop path
          # fcA (col `withOpacity` 1.0)
          # lwG 0
          # moveTo (p2 (x0, y0))
drawingToDiagramWith _ (DrawPath _ []) = mempty
drawingToDiagramWith _ (DrawCircle col center rd) =
    let (cx, cy) = toDP center
        r = toD rd
    in  circle r
          # fcA (col `withOpacity` 1.0)
          # lwG 0
          # moveTo (p2 (cx, cy))
drawingToDiagramWith m (DrawSVGOverlay path center edge) =
    case Map.lookup path m of
        Nothing -> mempty
        Just overlayDiag ->
            let (cx, cy) = toDP center
                (ex, ey) = toDP edge
                r = sqrt ((ex - cx) ^ (2 :: Int) + (ey - cy) ^ (2 :: Int))
                w = width overlayDiag
                h = height overlayDiag
                diag = sqrt (w * w + h * h)
                s = 2 * r / diag
            in  overlayDiag
                  # scale s
                  # centerXY
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
renderConstructionGeom (LayerIntersectLL p1 p2 p3 p4 pts) =
    renderLine (toDP p1) (toDP p2)
    <> renderLine (toDP p3) (toDP p4)
    <> renderDots (map toDP pts)

renderConstructionGeom (LayerIntersectLC p1 p2 cc ce pts) =
    renderLine (toDP p1) (toDP p2)
    <> renderCircle (toDP cc) (toD (pointDist cc ce))
    <> renderDots (map toDP pts)

renderConstructionGeom (LayerIntersectCC c1 e1 c2 e2 pts) =
    renderCircle (toDP c1) (toD (pointDist c1 e1))
    <> renderCircle (toDP c2) (toD (pointDist c2 e2))
    <> renderDots (map toDP pts)

renderConstructionGeom (LayerTriangle _ _ _ _) = mempty
renderConstructionGeom (LayerCircle _ center edge) =
    renderCircle (toDP center) (toD (pointDist center edge))
    <> renderDots (map toDP [center, edge])
renderConstructionGeom (LayerSVGOverlay _ _ _) = mempty

-- | Render the persistent fill for a layer (only triangles and circles produce fills)
renderFill :: ConstructionLayer -> Diagram B
renderFill (LayerTriangle col pt1 pt2 pt3) =
    let (x1, y1) = toDP pt1
        (x2, y2) = toDP pt2
        (x3, y3) = toDP pt3
        offsets = [ r2 (x2-x1, y2-y1), r2 (x3-x2, y3-y2) ]
        tri = closeLine (fromOffsets offsets)
    in  strokeLoop tri
          # fcA (col `withOpacity` 0.6)
          # lc col
          # lwG 0.02
          # moveTo (p2 (x1, y1))
renderFill (LayerCircle col center edge) =
    let (cx, cy) = toDP center
        r = toD (pointDist center edge)
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
