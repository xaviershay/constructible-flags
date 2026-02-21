{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: Clean this up and DRY with other render files.

module Flag.Render.Raster
  ( renderDiagramPNG
  , renderDrawingPNG
  ) where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Flag.Construction.Types (Drawing(..))
import qualified Flag.Construction.Types as T
import Flag.Construction.Layers (ConstructionLayer(..), pointDist)
import Flag.Construction.Radical (Radical, toDouble)

-- | Render a diagram to a PNG file at the given width (in pixels).
-- Uses the diagrams-rasterific backend.
renderDiagramPNG :: FilePath -> Int -> Diagram B -> IO ()
renderDiagramPNG path width dia =
  -- Use the same "mkWidth" sizing helper used elsewhere (e.g. SVG rendering)
  renderRasterific path (mkWidth (fromIntegral width)) dia

-- Duplicate of drawingToDiagram specialized to the Rasterific backend.
-- This avoids changing the SVG-specialized Diagram module.
 

-- | Convert a Point (Radical, Radical) to (Double, Double) for diagrams.
toDP :: T.Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

toD :: Radical -> Double
toD = toDouble

drawingToDiagramRaster :: Drawing -> Diagram B
drawingToDiagramRaster EmptyDrawing = mempty
drawingToDiagramRaster (Overlay a b) = drawingToDiagramRaster b <> drawingToDiagramRaster a
drawingToDiagramRaster (DrawTriangle col pt1 pt2 pt3) =
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
drawingToDiagramRaster (DrawPath col pts@(_:_)) =
  let dpts = map toDP pts
      (x0, y0) = head dpts
      offsets = zipWith (\(ax, ay) (bx, by) -> r2 (bx - ax, by - ay))
                        dpts (drop 1 dpts)
      path = closeLine (fromOffsets offsets)
  in  strokeLoop path
        # fcA (col `withOpacity` 1.0)
        # lwG 0
        # moveTo (p2 (x0, y0))
drawingToDiagramRaster (DrawPath _ []) = mempty
drawingToDiagramRaster (DrawSVGOverlay _ _ _) = mempty
drawingToDiagramRaster (DrawCircle col center rd) =
  let (cx, cy) = toDP center
      r = toD rd
  in  circle r
        # fcA (col `withOpacity` 1.0)
        # lwG 0
        # moveTo (p2 (cx, cy))

-- Construction geometry
renderLayerRaster :: ConstructionLayer -> Diagram B
renderLayerRaster l = renderConstructionGeomRaster l <> renderFillRaster l

renderConstructionGeomRaster :: ConstructionLayer -> Diagram B
renderConstructionGeomRaster (LayerIntersectLL p1 p2 p3 p4 pts) =
  renderLineRaster (toDP p1) (toDP p2)
  <> renderLineRaster (toDP p3) (toDP p4)
  <> renderDotsRaster (map toDP pts)

renderConstructionGeomRaster (LayerIntersectLC p1 p2 cc ce pts) =
  renderLineRaster (toDP p1) (toDP p2)
  <> renderCircleRaster (toDP cc) (toD (pointDist cc ce))
  <> renderDotsRaster (map toDP pts)

renderConstructionGeomRaster (LayerIntersectCC c1 e1 c2 e2 pts) =
  renderCircleRaster (toDP c1) (toD (pointDist c1 e1))
  <> renderCircleRaster (toDP c2) (toD (pointDist c2 e2))
  <> renderDotsRaster (map toDP pts)

renderConstructionGeomRaster (LayerTriangle _ _ _ _) = mempty
renderConstructionGeomRaster (LayerCircle _ center edge) =
  renderCircleRaster (toDP center) (toD (pointDist center edge))
  <> renderDotsRaster (map toDP [center, edge])

renderFillRaster :: ConstructionLayer -> Diagram B
renderFillRaster (LayerTriangle col pt1 pt2 pt3) =
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
renderFillRaster (LayerCircle col center edge) =
  let (cx, cy) = toDP center
      r = toD (pointDist center edge)
  in  circle r
        # fcA (col `withOpacity` 0.6)
        # lc col
        # lwG 0.02
        # moveTo (p2 (cx, cy))
renderFillRaster _ = mempty

renderLineRaster :: (Double, Double) -> (Double, Double) -> Diagram B
renderLineRaster (x1, y1) (x2, y2) =
  fromVertices [p2 (x1, y1), p2 (x2, y2)]
     # dashingG [0.05, 0.05] 0
     # lc grey
     # lwG 0.02

renderCircleRaster :: (Double, Double) -> Double -> Diagram B
renderCircleRaster (cx, cy) r =
  circle r
    # moveTo (p2 (cx, cy))
    # dashingG [0.08, 0.05] 0
    # lc grey
    # lwG 0.02
    # fillColor transparent

renderDotsRaster :: [(Double, Double)] -> Diagram B
renderDotsRaster pts =
  mconcat [ circle 0.04 # fc black # lw none # moveTo (p2 (x, y))
      | (x, y) <- pts
      ]

-- | Render a Drawing directly to a PNG
renderDrawingPNG :: FilePath -> Int -> Drawing -> IO ()
renderDrawingPNG path width drawing =
  renderRasterific path (mkWidth (fromIntegral width)) (drawingToDiagramRaster drawing)
