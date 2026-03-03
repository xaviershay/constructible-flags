module Flag.Render.SVGBuilderBackend
    ( SVGBuilderBackend(..)
    ) where

import Flag.Render.Backend (RenderBackend(..))
import Flag.Render.Diagram (drawingToElement)
import Flag.Render.SVGOverlay (renderDrawingToText)

-- | The svg-builder rendering backend.
--
-- Renders a 'Drawing' to an SVG file by converting it to a
-- 'Graphics.Svg.Element' canvas via 'Flag.Render.Diagram.drawingToElement',
-- assembling the full SVG document (including any overlay injection) in
-- memory via 'renderDrawingToText', and writing it once to disk.
data SVGBuilderBackend = SVGBuilderBackend

instance RenderBackend SVGBuilderBackend where
    renderDrawing _ = renderDrawingToText drawingToElement
