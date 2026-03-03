module Flag.Render.Backend
    ( RenderBackend(..)
    ) where

import Flag.Construction.Types (Drawing)

-- | A swappable rendering backend.
--
-- Each backend implements the full pipeline from 'Drawing' to file,
-- including optimisation, bounding-box computation, and any
-- backend-specific post-processing (e.g. SVG overlay injection for
-- 'SVGBuilderBackend', rsvg-convert for 'PNGBackend').
--
-- This typeclass isn't actually used, but kept around to document the expected
-- interface for backends.
class RenderBackend b where
    -- | Render a 'Drawing' to a file.
    --
    -- @renderDrawing backend outPath width drawing@ writes the rendered
    -- output to @outPath@.  The @width@ is the desired output width in
    -- pixels (or an equivalent measure for non-raster backends).
    --
    -- No default implementation is provided — each backend's pipeline is
    -- meaningfully different and must be implemented explicitly.
    renderDrawing :: b -> FilePath -> Double -> Drawing -> IO ()
