{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Flag.Render.Backend
    ( RenderBackend(..)
    ) where

import Flag.Construction.Layers (ConstructionLayer)
import Flag.Construction.Types (Drawing)
import Flag.Render.Bounds (BBox)

-- | A swappable rendering backend.
--
-- A backend converts 'Drawing' and 'ConstructionLayer' values into some
-- canvas type @c@ (which must be a 'Monoid' so that results can be
-- composed), and knows how to serialise that canvas to a file.
--
-- The pre-computed 'BBox' is passed to 'writeCanvas' so backends only
-- serialise; they never need to measure.
class Monoid (Canvas b) => RenderBackend b where
    -- | The canvas type produced by this backend.
    type Canvas b

    -- | Convert a 'Drawing' to a canvas.
    drawingToCanvas :: b -> Drawing -> Canvas b

    -- | Convert the ephemeral construction geometry of a 'ConstructionLayer'
    -- (dotted lines/circles and result-point dots) to a canvas.
    layerGeomToCanvas :: b -> ConstructionLayer -> Canvas b

    -- | Convert the persistent fill of a 'ConstructionLayer' to a canvas.
    layerFillToCanvas :: b -> ConstructionLayer -> Canvas b

    -- | Render a list of points as dots on the canvas.
    dotsToCanvas :: b -> [(Double, Double)] -> Canvas b

    -- | Write a canvas to a file.
    -- The 'BBox' supplies the viewBox/bounding information; the 'Double'
    -- is the desired output width in pixels.
    writeCanvas :: b -> FilePath -> Double -> BBox -> Canvas b -> IO ()
