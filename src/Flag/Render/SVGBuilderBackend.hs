{-# LANGUAGE TypeFamilies #-}

module Flag.Render.SVGBuilderBackend
    ( SVGBuilderBackend(..)
    ) where

import Graphics.Svg (Element)


import Flag.Render.Backend (RenderBackend(..))

import Flag.Render.Diagram
    ( drawingToElement
    , renderConstructionGeom
    , renderFill
    , renderDots
    )
import Flag.Render.SVGOverlay (writeSVG)

-- | The svg-builder rendering backend.
--
-- This is a simple token type — all state lives in the pure functions
-- imported from 'Flag.Render.Diagram' and 'Flag.Render.SVGOverlay'.
data SVGBuilderBackend = SVGBuilderBackend

instance RenderBackend SVGBuilderBackend where
    type Canvas SVGBuilderBackend = Element

    drawingToCanvas _ = drawingToElement

    layerGeomToCanvas _ = renderConstructionGeom

    layerFillToCanvas _ = renderFill

    dotsToCanvas _ = renderDots

    writeCanvas _ path width bbox canvas = writeSVG path width bbox canvas
