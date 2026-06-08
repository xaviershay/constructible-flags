{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flag.Render.Diagram
  ( drawingToElement,
    renderConstructionGeom,
    renderConstructionStrokes,
    renderConstructionDots,
    renderFill,
    renderLayer,
    renderLine,
    renderCircle,
    renderDots,
  )
where

import Data.Colour (Colour)
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.Hashable (hash)
import qualified Data.Text as T
import Flag.Construction.FieldNumber (FieldNumber, toDouble)
import Flag.Construction.Layers (ConstructionLayer (..), pointDist)
import Flag.Construction.Types (Drawing (..))
import qualified Flag.Construction.Types as CT
import Graphics.Svg
import Numeric (showFFloat, showHex)

-- | Convert a Point to (Double, Double) for rendering.
toDP :: CT.Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

-- | Convert a Number to Double for rendering.
toD :: FieldNumber -> Double
toD = toDouble

-- | Format a Double with enough precision for SVG attributes.
-- Trailing zeros after the decimal point are trimmed, and a lone
-- trailing decimal point is also removed (e.g. 1.500000 → "1.5",
-- 2.000000 → "2").
sd :: Double -> T.Text
sd d = T.pack (trimZeros (showFFloat (Just 6) d ""))
  where
    trimZeros s
      | '.' `elem` s = reverse (dropWhile (== '.') (dropWhile (== '0') (reverse s)))
      | otherwise = s

-- | Format a Colour as an SVG hex colour string #rrggbb.
colHex :: Colour Double -> T.Text
colHex col =
  let RGB r g b = toSRGB col
      toByte x = round (x * 255) :: Int
      hexByte n =
        let s = showHex n ""
         in T.pack (if length s == 1 then "0" ++ s else s)
   in "#" <> hexByte (toByte r) <> hexByte (toByte g) <> hexByte (toByte b)

-- | Format a list of (x,y) pairs as SVG points attribute value.
pointsAttr :: [(Double, Double)] -> T.Text
pointsAttr pts = T.intercalate " " [sd x <> "," <> sd y | (x, y) <- pts]

-- | Create a stable SVG ID fragment from the hash of a 'Drawing'.
sdId :: CT.Drawing -> T.Text
sdId d = T.pack (showHex (fromIntegral (hash d) :: Word) "")

-- ---------------------------------------------------------------------------
-- Shape-only rendering (for use inside <mask> and <clipPath>)
-- ---------------------------------------------------------------------------

-- | Collect all @\<defs\>@ content (mask and clip-path definitions) from a
-- 'Drawing' that will be rendered as shapes.  Returns the inner content to
-- be placed inside a single @\<defs\>@ element — does *not* wrap in @\<defs\>@
-- itself.
collectDefsForShapes :: T.Text -> CT.Drawing -> Element
collectDefsForShapes _ CT.EmptyDrawing = mempty
collectDefsForShapes col (CT.Overlay a b) =
  collectDefsForShapes col a <> collectDefsForShapes col b
collectDefsForShapes _ (CT.DrawTriangle _ _ _ _) = mempty
collectDefsForShapes _ (CT.DrawPath _ _) = mempty
collectDefsForShapes _ (CT.DrawCircle _ _ _) = mempty
collectDefsForShapes col (CT.DrawMasked CT.Mask content maskD) =
  let innerId = "smsk-" <> sdId content
   in mask_
        [makeAttribute "id" innerId]
        ( rect_
            [ makeAttribute "x" "-1000",
              makeAttribute "y" "-1000",
              makeAttribute "width" "2000",
              makeAttribute "height" "2000",
              Fill_ <<- "white"
            ]
            <> drawingToShapesBody col maskD
        )
        <> collectDefsForShapes col content
        <> collectDefsForShapes "black" maskD
collectDefsForShapes col (CT.DrawMasked CT.Clip content maskD) =
  let innerId = "sclp-" <> sdId content
   in clipPath_ [makeAttribute "id" innerId] (drawingToShapesBody col maskD)
        <> collectDefsForShapes col content
        <> collectDefsForShapes col maskD
collectDefsForShapes _ (CT.DrawSVGOverlay _ _ _) = mempty

-- | Render a 'Drawing' as pure shape elements with every fill replaced by the
-- given colour text, *without* any @\<defs\>@ blocks (those are collected
-- separately by 'collectDefsForShapes').  Used to populate SVG @\<mask\>@ and
-- @\<clipPath\>@ elements.
-- In a @\<mask\>@, pass @"black"@ so the shapes become transparent cut-outs
-- (SVG masks: white = visible, black = transparent).
-- In a @\<clipPath\>@, the fill colour is irrelevant — the shape boundary alone
-- defines the clip region.
drawingToShapesBody :: T.Text -> CT.Drawing -> Element
drawingToShapesBody _ CT.EmptyDrawing = mempty
drawingToShapesBody col (CT.Overlay a b) =
  drawingToShapesBody col a <> drawingToShapesBody col b
drawingToShapesBody col (CT.DrawTriangle _ p1 p2 p3) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP [p1, p2, p3]),
      Fill_ <<- col,
      Fill_opacity_ <<- "1",
      Stroke_ <<- "none",
      Stroke_width_ <<- "0"
    ]
drawingToShapesBody col (CT.DrawPath _ pts@(_ : _)) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP pts),
      Fill_ <<- col,
      Fill_opacity_ <<- "1",
      Stroke_ <<- "none",
      Stroke_width_ <<- "0"
    ]
drawingToShapesBody _ (CT.DrawPath _ []) = mempty
drawingToShapesBody col (CT.DrawCircle _ center rd) =
  let (cx, cy) = toDP center
      r = toD rd
   in circle_
        [ Cx_ <<- sd cx,
          Cy_ <<- sd cy,
          R_ <<- sd r,
          Fill_ <<- col,
          Fill_opacity_ <<- "1",
          Stroke_ <<- "none"
        ]
drawingToShapesBody col (CT.DrawMasked CT.Mask content _maskD) =
  let innerId = "smsk-" <> sdId content
   in g_
        [makeAttribute "mask" ("url(#" <> innerId <> ")")]
        (drawingToShapesBody col content)
drawingToShapesBody col (CT.DrawMasked CT.Clip content _maskD) =
  let innerId = "sclp-" <> sdId content
   in g_
        [makeAttribute "clip-path" ("url(#" <> innerId <> ")")]
        (drawingToShapesBody col content)
drawingToShapesBody _ (CT.DrawSVGOverlay _ _ _) = mempty

-- ---------------------------------------------------------------------------
-- Drawing → Element
-- ---------------------------------------------------------------------------

-- | Collect all @\<defs\>@ content (mask and clip-path definitions) from a
-- 'Drawing'.  Returns the inner content to be placed inside a single
-- @\<defs\>@ element — does *not* wrap in @\<defs\>@ itself.
collectDefs :: CT.Drawing -> Element
collectDefs CT.EmptyDrawing = mempty
collectDefs (CT.Overlay a b) = collectDefs a <> collectDefs b
collectDefs (CT.DrawTriangle _ _ _ _) = mempty
collectDefs (CT.DrawPath _ _) = mempty
collectDefs (CT.DrawCircle _ _ _) = mempty
-- Mask mode: the mask element itself goes into defs; recurse into both
-- the content and mask sub-drawings to collect any nested defs.
collectDefs (CT.DrawMasked CT.Mask content maskD) =
  let maskId = "msk-" <> sdId content
   in mask_
        [makeAttribute "id" maskId]
        ( rect_
            [ makeAttribute "x" "-1000",
              makeAttribute "y" "-1000",
              makeAttribute "width" "2000",
              makeAttribute "height" "2000",
              Fill_ <<- "white"
            ]
            <> drawingToShapesBody "black" maskD
        )
        <> collectDefs content
        <> collectDefsForShapes "black" maskD
-- Clip mode: the clipPath element itself goes into defs; recurse into both
-- the content and mask sub-drawings to collect any nested defs.
collectDefs (CT.DrawMasked CT.Clip content maskD) =
  let clipId = "clp-" <> sdId content
   in clipPath_ [makeAttribute "id" clipId] (drawingToShapesBody "black" maskD)
        <> collectDefs content
        <> collectDefsForShapes "black" maskD
collectDefs (CT.DrawSVGOverlay _ _ _) = mempty

-- | Convert a 'Drawing' to a renderable SVG 'Element', *without* any
-- @\<defs\>@ blocks (those are collected separately by 'collectDefs').
-- SVG overlays are skipped here; they are injected as raw SVG in a
-- post-processing step (see 'Flag.Render.SVGOverlay.injectOverlays').
drawingToBody :: CT.Drawing -> Element
drawingToBody CT.EmptyDrawing = mempty
drawingToBody (CT.Overlay a b) = drawingToBody a <> drawingToBody b
drawingToBody (CT.DrawTriangle col pt1 pt2 pt3) = drawingToBody (CT.DrawPath col [pt1, pt2, pt3])
drawingToBody (CT.DrawPath col pts@(_ : _)) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP pts),
      Fill_ <<- colHex col,
      Fill_opacity_ <<- "1",
      Stroke_ <<- "none",
      Stroke_width_ <<- "0"
    ]
drawingToBody (CT.DrawPath _ []) = mempty
drawingToBody (CT.DrawCircle col center rd) =
  let (cx, cy) = toDP center
      r = toD rd
   in circle_
        [ Cx_ <<- sd cx,
          Cy_ <<- sd cy,
          R_ <<- sd r,
          Fill_ <<- colHex col,
          Fill_opacity_ <<- "1",
          Stroke_ <<- "none"
        ]
-- \| Mask mode: reference the mask by id (defined in the top-level defs block).
drawingToBody (CT.DrawMasked CT.Mask content _maskD) =
  let maskId = "msk-" <> sdId content
   in g_ [makeAttribute "mask" ("url(#" <> maskId <> ")")] (drawingToBody content)
-- \| Clip mode: reference the clipPath by id (defined in the top-level defs block).
drawingToBody (CT.DrawMasked CT.Clip content _maskD) =
  let clipId = "clp-" <> sdId content
   in g_ [makeAttribute "clip-path" ("url(#" <> clipId <> ")")] (drawingToBody content)
drawingToBody (CT.DrawSVGOverlay _ _ _) = mempty

-- | Convert a 'Drawing' to a renderable SVG 'Element'.
-- All mask and clip-path definitions are gathered into a single @\<defs\>@
-- block prepended to the body.
-- SVG overlays are skipped here; they are injected as raw SVG in a
-- post-processing step (see 'Flag.Render.SVGOverlay.injectOverlays').
drawingToElement :: Drawing -> Element
drawingToElement d =
  let defsContent = collectDefs d
      body = drawingToBody d
   in defs_ [] defsContent <> body

-- ---------------------------------------------------------------------------
-- Construction geometry rendering
-- ---------------------------------------------------------------------------

-- | Render a complete layer (construction geometry + fill).
-- @geomScale@ is the conversion factor (diagram-units per output pixel),
-- used to size construction markers proportionally to the canvas.  Pass
-- @bboxWidth / svgWidthPx@ for the current frame's viewport.
renderLayer :: Double -> ConstructionLayer -> Element
renderLayer s l = renderConstructionGeom s l <> renderFill l

-- | Render the ephemeral construction geometry (dotted lines/circles)
-- and result-point dots for a single step.
-- @geomScale@ converts target pixel sizes to diagram-coordinate sizes so
-- that markers appear at a consistent visual size regardless of the flag's
-- internal coordinate scale.  Pass @bboxWidth / svgWidthPx@.
renderConstructionGeom :: Double -> ConstructionLayer -> Element
renderConstructionGeom s l = renderConstructionStrokes s l <> renderConstructionDots s l

-- | Render only the dotted strokes (lines/circles) of a layer's
-- construction geometry, omitting the result-point dots.  Useful when
-- those two parts need separate styling (e.g. fading the strokes while
-- keeping the dots fully opaque).
renderConstructionStrokes :: Double -> ConstructionLayer -> Element
renderConstructionStrokes s (LayerIntersectLL p1 p2 p3 p4 _) =
  renderLine s (toDP p1) (toDP p2)
    <> renderLine s (toDP p3) (toDP p4)
renderConstructionStrokes s (LayerIntersectLC p1 p2 cc ce _) =
  renderLine s (toDP p1) (toDP p2)
    <> renderCircle s (toDP cc) (toD (pointDist cc ce))
renderConstructionStrokes s (LayerIntersectCC c1 e1 c2 e2 _) =
  renderCircle s (toDP c1) (toD (pointDist c1 e1))
    <> renderCircle s (toDP c2) (toD (pointDist c2 e2))
renderConstructionStrokes _ (LayerNGonVertex _ _ _) = mempty
renderConstructionStrokes _ (LayerTriangle _ _ _ _) = mempty
renderConstructionStrokes s (LayerCircle _ center edge) =
  renderCircle s (toDP center) (toD (pointDist center edge))
renderConstructionStrokes _ (LayerMasked _ _ _) = mempty
renderConstructionStrokes _ (LayerSVGOverlay _ _ _) = mempty
renderConstructionStrokes _ (LayerLabel _ _) = mempty

-- | Render only the result-point dots of a layer's construction geometry,
-- omitting the dotted strokes.  Complement of 'renderConstructionStrokes'.
renderConstructionDots :: Double -> ConstructionLayer -> Element
renderConstructionDots s (LayerIntersectLL _ _ _ _ pts) = renderDots s (map toDP pts)
renderConstructionDots s (LayerIntersectLC _ _ _ _ pts) = renderDots s (map toDP pts)
renderConstructionDots s (LayerIntersectCC _ _ _ _ pts) = renderDots s (map toDP pts)
renderConstructionDots _ (LayerNGonVertex _ _ _) = mempty
renderConstructionDots _ (LayerTriangle _ _ _ _) = mempty
renderConstructionDots s (LayerCircle _ center edge) =
  renderDots s (map toDP [center, edge])
renderConstructionDots _ (LayerMasked _ _ _) = mempty
renderConstructionDots _ (LayerSVGOverlay _ _ _) = mempty
renderConstructionDots s (LayerLabel _ p) = renderDots s [toDP p]

-- | Render the persistent fill for a layer.
renderFill :: ConstructionLayer -> Element
renderFill (LayerTriangle col pt1 pt2 pt3) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP [pt1, pt2, pt3]),
      Fill_ <<- colHex col,
      Fill_opacity_ <<- "0.6",
      Stroke_ <<- colHex col,
      Stroke_width_ <<- sd 0.02
    ]
renderFill (LayerCircle col center edge) =
  let (cx, cy) = toDP center
      r = toD (pointDist center edge)
   in circle_
        [ Cx_ <<- sd cx,
          Cy_ <<- sd cy,
          R_ <<- sd r,
          Fill_ <<- colHex col,
          Fill_opacity_ <<- "0.6",
          Stroke_ <<- colHex col,
          Stroke_width_ <<- sd 0.02
        ]
-- \| Use `opacity` (not `fill-opacity`) so it applies to the whole composited group.
renderFill (LayerMasked mode content maskD) =
  g_
    [makeAttribute "opacity" "0.6"]
    (drawingToElement (CT.DrawMasked mode content maskD))
renderFill _ = mempty

-- | Render a dotted construction line connecting two points.
-- @geomScale@ is diagram-units per output pixel; stroke and dash sizes are
-- expressed as pixel values multiplied by this factor so they stay visually
-- constant regardless of the flag's coordinate scale.
renderLine :: Double -> (Double, Double) -> (Double, Double) -> Element
renderLine s (x1, y1) (x2, y2) =
  line_
    [ X1_ <<- sd x1,
      Y1_ <<- sd y1,
      X2_ <<- sd x2,
      Y2_ <<- sd y2,
      Stroke_ <<- "grey",
      Stroke_width_ <<- sd (2.0 * s),
      Stroke_dasharray_ <<- (sd (5.0 * s) <> "," <> sd (5.0 * s))
    ]

-- | Render a dotted construction circle.
-- See 'renderLine' for the @geomScale@ convention.
renderCircle :: Double -> (Double, Double) -> Double -> Element
renderCircle s (cx, cy) r =
  circle_
    [ Cx_ <<- sd cx,
      Cy_ <<- sd cy,
      R_ <<- sd r,
      Fill_ <<- "none",
      Stroke_ <<- "grey",
      Stroke_width_ <<- sd (2.0 * s),
      Stroke_dasharray_ <<- (sd (8.0 * s) <> "," <> sd (5.0 * s))
    ]

-- | Render intersection points as black dots.
-- @geomScale@ is diagram-units per output pixel; the dot radius is expressed
-- as a pixel target so it stays visually constant across flag coordinate scales.
renderDots :: Double -> [(Double, Double)] -> Element
renderDots s pts =
  mconcat
    [ circle_ [Cx_ <<- sd x, Cy_ <<- sd y, R_ <<- sd (4.0 * s), Fill_ <<- "black", Stroke_ <<- "none"]
    | (x, y) <- pts
    ]
