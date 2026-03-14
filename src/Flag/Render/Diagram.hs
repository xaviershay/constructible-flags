{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flag.Render.Diagram
  ( drawingToElement,
    renderConstructionGeom,
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

-- | Render a 'Drawing' as pure shape elements with every fill replaced by the
-- given colour text.  Used to populate SVG @\<mask\>@ and @\<clipPath\>@ elements.
-- In a @\<mask\>@, pass @"black"@ so the shapes become transparent cut-outs
-- (SVG masks: white = visible, black = transparent).
-- In a @\<clipPath\>@, the fill colour is irrelevant — the shape boundary alone
-- defines the clip region.
drawingToShapes :: T.Text -> CT.Drawing -> Element
drawingToShapes _ CT.EmptyDrawing = mempty
drawingToShapes col (CT.Overlay a b) =
  drawingToShapes col a <> drawingToShapes col b
drawingToShapes col (CT.DrawTriangle _ p1 p2 p3) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP [p1, p2, p3]),
      Fill_ <<- col,
      Fill_opacity_ <<- "1",
      Stroke_ <<- "none",
      Stroke_width_ <<- "0"
    ]
drawingToShapes col (CT.DrawPath _ pts@(_ : _)) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP pts),
      Fill_ <<- col,
      Fill_opacity_ <<- "1",
      Stroke_ <<- "none",
      Stroke_width_ <<- "0"
    ]
drawingToShapes _ (CT.DrawPath _ []) = mempty
drawingToShapes col (CT.DrawCircle _ center rd) =
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
drawingToShapes col (CT.DrawMasked CT.Mask content maskD) =
  -- Nested Mask: white background with black shapes punched out, then recoloured.
  let innerId = "smsk-" <> sdId content
   in defs_
        []
        ( mask_
            [makeAttribute "id" innerId]
            ( rect_
                [ makeAttribute "x" "-1000",
                  makeAttribute "y" "-1000",
                  makeAttribute "width" "2000",
                  makeAttribute "height" "2000",
                  Fill_ <<- "white"
                ]
                <> drawingToShapes "black" maskD
            )
        )
        <> g_
          [makeAttribute "mask" ("url(#" <> innerId <> ")")]
          (drawingToShapes col content)
drawingToShapes col (CT.DrawMasked CT.Clip content maskD) =
  -- Nested Clip: clip shapes define the visible region, then recolour.
  let innerId = "sclp-" <> sdId content
   in defs_
        []
        (clipPath_ [makeAttribute "id" innerId] (drawingToShapes col maskD))
        <> g_
          [makeAttribute "clip-path" ("url(#" <> innerId <> ")")]
          (drawingToShapes col content)
drawingToShapes _ (CT.DrawSVGOverlay _ _ _) = mempty

-- ---------------------------------------------------------------------------
-- Drawing → Element
-- ---------------------------------------------------------------------------

-- | Convert a 'Drawing' to a renderable SVG 'Element'.
-- SVG overlays are skipped here; they are injected as raw SVG in a
-- post-processing step (see 'Flag.Render.SVGOverlay.injectOverlays').
drawingToElement :: Drawing -> Element
drawingToElement CT.EmptyDrawing = mempty
drawingToElement (CT.Overlay a b) = drawingToElement a <> drawingToElement b
drawingToElement (CT.DrawTriangle col pt1 pt2 pt3) = drawingToElement (CT.DrawPath col [pt1, pt2, pt3])
drawingToElement (CT.DrawPath col pts@(_ : _)) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP pts),
      Fill_ <<- colHex col,
      Fill_opacity_ <<- "1",
      Stroke_ <<- "none",
      Stroke_width_ <<- "0"
    ]
drawingToElement (CT.DrawPath _ []) = mempty
drawingToElement (CT.DrawCircle col center rd) =
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
-- \| Mask mode: white background rect makes everything visible by default;
-- black shapes from the mask drawing punch transparent holes in the content.
drawingToElement (CT.DrawMasked CT.Mask content maskD) =
  let maskId = "msk-" <> sdId content
      contentElem = drawingToElement content
      maskShapes = drawingToShapes "black" maskD
   in defs_
        []
        ( mask_
            [makeAttribute "id" maskId]
            ( rect_
                [ makeAttribute "x" "-1000",
                  makeAttribute "y" "-1000",
                  makeAttribute "width" "2000",
                  makeAttribute "height" "2000",
                  Fill_ <<- "white"
                ]
                <> maskShapes
            )
        )
        <> g_ [makeAttribute "mask" ("url(#" <> maskId <> ")")] contentElem
-- \| Clip mode: shapes from the mask drawing define the visible region;
-- everything outside those shapes is hidden.
drawingToElement (CT.DrawMasked CT.Clip content maskD) =
  let clipId = "clp-" <> sdId content
      contentElem = drawingToElement content
      clipShapes = drawingToShapes "black" maskD
   in defs_
        []
        (clipPath_ [makeAttribute "id" clipId] clipShapes)
        <> g_ [makeAttribute "clip-path" ("url(#" <> clipId <> ")")] contentElem
drawingToElement (CT.DrawSVGOverlay _ _ _) = mempty

-- ---------------------------------------------------------------------------
-- Construction geometry rendering
-- ---------------------------------------------------------------------------

-- | Render a complete layer (construction geometry + fill).
renderLayer :: ConstructionLayer -> Element
renderLayer l = renderConstructionGeom l <> renderFill l

-- | Render the ephemeral construction geometry (dotted lines/circles)
-- and result-point dots for a single step.
renderConstructionGeom :: ConstructionLayer -> Element
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
renderConstructionGeom (LayerNGonVertex _ _ _) = mempty
renderConstructionGeom (LayerTriangle _ _ _ _) = mempty
renderConstructionGeom (LayerCircle _ center edge) =
  renderCircle (toDP center) (toD (pointDist center edge))
    <> renderDots (map toDP [center, edge])
renderConstructionGeom (LayerMasked _ _ _) = mempty
renderConstructionGeom (LayerSVGOverlay _ _ _) = mempty
renderConstructionGeom (LayerLabel _ p) = renderDots [toDP p]

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
renderLine :: (Double, Double) -> (Double, Double) -> Element
renderLine (x1, y1) (x2, y2) =
  line_
    [ X1_ <<- sd x1,
      Y1_ <<- sd y1,
      X2_ <<- sd x2,
      Y2_ <<- sd y2,
      Stroke_ <<- "grey",
      Stroke_width_ <<- sd 0.02,
      Stroke_dasharray_ <<- "0.05,0.05"
    ]

-- | Render a dotted construction circle.
renderCircle :: (Double, Double) -> Double -> Element
renderCircle (cx, cy) r =
  circle_
    [ Cx_ <<- sd cx,
      Cy_ <<- sd cy,
      R_ <<- sd r,
      Fill_ <<- "none",
      Stroke_ <<- "grey",
      Stroke_width_ <<- sd 0.02,
      Stroke_dasharray_ <<- "0.08,0.05"
    ]

-- | Render intersection points as black dots.
renderDots :: [(Double, Double)] -> Element
renderDots pts =
  mconcat
    [ circle_ [Cx_ <<- sd x, Cy_ <<- sd y, R_ <<- "0.04", Fill_ <<- "black", Stroke_ <<- "none"]
    | (x, y) <- pts
    ]
