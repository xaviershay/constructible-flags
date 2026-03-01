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
import Data.Hashable (Hashable, hashWithSalt)
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

-- | Create a valid XML ID from a list of hashable elements by combining
-- their hashes and formatting the result as a hex string.
sdId :: (Hashable a) => [a] -> T.Text
sdId xs = T.pack (showHex (fromIntegral combined :: Word) "")
  where
    combined = foldl hashWithSalt 0 xs

-- ---------------------------------------------------------------------------
-- Drawing → Element
-- ---------------------------------------------------------------------------

-- | Convert a 'Drawing' to a renderable SVG 'Element'.
-- SVG overlays are skipped here; they are injected as raw SVG in a
-- post-processing step (see 'Flag.Render.SVGOverlay.injectOverlays').
drawingToElement :: Drawing -> Element
drawingToElement CT.EmptyDrawing = mempty
drawingToElement (CT.Overlay a b) = drawingToElement a <> drawingToElement b
drawingToElement (CT.DrawTriangle col pt1 pt2 pt3) =
  polygon_
    [ Points_ <<- pointsAttr (map toDP [pt1, pt2, pt3]),
      Fill_ <<- colHex col,
      Fill_opacity_ <<- "1",
      Stroke_ <<- colHex col,
      Stroke_width_ <<- sd 0.02
    ]
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
drawingToElement (CT.DrawCrescent col outerCenter outerR innerCenter innerR) =
  let (ocx, ocy) = toDP outerCenter
      or' = toD outerR
      (icx, icy) = toDP innerCenter
      ir = toD innerR
      maskId = "cres" <> sdId [ocx, ocy]
   in defs_
        []
        ( mask_
            [makeAttribute "id" maskId]
            ( circle_ [Cx_ <<- sd ocx, Cy_ <<- sd ocy, R_ <<- sd or', Fill_ <<- "white"]
                <> circle_ [Cx_ <<- sd icx, Cy_ <<- sd icy, R_ <<- sd ir, Fill_ <<- "black"]
            )
        )
        <> circle_
          [ Cx_ <<- sd ocx,
            Cy_ <<- sd ocy,
            R_ <<- sd or',
            Fill_ <<- colHex col,
            Fill_opacity_ <<- "1",
            Stroke_ <<- "none",
            makeAttribute "mask" ("url(#" <> maskId <> ")")
          ]
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
renderConstructionGeom (LayerCrescent _ oc oe ic ie) =
  renderCircle (toDP oc) (toD (pointDist oc oe))
    <> renderCircle (toDP ic) (toD (pointDist ic ie))
    <> renderDots (map toDP [oc, oe, ic, ie])
renderConstructionGeom (LayerSVGOverlay _ _ _) = mempty

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
renderFill (LayerCrescent col oc oe ic ie) =
  let (ocx, ocy) = toDP oc
      or' = toD (pointDist oc oe)
      (icx, icy) = toDP ic
      ir = toD (pointDist ic ie)
      maskId = "layer-cres" <> sdId [ocx, ocy]
   in defs_
        []
        ( mask_
            [makeAttribute "id" maskId]
            ( circle_ [Cx_ <<- sd ocx, Cy_ <<- sd ocy, R_ <<- sd or', Fill_ <<- "white"]
                <> circle_ [Cx_ <<- sd icx, Cy_ <<- sd icy, R_ <<- sd ir, Fill_ <<- "black"]
            )
        )
        <> circle_
          [ Cx_ <<- sd ocx,
            Cy_ <<- sd ocy,
            R_ <<- sd or',
            Fill_ <<- colHex col,
            Fill_opacity_ <<- "0.6",
            Stroke_ <<- colHex col,
            Stroke_width_ <<- sd 0.02,
            makeAttribute "mask" ("url(#" <> maskId <> ")")
          ]
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
