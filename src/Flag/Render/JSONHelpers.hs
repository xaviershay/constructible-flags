{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.JSONHelpers
  ( jObj
  , jStr
  , jNum
  , fromList
  , jsonPoint
  , jsonXY
  , jsonLine
  , colourToHex
  , layerGeomJson
  , layerFillJson
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Colour (Colour)
import Data.Colour.SRGB (channelBlue, channelGreen, channelRed, toSRGB)
import Flag.Construction.FieldNumber (FieldNumber, toDouble, toKaTeX)
import Flag.Construction.Layers
  ( ConstructionLayer (..)
  , pointDist
  )
import Flag.Construction.Types (Point)
import Numeric (showHex)

-- ---------------------------------------------------------------------------
-- Aeson combinators
-- ---------------------------------------------------------------------------

jObj :: [(T.Text, Aeson.Value)] -> Aeson.Value
jObj pairs = Aeson.Object $ KM.fromList [(Key.fromText k, v) | (k, v) <- pairs]

jStr :: String -> Aeson.Value
jStr = Aeson.String . T.pack

jNum :: Double -> Aeson.Value
jNum = Aeson.Number . realToFrac

fromList :: [Aeson.Value] -> Aeson.Array
fromList = foldl (\v x -> v <> pure x) mempty

-- ---------------------------------------------------------------------------
-- Point helpers
-- ---------------------------------------------------------------------------

-- | Convert a Point to (Double, Double).
toDP :: Point -> (Double, Double)
toDP (x, y) = (toDouble x, toDouble y)

-- | Convert a Number to Double.
toD :: FieldNumber -> Double
toD = toDouble

-- | Encode a point as a JSON object with exact and approximate coordinates.
jsonPoint :: Point -> String -> Aeson.Value
jsonPoint (x, y) label =
  jObj
    [ ("x",      jNum (toDouble x))
    , ("y",      jNum (toDouble y))
    , ("exactX", jStr (toKaTeX x))
    , ("exactY", jStr (toKaTeX y))
    , ("label",  jStr label)
    ]

-- | Compact [x, y] array for use in geometry instructions.
jsonXY :: Double -> Double -> Aeson.Value
jsonXY x y = Aeson.Array $ fromList [jNum x, jNum y]

-- | A line as [[x1,y1],[x2,y2]].
jsonLine :: Point -> Point -> Aeson.Value
jsonLine p1 p2 =
  let (x1, y1) = toDP p1
      (x2, y2) = toDP p2
   in Aeson.Array $ fromList [jsonXY x1 y1, jsonXY x2 y2]

-- ---------------------------------------------------------------------------
-- Colour helpers
-- ---------------------------------------------------------------------------

colourToHex :: Colour Double -> String
colourToHex col =
  let rgb = toSRGB col
      r = clamp $ round (channelRed   rgb * 255) :: Int
      g = clamp $ round (channelGreen rgb * 255) :: Int
      b = clamp $ round (channelBlue  rgb * 255) :: Int
   in "#" ++ hexByte r ++ hexByte g ++ hexByte b
  where
    clamp x = max 0 (min 255 x)
    hexByte n
      | n < 16    = "0" ++ showHex n ""
      | otherwise = showHex n ""

-- ---------------------------------------------------------------------------
-- Layer → JSON
-- ---------------------------------------------------------------------------

-- | Structured description of the construction geometry for a layer.
-- Returns Null for layers with no construction geometry to display.
layerGeomJson :: ConstructionLayer -> Aeson.Value
layerGeomJson (LayerIntersectLL p1 p2 p3 p4 _) =
  jObj
    [ ("type", jStr "intersectLL")
    , ("l1",   jsonLine p1 p2)
    , ("l2",   jsonLine p3 p4)
    ]
layerGeomJson (LayerIntersectLC lp1 lp2 cc ce _) =
  let (cx, cy) = toDP cc
      r        = toD (pointDist cc ce)
   in jObj
        [ ("type", jStr "intersectLC")
        , ("line", jsonLine lp1 lp2)
        , ("cx",   jNum cx)
        , ("cy",   jNum cy)
        , ("r",    jNum r)
        ]
layerGeomJson (LayerIntersectCC c1 e1 c2 e2 _) =
  let (c1x, c1y) = toDP c1
      r1          = toD (pointDist c1 e1)
      (c2x, c2y) = toDP c2
      r2          = toD (pointDist c2 e2)
   in jObj
        [ ("type", jStr "intersectCC")
        , ("cx1",  jNum c1x)
        , ("cy1",  jNum c1y)
        , ("r1",   jNum r1)
        , ("cx2",  jNum c2x)
        , ("cy2",  jNum c2y)
        , ("r2",   jNum r2)
        ]
layerGeomJson (LayerNGonVertex _ _ _)  = Aeson.Null
layerGeomJson (LayerTriangle _ _ _ _)  = Aeson.Null
layerGeomJson (LayerCircle _ cc ce) =
  let (cx, cy) = toDP cc
      r         = toD (pointDist cc ce)
   in jObj
        [ ("type", jStr "circle")
        , ("cx",   jNum cx)
        , ("cy",   jNum cy)
        , ("r",    jNum r)
        ]
layerGeomJson (LayerMasked _ _ _)      = Aeson.Null
layerGeomJson (LayerSVGOverlay _ _ _)  = Aeson.Null
layerGeomJson (LayerLabel _ _)         = Aeson.Null

-- | Structured description of the persistent fill for a layer.
-- Returns Null for layers that don't produce a filled shape.
layerFillJson :: ConstructionLayer -> Aeson.Value
layerFillJson (LayerTriangle col p1 p2 p3) =
  let (x1, y1) = toDP p1
      (x2, y2) = toDP p2
      (x3, y3) = toDP p3
   in jObj
        [ ("type",  jStr "triangle")
        , ("pts",   Aeson.Array $ fromList [jsonXY x1 y1, jsonXY x2 y2, jsonXY x3 y3])
        , ("color", jStr (colourToHex col))
        ]
layerFillJson (LayerCircle col cc ce) =
  let (cx, cy) = toDP cc
      r         = toD (pointDist cc ce)
   in jObj
        [ ("type",  jStr "circle")
        , ("cx",    jNum cx)
        , ("cy",    jNum cy)
        , ("r",     jNum r)
        , ("color", jStr (colourToHex col))
        ]
layerFillJson (LayerLabel _ _) = Aeson.Null
layerFillJson _                = Aeson.Null
