{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.JSONBackend
  ( JSONBackend (..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Flag.Construction.FieldNumber (toDouble)
import Flag.Construction.Optimize (optimize)
import Flag.Construction.Types (Drawing (..))
import Flag.Render.Backend (RenderBackend (..))
import Flag.Render.JSONHelpers
  ( colourToHex
  , fromList
  , jNum
  , jObj
  , jStr
  , jsonXY
  )

-- | A rendering backend that serialises a 'Drawing' to a JSON array of fill
-- objects, one per leaf layer.  SVG overlay placements are omitted — they are
-- not meaningful in a JSON representation.
data JSONBackend = JSONBackend

instance RenderBackend JSONBackend where
    -- | Render a 'Drawing' to a JSON file.
    --
    -- Optimises the drawing, flattens it to a list of fill descriptions, and
    -- writes the resulting JSON array to @outPath@.  The @width@ argument is
    -- ignored — it is not meaningful for JSON output.
    -- SVG overlay injection is skipped — overlays are an SVG-specific concern.
    renderDrawing _ outPath _width drawing =
        BL.writeFile outPath
            (Aeson.encode (Aeson.Array (fromList (drawingToJson (optimize drawing)))))

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Recursively flatten a 'Drawing' into a list of JSON fill objects.
-- Structural nodes are flattened; each filled leaf produces one object.
-- 'DrawSVGOverlay' is skipped — not meaningful in JSON output.
drawingToJson :: Drawing -> [Aeson.Value]
drawingToJson EmptyDrawing             = []
drawingToJson (Overlay a b)            = drawingToJson a ++ drawingToJson b
drawingToJson (DrawMasked _ a _)       = drawingToJson a
drawingToJson (DrawSVGOverlay _ _ _)   = []
drawingToJson (DrawTriangle col p1 p2 p3) = [drawingToJson' (DrawPath col [p1, p2, p3])]
drawingToJson d                        = [drawingToJson' d]

-- | Encode a single filled leaf 'Drawing' as a JSON object.
drawingToJson' :: Drawing -> Aeson.Value
drawingToJson' (DrawPath col pts) =
    let xys = map (\(x, y) -> (toDouble x, toDouble y)) pts
    in  jObj
            [ ("type",  jStr "polygon")
            , ("pts",   Aeson.Array $ fromList [jsonXY x y | (x, y) <- xys])
            , ("color", jStr (colourToHex col))
            ]
drawingToJson' (DrawCircle col center rd) =
    let (cx, cy) = (toDouble (fst center), toDouble (snd center))
        r        = toDouble rd
    in  jObj
            [ ("type",  jStr "circle")
            , ("cx",    jNum cx)
            , ("cy",    jNum cy)
            , ("r",     jNum r)
            , ("color", jStr (colourToHex col))
            ]
drawingToJson' _ = Aeson.Null
