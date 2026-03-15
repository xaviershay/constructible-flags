{-# LANGUAGE OverloadedStrings #-}

-- Small helpers used by the `pantone-sample` executable.
module Flag.Pantone.Cli
  ( slugFromKey,
    inferChipUrl,
    sampleTopLeftRGB,
    updatePantoneJson,
  )
where

import Codec.Picture (PixelRGB8 (..), pixelAt)
import Codec.Picture.WebP (decodeRgb8)
import Data.Aeson (Value (..), decode, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist)

-- | Convert a constructor-like key (e.g. "RED-032-C") into a slug
-- suitable for Pantone chip filenames: "red-032-c".
slugFromKey :: String -> String
slugFromKey = map toLower

-- Build a canonical Pantone chip URL from a key.
inferChipUrl :: String -> String
inferChipUrl key = "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-" ++ slugFromKey key ++ ".webp"

-- Decode WebP image bytes and return top-left pixel as (r,g,b) in 0-255.
sampleTopLeftRGB :: BL.ByteString -> (Int, Int, Int)
sampleTopLeftRGB lbs =
  let img = decodeRgb8 (BL.toStrict lbs)
      PixelRGB8 r g b = pixelAt img 0 0
   in (fromIntegral r, fromIntegral g, fromIntegral b)

-- Update (or insert) an entry in data/pantone.json. Creates file if missing.
updatePantoneJson :: FilePath -> String -> (Int, Int, Int) -> String -> IO ()
updatePantoneJson pantoneJsonPath key (r, g, b) sourceUrl = do
  exists <- doesFileExist pantoneJsonPath
  content <- if exists then do c <- BL.readFile pantoneJsonPath; BL.length c `seq` return c else return "{}"
  now <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      chipPath = "images/pantone/" ++ key ++ ".webp"
      entry =
        object
          [ "label" .= key,
            "rgb" .= [r, g, b],
            "chip" .= chipPath,
            "sourceUrl" .= sourceUrl,
            "sampledAt" .= timeStr
          ]
      updated = case decode content of
        Just (Object o) -> Object (KM.insert (Key.fromString key) entry o)
        _ -> Object (KM.singleton (Key.fromString key) entry)
  BL.writeFile pantoneJsonPath (encodePretty updated)
