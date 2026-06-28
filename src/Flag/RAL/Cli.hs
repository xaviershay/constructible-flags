{-# LANGUAGE OverloadedStrings #-}

module Flag.RAL.Cli
  ( updateRALJson
  )
where

import Data.Aeson (Value (..), decode, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist)

-- | Update (or insert) an entry in data/ral.json.
updateRALJson :: FilePath -> String -> (Int, Int, Int) -> IO ()
updateRALJson ralJsonPath code (r, g, b) = do
  exists <- doesFileExist ralJsonPath
  content <-
    if exists
      then do c <- BL.readFile ralJsonPath; BL.length c `seq` return c
      else return "{}"
  now <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      chipPath = "images/ral/ral-" ++ code ++ ".png"
      sourceUrl = "https://qconv.com/en/colors/ral-" ++ code
      entry =
        object
          [ "label" .= ("RAL " ++ code)
          , "rgb" .= [r, g, b]
          , "chip" .= chipPath
          , "sourceUrl" .= sourceUrl
          , "sampledAt" .= timeStr
          ]
      updated = case decode content of
        Just (Object o) -> Object (KM.insert (Key.fromString code) entry o)
        _ -> Object (KM.singleton (Key.fromString code) entry)
  BL.writeFile ralJsonPath (encodePretty updated)
