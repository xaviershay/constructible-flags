{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (die)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit (Request(..), httpLbs, newManager, tlsManagerSettings, parseRequest, responseBody)
import Flag.Pantone.Cli

pantoneJsonPath :: FilePath
pantoneJsonPath = "data/pantone.json"

chipDir :: FilePath
chipDir = "data/images/pantone"

usage :: IO a
usage = die $ unlines
  [ "Usage: pantone-sample add <KEY> [URL] [--force] | pantone-sample show <KEY>"
  , ""
  , "If pantone.com blocks the download, save the chip by hand to"
  , "  data/images/pantone/<KEY>.webp"
  , "then run 'add' again (without --force) — it will skip the fetch and"
  , "sample colours / update data/pantone.json from the file already there."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("add":key:rest) -> cmdAdd key rest
    ("show":key:_) -> cmdShow key
    _ -> usage

cmdShow :: String -> IO ()
cmdShow key = do
  putStrLn $ "Key: " ++ key
  putStrLn $ "Inferred URL: " ++ inferChipUrl key

cmdAdd :: String -> [String] -> IO ()
cmdAdd key rest = do
  let (mUrl, force) = parseRest rest
      url = case mUrl of
              Just u -> u
              Nothing -> inferChipUrl key
      outPath = chipDir </> key ++ ".webp"
  createDirectoryIfMissing True chipDir
  exists <- doesFileExist outPath
  whenDownload <- if exists && not force
                    then do putStrLn ("Chip already at " ++ outPath ++ ", skipping download and sampling from it (pass --force to redownload)"); return False
                    else return True
  when whenDownload $ do
    putStrLn $ "Downloading chip from " ++ url
    bs <- fetchAsBrowser url
    BL.writeFile outPath bs
    putStrLn $ "Wrote chip to " ++ outPath
  bs <- BL.readFile outPath
  let rgb = sampleTopLeftRGB bs
  updatePantoneJson pantoneJsonPath key rgb url
  putStrLn $ "Updated " ++ pantoneJsonPath ++ " — " ++ show rgb

  where
    parseRest xs = (lookupUrl xs, "--force" `elem` xs)
    lookupUrl (u:_) | not ("--" `elem` [u]) = Just u
    lookupUrl _ = Nothing

-- | Fetch a URL with headers matching a real browser navigating from
-- pantone.com, since the site blocks plain scripted requests.
fetchAsBrowser :: String -> IO BL.ByteString
fetchAsBrowser url = do
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest url
  let req = req0 { requestHeaders = browserHeaders }
  responseBody <$> httpLbs req manager

browserHeaders =
  [ ("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36")
  , ("Accept", "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8")
  , ("Accept-Language", "en-US,en;q=0.9")
  , ("Referer", "https://www.pantone.com/")
  , ("Sec-Fetch-Dest", "image")
  , ("Sec-Fetch-Mode", "no-cors")
  , ("Sec-Fetch-Site", "same-origin")
  ]
