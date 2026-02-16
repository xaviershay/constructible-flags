{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (die)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit (simpleHttp)
import Flag.Pantone.Cli

pantoneJsonPath :: FilePath
pantoneJsonPath = "data/pantone.json"

chipDir :: FilePath
chipDir = "data/images/pantone"

usage :: IO a
usage = die "Usage: pantone-sample add <KEY> [URL] [--force] | pantone-sample show <KEY>"

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
                    then do putStrLn ("Chip exists at " ++ outPath ++ ", use --force to overwrite"); return False
                    else return True
  when whenDownload $ do
    putStrLn $ "Downloading chip from " ++ url
    bs <- simpleHttp url
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
