{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (unless)
import Data.Char (toLower, toUpper)
import Effectful (runPureEff)
import Flag.Construction.Interpreter (eval)
import Flag.Construction.Tree (evalTree)
import Flag.Construction.Types (Point)
import Flag.Definition (Flag (..))
import Flag.Registry (allCountryFlags)
import Flag.Render.Animation
  ( AnimationConfig (..),
    OutputFormat (..),
    buildFrames,
    defaultAnimationConfig,
    encodeAnimation,
    layersForAnimation,
    outputExtension,
    pathsForAnimation,
    writeAllFrames,
  )
import Flag.Source (runSourcedPure)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    removeDirectoryRecursive,
  )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((<.>), (</>))

-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

data Options = Options
  { optIso :: String,
    optFormat :: OutputFormat,
    optKeepFrames :: Bool,
    optAnim :: AnimationConfig
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optIso = "",
      optFormat = FmtGif,
      optKeepFrames = False,
      optAnim = defaultAnimationConfig
    }

usage :: IO a
usage =
  die $
    unlines
      [ "Usage: animate <ISO> [options]",
        "",
        "Generate an animated movie of the construction of a flag.",
        "",
        "Options:",
        "  --format FMT          One of: gif, mp4, webm, apng, webp (default: gif)",
        "  --width N             Pixel width of the output (default: 600)",
        "  --fps N               Frames per second (default: 12)",
        "  --frames-per-step N   Frames per construction step (default: 6)",
        "  --hold-frames N       Trailing frames showing only the final flag (default: 36)",
        "  --trail-steps N       How many previous steps' geometry to show faintly (default: 1)",
        "  --keep-frames         Don't delete the per-frame scratch directory after encoding"
      ]

parseFormat :: String -> Maybe OutputFormat
parseFormat = \case
  "gif" -> Just FmtGif
  "mp4" -> Just FmtMp4
  "webm" -> Just FmtWebm
  "apng" -> Just FmtApng
  "webp" -> Just FmtWebp
  _ -> Nothing

readPositiveInt :: String -> String -> IO Int
readPositiveInt name s = case reads s of
  [(n, "")] | n > 0 -> pure n
  _ -> die $ "Expected a positive integer for " ++ name ++ ", got: " ++ s

readNonNegativeInt :: String -> String -> IO Int
readNonNegativeInt name s = case reads s of
  [(n, "")] | n >= 0 -> pure n
  _ -> die $ "Expected a non-negative integer for " ++ name ++ ", got: " ++ s

readPositiveDouble :: String -> String -> IO Double
readPositiveDouble name s = case reads s of
  [(n, "")] | n > 0 -> pure n
  _ -> die $ "Expected a positive number for " ++ name ++ ", got: " ++ s

parseArgs :: [String] -> IO Options
parseArgs = go defaultOptions
  where
    go opts [] = case optIso opts of
      "" -> usage
      _ -> pure opts
    go opts (a : rest) = case a of
      "--format" -> withArg "--format" rest $ \v rest' -> case parseFormat v of
        Just f -> go opts {optFormat = f} rest'
        Nothing -> die $ "Unknown format: " ++ v
      "--width" -> withArg "--width" rest $ \v rest' -> do
        n <- readPositiveDouble "--width" v
        go opts {optAnim = (optAnim opts) {acWidth = n}} rest'
      "--fps" -> withArg "--fps" rest $ \v rest' -> do
        n <- readPositiveInt "--fps" v
        go opts {optAnim = (optAnim opts) {acFps = n}} rest'
      "--frames-per-step" -> withArg "--frames-per-step" rest $ \v rest' -> do
        n <- readPositiveInt "--frames-per-step" v
        go opts {optAnim = (optAnim opts) {acFramesPerStep = n}} rest'
      "--hold-frames" -> withArg "--hold-frames" rest $ \v rest' -> do
        n <- readNonNegativeInt "--hold-frames" v
        go opts {optAnim = (optAnim opts) {acHoldFrames = n}} rest'
      "--trail-steps" -> withArg "--trail-steps" rest $ \v rest' -> do
        n <- readNonNegativeInt "--trail-steps" v
        go opts {optAnim = (optAnim opts) {acTrailSteps = n}} rest'
      "--keep-frames" -> go opts {optKeepFrames = True} rest
      "-h" -> usage
      "--help" -> usage
      _ | take 2 a == "--" -> die $ "Unknown option: " ++ a
      _ | optIso opts == "" -> go opts {optIso = map toUpper a} rest
      _ -> die $ "Unexpected positional argument: " ++ a

    withArg name args k = case args of
      (v : rest') -> k v rest'
      _ -> die $ name ++ " requires an argument"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- getArgs >>= parseArgs

  flag <- case filter (\f -> map toUpper (flagIsoCode f) == optIso opts) allCountryFlags of
    [f] -> pure f
    [] -> die $ "No flag found with ISO code: " ++ optIso opts
    _ -> die $ "Ambiguous ISO code: " ++ optIso opts

  let isoLower = map toLower (flagIsoCode flag)
      flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      input = ((0, 0), (1, 0)) :: (Point, Point)
      drawing = eval flagArrow input
      (_, tree) = evalTree flagArrow input
      layers = layersForAnimation tree
      paths = pathsForAnimation tree
      cfg = optAnim opts
      frames = buildFrames cfg drawing layers paths
      framesDir = "tmp" </> "animation" </> isoLower
      outDir = "out" </> "animation"
      outPath = outDir </> isoLower <.> outputExtension (optFormat opts)

  createDirectoryIfMissing True outDir

  putStrLn $
    "Building "
      ++ show (length frames)
      ++ " frames for "
      ++ flagName flag
      ++ " ("
      ++ show (length layers)
      ++ " construction steps)"

  -- Clean any stale frames from a previous run so frame-NNNNN.png patterns
  -- don't pick up leftover files from a longer earlier animation.
  staleExists <- doesDirectoryExist framesDir
  if staleExists
    then removeDirectoryRecursive framesDir
    else pure ()

  _ <- writeAllFrames cfg framesDir frames

  putStrLn $ "Encoding " ++ outputExtension (optFormat opts) ++ " to " ++ outPath
  encodeAnimation cfg (optFormat opts) framesDir outPath
  putStrLn $ "Wrote " ++ outPath

  unless (optKeepFrames opts) $ do
    exists <- doesDirectoryExist framesDir
    if exists then removeDirectoryRecursive framesDir else pure ()
