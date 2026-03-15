{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Char (toLower, toUpper)
import Data.List (nub)
import Effectful (runPureEff)
import Flag.Construction.Interpreter (Step, eval, evalLabels)
import Flag.Construction.Tree (evalTree, prunedSteps)
import Flag.Construction.Types (Point)
import Flag.Definition (Flag (..))
import Flag.Registry (allCountryFlags)
import Flag.Render.Backend (renderDrawing)
import Flag.Render.DebugV2 (writeConstructionJson, writeDebugViewer)
import Flag.Render.Html (generateConstructionPage, generateIndex, generateShowPage)
import Flag.Render.Prov (generateProvJson)
import Flag.Render.SVGBuilderBackend (SVGBuilderBackend (..))
import Flag.Source (Sourced, SourcedElement, runSourcedCollect, runSourcedPure)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
  args <- getArgs
  mFilter <- parseArgs args
  -- Copy static data at runtime (so `out/` always contains `data/` when serving)
  copyDirRecursive "data" "out"
  let flags = filterFlags mFilter allCountryFlags
  buildHtml mFilter flags
  writeDebugViewer
  mapM_ writeConstructionJsonForFlag flags

-- | Parse command line arguments, returning the ISO code filter if -f was given.
parseArgs :: [String] -> IO (Maybe String)
parseArgs ("-f" : code : _) = pure (Just (map toUpper code))
parseArgs ("--flag" : code : _) = pure (Just (map toUpper code))
parseArgs ("-f" : []) = do
  putStrLn "Error: -f requires a flag ISO code argument"
  exitFailure
parseArgs ("--flag" : []) = do
  putStrLn "Error: --flag requires a flag ISO code argument"
  exitFailure
parseArgs (_ : rest) = parseArgs rest
parseArgs [] = pure Nothing

-- | Filter the flag list by ISO code, or return all flags if no filter.
filterFlags :: Maybe String -> [Flag (Sourced : '[])] -> [Flag (Sourced : '[])]
filterFlags Nothing flags = flags
filterFlags (Just code) flags =
  case filter (\f -> map toUpper (flagIsoCode f) == code) flags of
    [] -> error $ "No flag found with ISO code: " ++ code
    matched -> matched

writeConstructionJsonForFlag :: Flag (Sourced : '[]) -> IO ()
writeConstructionJsonForFlag flag = do
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      input = ((0, 0), (1, 0)) :: (Point, Point)
      (_, tree) = evalTree flagArrow input
      labels = evalLabels flagArrow input
  writeConstructionJson (flagName flag) (flagIsoCode flag) input tree labels

-- ---------------------------------------------------------------------------
-- HTML index build
-- ---------------------------------------------------------------------------

-- Helper to drop the editor note for index page
-- (a,b,c,d,e,f,g,h) -> (a,b,c,d,e,f,g)
dropEditorNote :: (a, b, c, d, e, f, g, h) -> (a, b, c, d, e, f, g)
dropEditorNote (a, b, c, d, e, f, g, _) = (a, b, c, d, e, f, g)

buildHtml :: Maybe String -> [Flag (Sourced : '[])] -> IO ()
buildHtml mFilter flags = do
  createDirectoryIfMissing True "out"

  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag flags

  -- Generate show pages for each flag
  mapM_
    ( \fd@(_, _, _, iso, _, _, _, _) -> do
        let showHtml = generateShowPage fd
            showPath = "out/" ++ map toLower iso ++ ".html"
        writeFile showPath showHtml
    )
    flagData

  -- Only regenerate the index and construction pages when doing a full build
  case mFilter of
    Just code ->
      putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s) for filter: " ++ code ++ " (skipped index.html)"
    Nothing -> do
      -- Generate index.html
      let html = generateIndex (map dropEditorNote flagData)
      writeFile "out/index.html" html

      -- Generate construction.html
      writeFile "out/construction.html" generateConstructionPage

      putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s), index.html, and construction.html"

-- | Recursively copy directory contents from src to dst
copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
  exists <- doesDirectoryExist src
  unless exists $ putStrLn $ "Warning: source directory " ++ src ++ " does not exist"
  when exists $ do
    createDirectoryIfMissing True dst
    entries <- listDirectory src
    forM_ entries $ \e -> do
      let s = src </> e
          d = dst </> e
      isDir <- doesDirectoryExist s
      if isDir
        then copyDirRecursive s d
        else do
          createDirectoryIfMissing True (takeDirectory d)
          copyFile s d
    putStrLn $ "Copied " ++ src ++ " -> " ++ dst

-- | Process a single flag: render SVG, generate PROV XML, and extract metadata
-- Returns: (svgFile, name, desc, isoCode, updatedAt, sources, constructionSteps, editorNote)
processFlag :: Flag (Sourced : '[]) -> IO (String, String, String, String, String, [SourcedElement], [Step], String)
processFlag flag = do
  let isoLower = map toLower (flagIsoCode flag)
      svgFile = isoLower ++ ".svg"
      svgPath = "out/" ++ svgFile

  -- Resolve the FlagA arrow (sources colours etc.)
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag

  -- Evaluate the arrow on a unit input to get the Drawing
  let flagInput = ((0, 0), (1, 0)) :: (Point, Point)
      drawing = eval flagArrow flagInput
      svgOutputWidth = 300 :: Double

  -- render the optimized drawing using the shared pipeline (including
  -- SVG overlay injection) so that tests and the main executable stay in
  -- sync.
  renderDrawing SVGBuilderBackend svgPath svgOutputWidth drawing

  -- Get description
  let description = runPureEff $ runSourcedPure $ flagDescription flag

  -- Collect sources from design and description
  let (_, designSources) = runPureEff $ runSourcedCollect $ flagDesign flag
  let (_, descSources) = runPureEff $ runSourcedCollect $ flagDescription flag
  let allSources = nub (designSources ++ descSources)

  -- Generate PROV JSON
  let provFile = isoLower ++ "-prov.json"
      provPath = "out/" ++ provFile
      provJson = generateProvJson (flagIsoCode flag) (flagName flag) allSources
  writeFile provPath provJson

  -- Extract construction steps from the pruned tree, so the cost and
  -- breakdown shown on the flag page match what the debug viewer displays.
  let (_, tree) = evalTree flagArrow flagInput
      constructionSteps = prunedSteps tree

  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"

  pure (svgFile, flagName flag, description, flagIsoCode flag, flagUpdatedAt flag, allSources, constructionSteps, flagEditorNote flag)
