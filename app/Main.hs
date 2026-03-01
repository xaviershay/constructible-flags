{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (nub)
import Effectful (runPureEff)
import Flag.Construction.FieldNumber (Field (..), FieldNumber, fieldOf, isInteger, isNatural)
import Flag.Construction.Interpreter (Step, evalCollectNumbers, steps)
import Flag.Construction.Tree (evalTree)
import Flag.Construction.Types (Point)
import Flag.Definition (Flag (..))
import Flag.Registry (allCountryFlags)
import Flag.Render.DebugV2 (writeConstructionJson, writeDebugViewer)
import Flag.Render.Html (generateIndex, generateShowPage)
import Flag.Render.Prov (generateProvJson)
import Flag.Render.SVGBuilderBackend (SVGBuilderBackend (..))
import Flag.Render.SVGOverlay (renderDrawingToSVG)
import Flag.Source (Sourced, SourcedElement, runSourcedCollect, runSourcedPure)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
  -- Copy static data at runtime (so `out/` always contains `data/` when serving)
  copyDirRecursive "data" "out"
  buildHtml
  writeDebugViewer
  mapM_ writeConstructionJsonForFlag allCountryFlags

writeConstructionJsonForFlag :: Flag (Sourced : '[]) -> IO ()
writeConstructionJsonForFlag flag = do
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      input = ((0, 0), (1, 0)) :: (Point, Point)
      (_, tree) = evalTree flagArrow input
  writeConstructionJson (flagName flag) (flagIsoCode flag) input tree

-- ---------------------------------------------------------------------------
-- HTML index build
-- ---------------------------------------------------------------------------

-- Helper to drop the editor note for index page
-- (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h)
dropEditorNote :: (a, b, c, d, e, f, g, h, i) -> (a, b, c, d, e, f, g, h)
dropEditorNote (a, b, c, d, e, f, g, h, _) = (a, b, c, d, e, f, g, h)

buildHtml :: IO ()
buildHtml = do
  createDirectoryIfMissing True "out"

  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag allCountryFlags

  -- Generate show pages for each flag
  mapM_
    ( \fd@(_, _, _, iso, _, _, _, _, _) -> do
        let showHtml = generateShowPage fd
            showPath = "out/" ++ map toLower iso ++ ".html"
        writeFile showPath showHtml
    )
    flagData

  -- Generate index.html
  let html = generateIndex (map dropEditorNote flagData)
  writeFile "out/index.html" html

  putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s) and index.html"

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
-- Returns: (svgFile, name, desc, isoCode, updatedAt, sources, constructionSteps, fieldStr, editorNote)
processFlag :: Flag (Sourced : '[]) -> IO (String, String, String, String, String, [SourcedElement], [Step], String, String)
processFlag flag = do
  let isoLower = map toLower (flagIsoCode flag)
      svgFile = isoLower ++ ".svg"
      svgPath = "out/" ++ svgFile

  -- Resolve the FlagA arrow (sources colours etc.)
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag

  -- Evaluate the arrow on a unit input to get the Drawing
  let flagInput = ((0, 0), (1, 0)) :: (Point, Point)
      (drawing, intermediateNumbers) = evalCollectNumbers flagArrow flagInput
      svgOutputWidth = 300 :: Double

  -- render the optimized drawing using the shared pipeline (including
  -- SVG overlay injection) so that tests and the main executable stay in
  -- sync.
  renderDrawingToSVG SVGBuilderBackend svgPath svgOutputWidth drawing

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

  -- Extract construction steps from the FlagA arrow
  let constructionSteps = steps flagArrow

  -- Determine the number field from all intermediate construction points
  let fieldStr = classifyField intermediateNumbers

  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"

  pure (svgFile, flagName flag, description, flagIsoCode flag, flagUpdatedAt flag, allSources, constructionSteps, fieldStr, flagEditorNote flag)

-- | Classify the number field required by all intermediate construction points.
classifyField :: [FieldNumber] -> String
classifyField nums
  | null nums = "\\mathbb{N}"
  | otherwise =
      let maxField = maximum (map fieldOf nums)
       in case maxField of
            FReal -> "\\mathbb{R}"
            FCyclomatic -> "\\mathbb{Q}(\\cos)"
            FIrrational -> "\\mathbb{Q}(\\sqrt{\\cdot})"
            FRational -> "\\mathbb{Q}"
            FInteger
              | all isNatural nums -> "\\mathbb{N}"
              | all isInteger nums -> "\\mathbb{Z}"
              | otherwise -> "\\mathbb{Q}"
