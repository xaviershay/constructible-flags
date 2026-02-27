{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Char (toLower)
import Data.List (intercalate, nub, sort)
import Data.Ratio (numerator, denominator)
import qualified Data.Text.IO as TIO
import Effectful (runPureEff)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, copyFile)
import System.FilePath ((</>), takeDirectory)
import Control.Monad (unless, when, forM_)

import Flag.Construction.Types (Point)
import Flag.Construction.Interpreter (Step, steps, evalCollectRadicals)
import Flag.Construction.Optimize (optimize)
import Flag.Construction.Radical (Radical, isNatural, isInteger, radicands)
import Flag.Construction.Tree (evalTree)
import Flag.Source (Sourced, SourcedElement, runSourcedPure, runSourcedCollect)
import Flag.Definition (Flag(..))
import Flag.Registry (allCountryFlags)
import Flag.Render.Diagram (drawingToDiagram)
import Flag.Render.Html (generateIndex, generateShowPage)
import Flag.Render.Prov (generateProvJson)
import Flag.Render.DebugV2 (writeDebugViewer, writeConstructionJson)
import Flag.Render.SVGOverlay (renderDrawingToSVG)

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
dropEditorNote :: (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h)
dropEditorNote (a,b,c,d,e,f,g,h,_) = (a,b,c,d,e,f,g,h)

buildHtml :: IO ()
buildHtml = do
  createDirectoryIfMissing True "out"
  
  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag allCountryFlags
  
  -- Generate show pages for each flag
  mapM_ (\fd@(_, _, _, iso, _, _, _, _, _) -> do
    let showHtml = generateShowPage fd
        showPath = "out/" ++ map toLower iso ++ ".html"
    writeFile showPath showHtml) flagData

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
      (drawing, intermediateRadicals) = evalCollectRadicals flagArrow flagInput
      svgOutputWidth = 300 :: Double

  -- render the optimized drawing using the shared pipeline (including
  -- SVG overlay injection) so that tests and the main executable stay in
  -- sync.
  renderDrawingToSVG svgPath svgOutputWidth drawing

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
  let fieldStr = classifyField intermediateRadicals

  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"

  pure (svgFile, flagName flag, description, flagIsoCode flag, flagUpdatedAt flag, allSources, constructionSteps, fieldStr, flagEditorNote flag)

-- | Classify the number field required by all intermediate construction points.
classifyField :: [Radical] -> String
classifyField rads =
  let allRads = nub $ sort $ concatMap radicands rads
  in if not (null allRads)
     then let extensions = map radToKaTeX allRads
          in "\\mathbb{Q}(" ++ intercalate ", " extensions ++ ")"
     else if all isNatural rads
          then "\\mathbb{N}"
          else if all isInteger rads
               then "\\mathbb{Z}"
               else "\\mathbb{Q}"
  where
    radToKaTeX (r, 2) = "\\sqrt{" ++ ratToKaTeX r ++ "}"
    radToKaTeX (r, n) = "\\sqrt[" ++ show n ++ "]{" ++ ratToKaTeX r ++ "}"
    ratToKaTeX r
      | denominator r == 1 = show (numerator r)
      | otherwise = "\\frac{" ++ show (numerator r) ++ "}{" ++ show (denominator r) ++ "}"

