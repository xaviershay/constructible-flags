{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Char (toLower)
import Data.List (intercalate, nub, sort)
import Data.Ratio (numerator, denominator)
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude (mkWidth)
import Effectful (runPureEff)
import System.Directory (createDirectoryIfMissing)

import Flag.Construction.Types (Point)
import Flag.Construction.Interpreter (Step, steps, evalCollectRadicals)
import Flag.Construction.Optimize (optimize)
import Flag.Construction.Radical (Radical, isNatural, isInteger, radicands)
import Flag.Source (Sourced, SourcedElement, runSourcedPure, runSourcedCollect)
import Flag.Definition (Flag(..))
import Flag.Registry (allCountryFlags)
import Flag.Render.Diagram (drawingToDiagram)
import Flag.Render.Html (generateIndex, generateShowPage)
import Flag.Render.Prov (generateProvXml)
import Flag.Render.DebugV2 (writeDebugViewer, writeConstructionJson)

main :: IO ()
main = do
  buildHtml
  writeDebugViewer
  mapM_ writeConstructionJson allCountryFlags

-- ---------------------------------------------------------------------------
-- HTML index build
-- ---------------------------------------------------------------------------

buildHtml :: IO ()
buildHtml = do
  createDirectoryIfMissing True "out"
  
  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag allCountryFlags
  
  -- Generate show pages for each flag
  mapM_ (\fd@(_, _, _, iso, _, _, _) -> do
    let showHtml = generateShowPage fd
        showPath = "out/" ++ map toLower iso ++ ".html"
    writeFile showPath showHtml) flagData

  -- Generate index.html
  let html = generateIndex flagData
  writeFile "out/index.html" html
  
  putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s) and index.html"

-- | Process a single flag: render SVG, generate PROV XML, and extract metadata
processFlag :: Flag (Sourced : '[]) -> IO (String, String, String, String, [SourcedElement], [Step], String)
processFlag flag = do
  let isoLower = map toLower (flagIsoCode flag)
      svgFile = isoLower ++ ".svg"
      svgPath = "out/" ++ svgFile

  -- Resolve the FlagA arrow (sources colours etc.)
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag

  -- Evaluate the arrow on a unit input to get the Drawing
  let flagInput = ((0, 0), (1, 0)) :: (Point, Point)
      (drawing, intermediateRadicals) = evalCollectRadicals flagArrow flagInput
      diagram = drawingToDiagram (optimize drawing)
  renderSVG svgPath (mkWidth 300) diagram

  -- Get description
  let description = runPureEff $ runSourcedPure $ flagDescription flag

  -- Collect sources from design and description
  let (_, designSources) = runPureEff $ runSourcedCollect $ flagDesign flag
  let (_, descSources) = runPureEff $ runSourcedCollect $ flagDescription flag
  let allSources = nub (designSources ++ descSources)

  -- Generate PROV XML
  let provFile = isoLower ++ "-prov.xml"
      provPath = "out/" ++ provFile
      provXml = generateProvXml (flagIsoCode flag) (flagName flag) allSources
  writeFile provPath provXml

  -- Extract construction steps from the FlagA arrow
  let constructionSteps = steps flagArrow

  -- Determine the number field from all intermediate construction points
  let fieldStr = classifyField intermediateRadicals

  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"

  pure (svgFile, flagName flag, description, flagIsoCode flag, allSources, constructionSteps, fieldStr)

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

