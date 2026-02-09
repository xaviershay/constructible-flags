{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Char (toLower)
import Data.List (nub)
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude (mkWidth)
import Effectful (runPureEff)
import System.Directory (createDirectoryIfMissing)

import Flag.Construction.Types (Point)
import Flag.Construction.Interpreter (Step, steps, eval)
import Flag.Construction.Optimize (optimize)
import Flag.Source (Sourced, SourcedElement, runSourcedPure, runSourcedCollect)
import Flag.Definition (Flag(..))
import Flag.Registry (allCountryFlags, japan)
import Flag.Render.Diagram (drawingToDiagram)
import Flag.Render.Html (generateIndex)
import Flag.Render.Debug (buildDebug)
import Flag.Render.DebugV2 (buildDebugV2)

main :: IO ()
--main = buildHtml
main = do
  buildHtml
  buildDebug japan
  buildDebugV2 japan

-- ---------------------------------------------------------------------------
-- HTML index build
-- ---------------------------------------------------------------------------

buildHtml :: IO ()
buildHtml = do
  createDirectoryIfMissing True "out"
  
  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag allCountryFlags
  
  -- Generate index.html
  let html = generateIndex flagData
  writeFile "out/index.html" html
  
  putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s) and index.html"

-- | Process a single flag: render SVG and extract metadata
processFlag :: Flag (Sourced : '[]) -> IO (String, String, String, String, [SourcedElement], [Step])
processFlag flag = do
  let isoLower = map toLower (flagIsoCode flag)
      svgFile = isoLower ++ ".svg"
      svgPath = "out/" ++ svgFile
      
  -- Resolve the FlagA arrow (sources colours etc.)
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
  
  -- Evaluate the arrow on a unit input to get the Drawing
  let flagInput = ((0, 0), (1, 0)) :: (Point, Point)
      drawing = eval flagArrow flagInput
      diagram = drawingToDiagram (optimize drawing)
  renderSVG svgPath (mkWidth 300) diagram
  
  -- Get description
  let description = runPureEff $ runSourcedPure $ flagDescription flag
  
  -- Collect sources from design and description
  let (_, designSources) = runPureEff $ runSourcedCollect $ flagDesign flag
  let (_, descSources) = runPureEff $ runSourcedCollect $ flagDescription flag
  let allSources = nub (designSources ++ descSources)
  
  -- Extract construction steps from the FlagA arrow
  let constructionSteps = steps flagArrow
  
  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"
  
  pure (svgFile, flagName flag, description, flagIsoCode flag, allSources, constructionSteps)
