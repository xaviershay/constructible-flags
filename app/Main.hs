module Main (main) where

import Lib
import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG
import Effectful
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  createDirectoryIfMissing True "out"
  let diagram = runPureEff $ runSourcedPure $ runConstructionSVG $ flagDesign france
  renderSVG "out/fra.svg" (mkWidth 300) diagram
  
  -- Demo: show the trace of construction operations
  let (_, trace) = runPureEff $ runSourcedPure $ runConstructionTrace $ flagDesign france
  putStrLn "Construction trace:"
  mapM_ (putStrLn . ("  " ++)) trace

  -- Demo: show the trace of sourced values
  let (_, sources) = runPureEff $ runSourcedTrace $ runConstructionSVG $ flagDesign france
  putStrLn "Source trace:"
  mapM_ (putStrLn . ("  " ++)) sources
