module Main (main) where

import Lib
import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG
import Effectful
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  createDirectoryIfMissing True "out"
  let diagram = runPureEff $ runConstructionSVG frenchFlag
  renderSVG "out/fr.svg" (mkWidth 300) diagram
  
  -- Demo: show the trace of construction operations
  let (_, trace) = runPureEff $ runConstructionTrace frenchFlag
  putStrLn "Construction trace:"
  mapM_ (putStrLn . ("  " ++)) trace
