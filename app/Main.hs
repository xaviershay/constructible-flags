module Main (main) where

import Lib
import Diagrams.Prelude
import Diagrams.Backend.SVG
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  createDirectoryIfMissing True "out"
  renderSVG "out/fr.svg" (mkWidth 300) frenchFlag
