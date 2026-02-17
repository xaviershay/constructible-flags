module Main (main) where

import Test.Tasty

import qualified ConstructionSpec
import qualified GeometrySpec
import qualified RadicalSpec
import qualified NGonVertexSpec
import ImageGoldenSpec (imageGoldenTests)
import ConstructionCostSpec (constructionCostTests)
import InterpreterSpec (interpreterTests)
import OptimizeSpec (optimizeTests)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 2000000) $ testGroup "All Tests"
  [ ConstructionSpec.constructionTests
  , GeometrySpec.geometryTests
  , RadicalSpec.radicalTests
  , NGonVertexSpec.ngonVertexTests
  , imageGoldenTests
  , constructionCostTests
  , interpreterTests
  , optimizeTests
  ]
