module Main (main) where

import Test.Tasty

import qualified ConstructionSpec
import qualified GeometrySpec
import qualified NGonVertexSpec
import ImageGoldenSpec (imageGoldenTests)
import ConstructionCostSpec (constructionCostTests)
import InterpreterSpec (interpreterTests)
import OptimizeSpec (optimizeTests)
import qualified SVGOverlaySpec
import qualified HtmlSpec
import FieldNumberSpec (fieldNumberTests)
import FillOperationsSpec (fillOperationsTests)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 2000000) $ testGroup "All Tests"
  [ ConstructionSpec.constructionTests
  , GeometrySpec.geometryTests
  , NGonVertexSpec.ngonVertexTests
  , HtmlSpec.htmlTests
  , imageGoldenTests
  , constructionCostTests
  , interpreterTests
  , optimizeTests
  , SVGOverlaySpec.svgOverlayTests
  , fieldNumberTests
  , fillOperationsTests
  ]
