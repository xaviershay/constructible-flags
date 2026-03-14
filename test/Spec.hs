module Main (main) where

import ConstructionCostSpec (constructionCostTests)
import qualified ConstructionSpec
import FieldNumberSpec (fieldNumberTests)
import FillOperationsSpec (fillOperationsTests)
import qualified GeometrySpec
import qualified HtmlSpec
import ImageGoldenSpec (imageGoldenTests)
import InterpreterSpec (interpreterTests)
import qualified NGonVertexSpec
import OptimizeSpec (optimizeTests)
import PruneLayersSpec (pruneLayersTests, pruneTreeTests)
import qualified SVGOverlaySpec
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    localOption (mkTimeout 2000000) $
      testGroup
        "All Tests"
        [ ConstructionSpec.constructionTests,
          GeometrySpec.geometryTests,
          NGonVertexSpec.ngonVertexTests,
          HtmlSpec.htmlTests,
          imageGoldenTests,
          constructionCostTests,
          interpreterTests,
          optimizeTests,
          pruneLayersTests,
          pruneTreeTests,
          SVGOverlaySpec.svgOverlayTests,
          fieldNumberTests,
          fillOperationsTests
        ]
