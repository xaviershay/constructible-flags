module HtmlSpec (htmlTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Flag.Construction.Interpreter (Step(..))
import Flag.Render.Html (formatSteps)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.List (isInfixOf)

htmlTests :: TestTree
htmlTests = testGroup "Html"
  [ testCase "formatSteps shows None for empty" $
      renderHtml (formatSteps []) @?= "<em>None</em>"
  , testCase "formatSteps includes plus sign for SVG overlay" $
      let out = renderHtml (formatSteps [StepSVGOverlay])
      in assertBool "should mention plus sign" ("+ &\\times 1" `isInfixOf` out)
  ]
