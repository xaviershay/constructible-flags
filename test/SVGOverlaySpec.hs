module SVGOverlaySpec (svgOverlayTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Flag.Render.SVGOverlay
import Flag.Construction.Types (Drawing(..))

import Control.Exception (try, SomeException)

svgOverlayTests :: TestTree
svgOverlayTests = testGroup "SVGOverlay"
  [ testCase "parseSVG falls back to viewBox" $ do
      let svgText = "<svg viewBox=\"0 0 100 50\"><rect/></svg>"
      case parseSVG (T.pack svgText) of
        Left err -> assertFailure ("parseSVG failed: " ++ err)
        Right src -> do
            overlayWidth src @?= 100
            overlayHeight src @?= 50

  , testCase "loadOverlaySources throws on missing file" $ do
      let drawing = DrawSVGOverlay "nonexistent.svg" (0,0) (1,0)
      r <- try (loadOverlaySources drawing) :: IO (Either SomeException (Map.Map FilePath OverlaySource))
      case r of
        Left _ -> assertBool "expected exception" True
        Right _ -> assertFailure "expected failure when file missing"

  ]
