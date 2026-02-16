module PantoneSpec (pantoneTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful (runPureEff)
import Flag.Source (runSourcedPure, Source(..), mkEntity, attributeTo)
import Flag.Pantone (pantoneAgent, pantoneToRGB)
import Flag.Render.Prov (generateProvXml)
import Data.Colour.SRGB (sRGB24)
import Data.List (isInfixOf)

import Flag.Pantone (pantoneToRGB)

pantoneTests :: TestTree
pantoneTests = testGroup "Pantone lookups"
  [ testCase "pantoneToRGB by key returns expected RGB" $ do
      let c = runPureEff $ runSourcedPure $ pantoneToRGB "PMSRed032C"
      assertEqual "PMSRed032C" (sRGB24 230 49 62) c

  , testCase "pantoneToRGB by key returns expected RGB (other)" $ do
      let c = runPureEff $ runSourcedPure $ pantoneToRGB "PMS342C"
      assertEqual "PMS342C" (sRGB24 24 104 72) c

  , testCase "PROV includes color-sample activity for pantone chip" $ do
      let chipEnt = attributeTo pantoneAgent (mkEntity "PMSRed032C" "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-red-032-c.webp")
          srcs = [("RGB Conversion", SourceReference chipEnt)]
          xml = generateProvXml "XX" "Test" srcs
      assertBool "contains color-sample activity" ("color-sample-PMSRed032C" `isInfixOf` xml)
      assertBool "contains chip URL" ("pantone-color-chip-red-032-c.webp" `isInfixOf` xml)
  ]
