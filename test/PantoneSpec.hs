module PantoneSpec (pantoneTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful (runPureEff)
import Flag.Source (runSourcedPure)
import Data.Colour.SRGB (sRGB24)

import Flag.Pantone (PantoneId(..), pmsToRGB, pantoneToRGB)

pantoneTests :: TestTree
pantoneTests = testGroup "Pantone lookups"
  [ testCase "pmsToRGB (PMSRed032C) returns expected RGB" $ do
      let c = runPureEff $ runSourcedPure $ pmsToRGB PMSRed032C
      assertEqual "PMSRed032C" (sRGB24 230 49 62) c

  , testCase "pantoneToRGB by key returns expected RGB" $ do
      let c = runPureEff $ runSourcedPure $ pantoneToRGB "PMS342C"
      assertEqual "PMS342C" (sRGB24 24 104 72) c
  ]
