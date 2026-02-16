module PantoneCliSpec (pantoneCliTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BL
import Codec.Picture (generateImage, PixelRGB8(..))
import Codec.Picture.WebP (encodeRgb8Lossless)

import Flag.Pantone.Cli (slugFromKey, inferChipUrl, sampleTopLeftRGB)

pantoneCliTests :: TestTree
pantoneCliTests = testGroup "Pantone CLI helpers"
  [ testCase "slugFromKey examples" $ do
      slugFromKey "PMSRed032C" @?= "red-032-c"
      slugFromKey "PMSReflexBlueC" @?= "reflex-blue-c"
      slugFromKey "PMS154225TCX" @?= "154225-tcx"

  , testCase "inferChipUrl builds expected URL" $ do
      inferChipUrl "PMSRed032C" @?= "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-red-032-c.webp"

  , testCase "sampleTopLeftRGB decodes WebP bytes and returns top-left pixel" $ do
      -- Build a 2x2 WebP with color (10,20,30)
      let img = generateImage (\_ _ -> PixelRGB8 10 20 30) 2 2
          bs = BL.fromStrict (encodeRgb8Lossless img)
          (r,g,b) = sampleTopLeftRGB bs
      (r,g,b) @?= (10,20,30)
  ]
