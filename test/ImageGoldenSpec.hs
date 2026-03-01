{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ImageGoldenSpec (imageGoldenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (when)
import Effectful (runPureEff)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Process (callProcess)

import Codec.Picture

import Flag.Render.SVGOverlay (renderDrawingToSVG)

import Flag.Registry (allCountryFlags)
import Flag.Definition (Flag(..))
import FlagsUnderConstruction (underConstruction)
import Flag.Construction.Types (Point)
import Flag.Construction.Interpreter (eval, evalCollectNumbers)
import Flag.Source (runSourcedPure, Sourced)

-- | Tests that render each flag to a PNG and compare to a golden PNG.
imageGoldenTests :: TestTree
imageGoldenTests = testGroup "ImageGolden"
  [ testCase (flagIsoCode f) (goldenTestFor f)
  | f <- allCountryFlags
  , flagIsoCode f `notElem` underConstruction
  ]

-- Paths
goldenDir :: FilePath
goldenDir = "test/golden"

failureDir :: FilePath
failureDir = "test/failures"

tmpDir :: FilePath
tmpDir = "test/tmp"

-- | Single-flag golden test.
goldenTestFor :: Flag (Sourced : '[]) -> Assertion
goldenTestFor flag = do
  createDirectoryIfMissing True goldenDir
  createDirectoryIfMissing True failureDir
  createDirectoryIfMissing True tmpDir

  let iso = map toLower (flagIsoCode flag)
      goldenPath  = goldenDir </> (iso ++ ".png")
      tmpSvgPath  = tmpDir </> (iso ++ ".svg")
      tmpPath     = tmpDir </> (iso ++ ".png")
      failurePath = failureDir </> (iso ++ "-diff.png")
      -- Render width (match SVG width used elsewhere)
      svgWidth    = 300 :: Double

  -- Resolve the FlagA arrow and evaluate on unit input
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      flagInput = ((0, 0), (1, 0)) :: (Point, Point)
      --drawing = eval flagArrow flagInput
      (drawing, _intermediateNumbers) = evalCollectNumbers flagArrow flagInput

  renderDrawingToSVG tmpSvgPath svgWidth drawing
  callProcess "convert" [tmpSvgPath, tmpPath]

  goldenExists <- doesFileExist goldenPath
  if not goldenExists
    then do
      -- First run: write golden and fail the test so user can review and commit
      imgE <- readPng tmpPath
      case imgE of
        Left err -> assertFailure $ "Failed reading generated PNG: " ++ err
        Right dyn -> writePngImage goldenPath dyn >> assertFailure ("Golden image created at " ++ goldenPath ++ ". Review and commit to accept.")
    else do
      -- Compare images
      genE <- readPng tmpPath
      goldE <- readPng goldenPath
      case (genE, goldE) of
        (Right gen, Right gold) -> do
          let imgGen = convertRGBA8 gen
              imgGold = convertRGBA8 gold
          if imagesEqual imgGen imgGold
            then pure ()
            else do
              -- create a visual diff for inspection
              let diff = diffImage imgGold imgGen
              writePng failurePath diff
              assertFailure $ "Image mismatch for " ++ iso ++ ". Diff written to " ++ failurePath
        (Left e, _) -> assertFailure $ "Failed reading generated PNG: " ++ e
        (_, Left e) -> assertFailure $ "Failed reading golden PNG: " ++ e

-- | Convert to lower-case (helper to avoid importing Data.Char at top-level)
toLower :: Char -> Char
toLower c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

-- | Pixel-by-pixel equality check; currently exact equality.
imagesEqual :: Image PixelRGBA8 -> Image PixelRGBA8 -> Bool
imagesEqual a b = imageWidth a == imageWidth b
               && imageHeight a == imageHeight b
               && allPixelsEqual 0
  where
    allPixelsEqual !idx
      | idx >= w * h = True
      | otherwise = let x = idx `mod` w
                        y = idx `div` w
                        p1 = pixelAt a x y
                        p2 = pixelAt b x y
                    in p1 == p2 && allPixelsEqual (idx + 1)
    w = imageWidth a
    h = imageHeight a

-- | Produce a simple absolute-difference visualisation (per-channel abs diff)
diffImage :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
diffImage a b = generateImage gen w h
  where
    w = min (imageWidth a) (imageWidth b)
    h = min (imageHeight a) (imageHeight b)
    gen x y = let (PixelRGBA8 r1 g1 b1 _) = pixelAt a x y
                  (PixelRGBA8 r2 g2 b2 _) = pixelAt b x y
                  dr = abs (fromIntegral r1 - fromIntegral r2) :: Int
                  dg = abs (fromIntegral g1 - fromIntegral g2) :: Int
                  db = abs (fromIntegral b1 - fromIntegral b2) :: Int
              in PixelRGBA8 (fromIntegral dr) (fromIntegral dg) (fromIntegral db) 255

-- | Helper to write a DynamicImage to PNG (JuicyPixels helper)
writePngImage :: FilePath -> DynamicImage -> IO ()
writePngImage path img = writePng path (convertRGBA8 img)
