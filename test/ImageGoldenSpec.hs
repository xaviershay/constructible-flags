{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ImageGoldenSpec (imageGoldenTests) where

import Codec.Picture

import Effectful (runPureEff)
import TestImageUtils (diffImage, imagesEqual, writePngImage)
import Flag.Construction.Interpreter (eval)
import Flag.Construction.Types (Point)
import Flag.Definition (Flag (..))
import Flag.Registry (allCountryFlags)
import Flag.Render.Backend (renderDrawing)
import Flag.Render.PNGBackend (PNGBackend (..))
import Flag.Source (Sourced, runSourcedPure)
import FlagsUnderConstruction (underConstruction)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

-- | Tests that render each flag to a PNG and compare to a golden PNG.
imageGoldenTests :: TestTree
imageGoldenTests =
  testGroup
    "ImageGolden"
    [ testCase (flagIsoCode f) (goldenTestFor f)
    | f <- allCountryFlags,
      flagIsoCode f `notElem` underConstruction
    ]

-- Paths
goldenDir :: FilePath
goldenDir = "test/golden"

failureDir :: FilePath
failureDir = "test/failures"

-- | Single-flag golden test.
goldenTestFor :: Flag (Sourced : '[]) -> Assertion
goldenTestFor flag = do
  createDirectoryIfMissing True goldenDir
  createDirectoryIfMissing True failureDir

  let iso = map toLower (flagIsoCode flag)
      goldenPath = goldenDir </> (iso ++ ".png")
      tmpPath = failureDir </> (iso ++ "-candidate.png")
      failurePath = failureDir </> (iso ++ "-diff.png")
      -- Render width (match SVG width used elsewhere)
      svgWidth = 600 :: Double

  -- Resolve the FlagA arrow and evaluate on unit input
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      flagInput = ((0, 0), (1, 0)) :: (Point, Point)
      drawing = eval flagArrow flagInput

  renderDrawing PNGBackend tmpPath svgWidth drawing

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
