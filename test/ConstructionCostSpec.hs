{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ConstructionCostSpec (constructionCostTests) where

import Effectful (runPureEff)
import Flag.Construction.Tree (evalTree, prunedSteps)
import Flag.Construction.Types (Point)
import Flag.Definition (Flag (..))
import Flag.Registry (allCountryFlags)
import Flag.Source (Sourced, runSourcedPure)
import FlagsUnderConstruction (underConstruction)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

-- | Tests to prevent performance regressions in construction.
-- On first run for a flag, the computed cost is written to a golden file and
-- the test fails so the value can be reviewed and committed.
-- On subsequent runs the recorded value is used for comparison.
constructionCostTests :: TestTree
constructionCostTests =
  testGroup
    "ConstructionCost"
    [ testCase (flagIsoCode f) (goldenCostTestFor f)
    | f <- allCountryFlags,
      flagIsoCode f `notElem` underConstruction
    ]

-- Paths

goldenDir :: FilePath
goldenDir = "test/golden"

goldenPathFor :: String -> FilePath
goldenPathFor iso = goldenDir </> (map toLower iso ++ ".cost")

-- | Single-flag golden cost test.
goldenCostTestFor :: Flag (Sourced : '[]) -> Assertion
goldenCostTestFor flag = do
  createDirectoryIfMissing True goldenDir

  let iso = flagIsoCode flag
      flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      input = ((0, 0), (1, 0)) :: (Point, Point)
      (_, trees) = evalTree flagArrow input
      cost = length (prunedSteps trees)
      path = goldenPathFor iso

  goldenExists <- doesFileExist path
  if not goldenExists
    then do
      writeFile path (show cost)
      assertFailure $
        "Golden cost created for " ++ iso ++ " (" ++ show cost ++ ") at " ++ path ++ ". Review and commit to accept."
    else do
      recorded <- readFile path
      case reads recorded of
        [(expected, "")] ->
          assertEqual ("construction cost for " ++ iso) (expected :: Int) cost
        _ ->
          assertFailure $ "Could not parse golden cost file " ++ path ++ ": " ++ show recorded

-- | Convert to lower-case without importing Data.Char
toLower :: Char -> Char
toLower c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c
