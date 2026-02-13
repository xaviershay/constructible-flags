{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ConstructionCostSpec (constructionCostTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful (runPureEff)
import Flag.Source (runSourcedPure, Sourced)
import Flag.Registry (allCountryFlags)
import Flag.Definition (Flag(..))
import Flag.Construction.Tree (evalTree, flattenTree)
import Flag.Construction.Types (Point)

-- | Map of expected construction step counts per flag ISO code.
-- When adding a new flag, add an entry here with the expected number
-- of geometric construction layers.
expectedCosts :: [(String, Int)]
expectedCosts =
  [ ("BGD", 55)
  , ("BWA", 78)
  , ("FRA", 35)
  , ("JPN", 29)
  ]

-- Tests to prevent performance regressions in construction.
constructionCostTests :: TestTree
constructionCostTests = testGroup "ConstructionCost"
  [ testCase (flagIsoCode f) $ do
      let iso = flagIsoCode f
          flagArrow = runPureEff $ runSourcedPure $ flagDesign f
          input = ((0,0),(1,0)) :: (Point, Point)
          (_, trees) = evalTree flagArrow input
          layers = concatMap flattenTree trees
          cost = length layers
      case lookup iso expectedCosts of
        Nothing -> assertFailure $ "No expected cost recorded for " ++ iso ++ ". Current computed cost: " ++ show cost ++ "."
        Just expected -> assertEqual ("construction cost for " ++ iso) expected cost
  | f <- allCountryFlags
  ]
