{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ConstructionCostSpec (constructionCostTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful (runPureEff)
import Flag.Source (runSourcedPure)
import Flag.Registry (allFlags)
import Flag.Definition (Flag(..))
import Flag.Construction.Tree (evalTree, flattenTree)
import Flag.Construction.Types (Point)
import FlagsUnderConstruction (underConstruction)

-- | Map of expected construction step counts per flag ISO code.
-- When adding a new flag, add an entry here with the expected number
-- of geometric construction layers.
expectedCosts :: [(String, Int)]
expectedCosts =
  [ ("AUS", 712)
  , ("BGD", 55)
  , ("BTN", 30)
  , ("BWA", 78)
  , ("GBR", 191)
  , ("FRA", 35)
  , ("JOR", 102)
  , ("JPN", 29)
  , ("SYC", 27)
  -- Other flags
  , ("ABORIGINAL", 32)
  , ("LGBTQ", 79)
  , ("TRANS", 71)
  , ("TSI", 66)
  ]

-- Tests to prevent performance regressions in construction.
constructionCostTests :: TestTree
constructionCostTests = testGroup "ConstructionCost"
  [ testCase (flagId f) $ do
      let iso = flagId f
          flagArrow = runPureEff $ runSourcedPure $ flagDesign f
          input = ((0,0),(1,0)) :: (Point, Point)
          (_, trees) = evalTree flagArrow input
          layers = concatMap flattenTree trees
          cost = length layers
      case lookup iso expectedCosts of
        Nothing -> assertFailure $ "No expected cost recorded for " ++ iso ++ ". Current computed cost: " ++ show cost ++ "."
        Just expected -> assertEqual ("construction cost for " ++ iso) expected cost
  | f <- allFlags
  , flagId f `notElem` underConstruction
  ]
