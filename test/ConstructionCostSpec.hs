{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ConstructionCostSpec (constructionCostTests) where

import Effectful (runPureEff)
import Flag.Construction.Tree (evalTree, prunedSteps)
import Flag.Construction.Types (Point)
import Flag.Definition (Flag (..))
import Flag.Registry (allCountryFlags)
import Flag.Source (runSourcedPure)
import FlagsUnderConstruction (underConstruction)
import Test.Tasty
import Test.Tasty.HUnit

-- | Map of expected construction step counts per flag ISO code.
-- Only geometric steps (IntersectLL, IntersectLC, IntersectCC, NGonVertex)
-- are counted — drawing primitives (fills, overlays) and labels are excluded.
-- Counts reflect the pruned tree (duplicate outputs and dead computations
-- are removed before counting).
expectedCosts :: [(String, Int)]
expectedCosts =
  [ ("AUS", 442),
    ("BGD", 46),
    ("BTN", 21),
    ("BWA", 61),
    ("DZA", 85),
    ("FRA", 19),
    ("GBR", 155),
    ("JOR", 65),
    ("JPN", 24),
    ("MHL", 167),
    ("SYC", 18)
  ]

-- Tests to prevent performance regressions in construction.
constructionCostTests :: TestTree
constructionCostTests =
  testGroup
    "ConstructionCost"
    [ testCase (flagIsoCode f) $ do
        let iso = flagIsoCode f
            flagArrow = runPureEff $ runSourcedPure $ flagDesign f
            input = ((0, 0), (1, 0)) :: (Point, Point)
            (_, trees) = evalTree flagArrow input
            cost = length (prunedSteps trees)
        case lookup iso expectedCosts of
          Nothing -> assertFailure $ "No expected cost recorded for " ++ iso ++ ". Current computed cost: " ++ show cost ++ "."
          Just expected -> assertEqual ("construction cost for " ++ iso) expected cost
    | f <- allCountryFlags,
      flagIsoCode f `notElem` underConstruction
    ]
