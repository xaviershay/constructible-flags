{-# LANGUAGE ScopedTypeVariables #-}

module ArbitraryRadical () where

import Test.QuickCheck
import Data.Ratio ((%))

import Flag.Construction.Radical

-- | Generate a small rational with bounded numerator/denominator.
genSmallRational :: Gen Rational
genSmallRational = do
  n <- choose (-10, 10)
  d <- choose (1, 10)
  return (n % d)

genRadical :: Int -> Gen Radical
genRadical 0 = Rational <$> genSmallRational
genRadical n = frequency
  [ (6, Rational <$> genSmallRational)
  , (3, do
        -- square-root extension a + b * r^(1/2)
        let n' = n `div` 2
        a <- genRadical n'
        b <- genRadical n'
        r <- fmap abs genSmallRational
        return $ normalize (Ext a b (Rational r) 2)
    )
  --, (1, do
  --      -- MinPolyExt using supported cosMinPoly field (use heptagon)
  --      let mp = cosMinPoly 7
  --          d = mpDegree mp
  --      cs <- vectorOf d genSmallRational
  --      return $ normalize (MinPolyExt mp cs)
  --  )
  ]

instance Arbitrary Radical where
  arbitrary = sized genRadical
  shrink _ = []
