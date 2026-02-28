{-# LANGUAGE ScopedTypeVariables #-}

module ArbitraryRadical () where

import Test.QuickCheck
import Data.Ratio (numerator, denominator, (%))

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
  , (1, do
        -- Real value (approximate floating-point)
        d <- choose (-10.0, 10.0)
        return $ Real d
    )
  ]

instance Arbitrary Radical where
  arbitrary = sized genRadical

  shrink (Rational r)
    | r == 0    = []
    | otherwise =
        let p = numerator r
            q = denominator r
            simplerP = filter (\n -> abs n < abs p) [0, p `div` 2]
            simplerQ = filter (\d -> d > 0 && d < q) [1, q `div` 2]
        in  [Rational (p' % q) | p' <- simplerP]
         ++ [Rational (p  % q') | q' <- simplerQ]

  shrink (Ext a b r 2) =
    [a, Rational 0]
    ++ [Ext a' b r 2 | a' <- shrink a]
    ++ [Ext a b' r 2 | b' <- shrink b, not (isZero b')]
    ++ [Ext a b r' 2 | r' <- shrink r, isPositiveRat r']
    where
      isPositiveRat (Rational x) = x > 0
      isPositiveRat _            = False

  shrink (Real d)
    | d == 0    = []
    | otherwise = [Real 0, Real (d / 2)]

  shrink _ = []
