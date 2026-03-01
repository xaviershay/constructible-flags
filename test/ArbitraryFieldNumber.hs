module ArbitraryFieldNumber () where

import Test.QuickCheck
import Data.Ratio ((%))

import Flag.Construction.FieldNumber

instance Arbitrary FieldNumber where
  arbitrary = do
    f <- arbitrary
    d <- choose (-10.0, 10.0)
    return (FieldNumber f d)

  shrink (FieldNumber f d)
    | d == 0    = []
    | otherwise = [ FieldNumber f 0
                  , FieldNumber f (d / 2)
                  ] ++ [ FieldNumber f' d | f' <- [minBound .. pred f] ]

instance Arbitrary Field where
  arbitrary = elements [minBound .. maxBound]
  shrink f  = [minBound .. pred f]
