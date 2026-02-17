{-# LANGUAGE ScopedTypeVariables #-}

module GeometrySpec (geometryTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Flag.Construction.Types (Point)
import Flag.Construction.Geometry (evalIntersectLL')
import Flag.Construction.Radical (Radical(..), toDouble)
import Data.Ratio ((%))
import Control.Exception (evaluate, try, SomeException)

-- | Approximate equality for points, converting Radical to Double.
approxEqual :: String -> Point -> Point -> Assertion
approxEqual msg (x1, y1) (x2, y2) = do
  assertBool (msg ++ " x: expected " ++ show x1 ++ " got " ++ show x2)
    (abs (toDouble x1 - toDouble x2) < 1e-9)
  assertBool (msg ++ " y: expected " ++ show y1 ++ " got " ++ show y2)
    (abs (toDouble y1 - toDouble y2) < 1e-9)

geometryTests :: TestTree
geometryTests = testGroup "Geometry"
  [ testGroup "intersectLL"
    [ testCase "horizontal and vertical lines intersect at origin" $
        approxEqual "hv" (0, 0) (evalIntersectLL' (((0,0),(1,0)), ((0,0),(0,1))))

    , testCase "diagonal lines intersect at (1/2,1/2)" $
        approxEqual "diag" (0.5, 0.5) (evalIntersectLL' (((0,0),(1,1)), ((0,1),(1,0))))

    , testCase "parallel lines error (division by zero)" $ do
        r <- try (evaluate (evalIntersectLL' (((0,0),(1,0)), ((0,1),(1,1))))) :: IO (Either SomeException (Radical, Radical))
        case r of
          Left _ -> assertBool "caught expected exception" True
          Right v -> assertFailure ("expected exception, got: " ++ show v)
    ]
  , testGroup "IntersectCC diagnostics"
    [ localOption (mkTimeout 100000) $ testCase "xdiff (x2 - x1) evaluates quickly" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            xdiff = c2x - c1x
        _ <- evaluate (toDouble xdiff)
        assertBool "xdiff ok" True

    , localOption (mkTimeout 100000) $ testCase "ydiff (y2 - y1) evaluates quickly" $ do
        let c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            ydiff = c2y - c1y
        _ <- evaluate (toDouble ydiff)
        assertBool "ydiff ok" True

    , localOption (mkTimeout 100000) $ testCase "x2 = xdiff^2" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            x2 = (c2x - c1x) * (c2x - c1x)
        _ <- evaluate (toDouble x2)
        assertBool "x2 ok" True

    , localOption (mkTimeout 100000) $ testCase "y2 = ydiff^2" $ do
        let c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            y2 = (c2y - c1y) * (c2y - c1y)
        _ <- evaluate (toDouble y2)
        assertBool "y2 ok" True

    , localOption (mkTimeout 100000) $ testCase "sum = x2 + y2" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            x2 = (c2x - c1x) * (c2x - c1x)
            y2 = (c2y - c1y) * (c2y - c1y)
            s = x2 + y2
        _ <- evaluate (toDouble s)
        case s of
          Real{} -> assertFailure "sum unexpectedly Real"
          _ -> assertBool "sum ok (Ext/Rational)" True

    , localOption (mkTimeout 100000) $ testCase "sum constructor is printable" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            x2 = (c2x - c1x) * (c2x - c1x)
            y2 = (c2y - c1y) * (c2y - c1y)
            s = x2 + y2
        _ <- evaluate (show s)
        assertBool "showable" True

    , localOption (mkTimeout 1000000) $ testCase "FOCUS sqrt(sum) (distance)" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            x2 = (c2x - c1x) * (c2x - c1x)
            y2 = (c2y - c1y) * (c2y - c1y)
            s = x2 + y2
            d = sqrt s
        _ <- evaluate (toDouble d)
        assertBool "distance ok" True
    ]
  ]
