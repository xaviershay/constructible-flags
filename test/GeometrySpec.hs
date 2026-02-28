{-# LANGUAGE ScopedTypeVariables #-}

module GeometrySpec (geometryTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, forAllShrink)
import Test.QuickCheck (Property, ioProperty, shrink)

import Flag.Construction.Types (Point)
import Flag.Construction.Geometry (evalIntersectLL')
import Flag.Construction.Radical (Radical(..), toDouble, isZero)
import Data.Ratio ((%))
import ArbitraryRadical ()
import Control.Exception (evaluate, try, SomeException)

-- | Approximate equality for points, converting Radical to Double.
approxEqual :: String -> Point -> Point -> Assertion
approxEqual msg (x1, y1) (x2, y2) = do
  assertBool (msg ++ " x: expected " ++ show x1 ++ " got " ++ show x2)
    (abs (toDouble x1 - toDouble x2) < 1e-9)
  assertBool (msg ++ " y: expected " ++ show y1 ++ " got " ++ show y2)
    (abs (toDouble y1 - toDouble y2) < 1e-9)

-- ---------------------------------------------------------------------------
-- QuickCheck shrinking helpers for Radical / line pairs
-- ---------------------------------------------------------------------------

type RadLine = (Point, Point)

shrinkPoint :: Point -> [Point]
shrinkPoint (x, y) =
     [(x', y) | x' <- shrink x]
  ++ [(x, y') | y' <- shrink y]

shrinkLine :: RadLine -> [RadLine]
shrinkLine (p1, p2) =
     [(p1', p2) | p1' <- shrinkPoint p1]
  ++ [(p1, p2') | p2' <- shrinkPoint p2]

shrinkLinePair :: (RadLine, RadLine) -> [(RadLine, RadLine)]
shrinkLinePair (l1, l2) =
     [(l1', l2) | l1' <- shrinkLine l1]
  ++ [(l1, l2') | l2' <- shrinkLine l2]

-- | Check evalIntersectLL' against double-precision arithmetic.
-- Returns True (pass/skip) when lines are near-parallel or the
-- Radical result matches the Double baseline; False (fail) on a
-- wrong result or any thrown exception.
checkIntersectLL :: (RadLine, RadLine) -> IO Bool
checkIntersectLL (l1', l2') = do
  let ((x1,y1),(x2,y2)) = l1'
      ((x3,y3),(x4,y4)) = l2'
      dx1 = toDouble x1; dy1 = toDouble y1
      dx2 = toDouble x2; dy2 = toDouble y2
      dx3 = toDouble x3; dy3 = toDouble y3
      dx4 = toDouble x4; dy4 = toDouble y4
      dd  = (dx1-dx2)*(dy3-dy4) - (dy1-dy2)*(dx3-dx4)
  if abs dd < 1e-12
    then return True   -- near-parallel: skip
    else do
      let dt  = ((dx1-dx3)*(dy3-dy4) - (dy1-dy3)*(dx3-dx4)) / dd
          epx = dx1 + dt*(dx2-dx1)
          epy = dy1 + dt*(dy2-dy1)
      result <- try (do
          (px, py) <- evaluate (evalIntersectLL' (l1', l2'))
          _  <- evaluate (toDouble px)
          _  <- evaluate (toDouble py)
          return (px, py))
        :: IO (Either SomeException (Radical, Radical))
      case result of
        Left  _        -> return False
        Right (px, py) ->
          return $  abs (toDouble px - epx) < 1e-6
                 && abs (toDouble py - epy) < 1e-6

-- | Start from the known failing (l1, l2) and let QuickCheck shrink it
-- to a minimal reproduction.  Run the test suite and inspect the output
-- to read off the shrunk counterexample.
prop_intersectLL_regression :: Property
prop_intersectLL_regression =
  forAllShrink (pure (l1Failing, l2Failing)) shrinkLinePair $
    \pair -> ioProperty (checkIntersectLL pair)
  where
    l1Failing =
      ( (Rational (1 % 1), Rational (0 % 1))
      , ( Ext (Rational ((-1) % 4)) (Rational ((-1) % 4)) (Rational (5 % 1)) 2
        , Ext (Rational (0 % 1)) (Rational (1 % 2))
              (Ext (Rational (5 % 2)) (Rational ((-1) % 2)) (Rational (5 % 1)) 2) 2
        )
      )
    l2Failing =
      ( ( Ext (Rational ((-1) % 4)) (Rational (1 % 4)) (Rational (5 % 1)) 2
        , Ext (Rational (0 % 1)) (Rational (1 % 2))
              (Ext (Rational (5 % 2)) (Rational (1 % 2)) (Rational (5 % 1)) 2) 2
        )
      , ( Ext (Rational ((-1) % 4)) (Rational ((-1) % 4)) (Rational (5 % 1)) 2
        , Ext (Rational (0 % 1)) (Rational ((-1) % 2))
              (Ext (Rational (5 % 2)) (Rational ((-1) % 2)) (Rational (5 % 1)) 2) 2
        )
      )

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
    --, testCase "regression" $ do
    --    let l1 = ((Rational (1 % 1),Rational (0 % 1)),(Ext (Rational ((-1) % 4)) (Rational ((-1) % 4)) (Rational (5 % 1)) 2,Ext (Rational (0 % 1)) (Rational (1 % 2)) (Ext (Rational (5 % 2)) (Rational ((-1) % 2)) (Rational (5 % 1)) 2) 2))
    --    let l2 = ((Ext (Rational ((-1) % 4)) (Rational (1 % 4)) (Rational (5 % 1)) 2,Ext (Rational (0 % 1)) (Rational (1 % 2)) (Ext (Rational (5 % 2)) (Rational (1 % 2)) (Rational (5 % 1)) 2) 2),(Ext (Rational ((-1) % 4)) (Rational ((-1) % 4)) (Rational (5 % 1)) 2,Ext (Rational (0 % 1)) (Rational ((-1) % 2)) (Ext (Rational (5 % 2)) (Rational ((-1) % 2)) (Rational (5 % 1)) 2) 2))

    --    approxEqual "diag" (0.5, 0.5) (evalIntersectLL' (l1, l2))

    --, testProperty "regression minimised (QuickCheck shrink)" prop_intersectLL_regression
    ]
  , testGroup "IntersectCC diagnostics"
    [ testCase "xdiff (x2 - x1) evaluates quickly" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            xdiff = c2x - c1x
        _ <- evaluate (toDouble xdiff)
        assertBool "xdiff ok" True

    , testCase "ydiff (y2 - y1) evaluates quickly" $ do
        let c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            ydiff = c2y - c1y
        _ <- evaluate (toDouble ydiff)
        assertBool "ydiff ok" True

    , testCase "x2 = xdiff^2" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            x2 = (c2x - c1x) * (c2x - c1x)
        _ <- evaluate (toDouble x2)
        assertBool "x2 ok" True

    , testCase "y2 = ydiff^2" $ do
        let c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            y2 = (c2y - c1y) * (c2y - c1y)
        _ <- evaluate (toDouble y2)
        assertBool "y2 ok" True

    , testCase "sum = x2 + y2" $ do
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

    , testCase "sum constructor is printable" $ do
        let c1x = Rational (0 % 1)
            c2x = Ext (Rational 0) (Rational (6 % 5)) (Rational (5 % 1)) 2
            c1y = Rational ((-3) % 1)
            c2y = Ext (Rational 0) (Rational ((-3) % 5)) (Rational (5 % 1)) 2
            x2 = (c2x - c1x) * (c2x - c1x)
            y2 = (c2y - c1y) * (c2y - c1y)
            s = x2 + y2
        _ <- evaluate (show s)
        assertBool "showable" True

    , testCase "FOCUS sqrt(sum) (distance)" $ do
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
