{-# LANGUAGE ScopedTypeVariables #-}
module Main (main, tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Flag.Constructions (naturalMult, rationalMult, perpendicular, parallel, midpoint, translate, bisectAngle)
import Flag.Construction.Interpreter (eval)
import Flag.Construction.Types (Point)
import Flag.Construction.Geometry (dist, evalIntersectCC')
import Flag.Construction.Radical (Radical(..), toDouble)
import Data.Ratio ((%))
import Control.Exception (evaluate)
import qualified RadicalSpec
import ArbitraryRadical ()
import qualified NGonVertexSpec
import ImageGoldenSpec (imageGoldenTests)
import ConstructionCostSpec (constructionCostTests)
import InterpreterSpec (interpreterTests)

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ tests
        , imageGoldenTests
    , constructionCostTests
    , RadicalSpec.radicalTests
    , NGonVertexSpec.ngonVertexTests
    , interpreterTests
  ]

-- | Approximate equality for points, converting Radical to Double.
approxEqual :: String -> Point -> Point -> Assertion
approxEqual msg (x1, y1) (x2, y2) = do
  assertBool (msg ++ " x: expected " ++ show x1 ++ " got " ++ show x2)
    (abs (toDouble x1 - toDouble x2) < 1e-9)
  assertBool (msg ++ " y: expected " ++ show y1 ++ " got " ++ show y2)
    (abs (toDouble y1 - toDouble y2) < 1e-9)

-- | Assert two Radicals are approximately equal (via Double conversion).
approxEqualD :: String -> Radical -> Radical -> Assertion
approxEqualD msg expected actual =
  assertBool (msg ++ ": expected " ++ show expected ++ " got " ++ show actual)
    (abs (toDouble expected - toDouble actual) < 1e-9)

evalNM :: Int -> (Point, Point) -> Point
evalNM n = eval (naturalMult n)

evalPerp :: (Point, Point) -> (Point, Point)
evalPerp = eval perpendicular

evalPar :: ((Point, Point), Point) -> (Point, Point)
evalPar = eval parallel

evalBisect :: (Point, (Point, Point)) -> (Point, Point)
evalBisect = eval bisectAngle

evalTrans :: ((Point, Point), Point) -> (Point, Point)
evalTrans = eval translate

evalMid :: (Point, Point) -> Point
evalMid = eval midpoint

evalRM :: Int -> Int -> (Point, Point) -> Point
evalRM p q = eval (rationalMult p q)

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "naturalMult"
    [ testGroup "eval"
      [ testCase "n=0 returns first point" $
          evalNM 0 ((0, 0), (1, 0)) @?= (0, 0)

      , testCase "n=1 returns second point" $
          evalNM 1 ((0, 0), (1, 0)) @?= (1, 0)

      , testCase "n=5 on horizontal unit segment" $
          approxEqual "5×" (5, 0) (evalNM 5 ((0, 0), (1, 0)))

      , testCase "n=2 on vertical segment" $
          approxEqual "2× vertical" (0, 2) (evalNM 2 ((0, 0), (0, 1)))

      , testCase "n=3 on diagonal segment" $
          approxEqual "3× diagonal" (3, 3) (evalNM 3 ((0, 0), (1, 1)))

      , testCase "n=2 with non-origin start" $
          approxEqual "2× offset" (5, 0) (evalNM 2 ((1, 0), (3, 0)))
      ]
    , testProperty "dist(a, result) = n * dist(a, b)" $
        \n' (ax :: Int) (ay :: Int) (bx :: Int) (by :: Int) ->
          (ax, ay) /= (bx, by) ==>
          let n = abs n' `mod` 51
              a = (fromIntegral ax, fromIntegral ay) :: Point
              b = (fromIntegral bx, fromIntegral by) :: Point
              result = evalNM n (a, b)
          in abs (toDouble (dist a result) - fromIntegral n * toDouble (dist a b)) < 1e-6
    ]
  , testGroup "perpendicular"
    [ testCase "result is perpendicular to input" $ do
        let (a, b) = ((0, 0), (1, 0))
            (p, _) = evalPerp (a, b)
            -- Vector a->b
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            -- Vector a->p
            (dx2, dy2) = (fst p - fst a, snd p - snd a)
            dot = dx1 * dx2 + dy1 * dy2
        approxEqualD "dot product should be 0" 0 dot

    , testCase "result preserves distance from a" $ do
        let (a, b) = ((0, 0), (1, 0))
            (p, _) = evalPerp (a, b)
        approxEqualD "distance" (dist a b) (dist a p)

    , testCase "both results are perpendicular" $ do
        let (a, b) = ((0, 0), (1, 0))
            (p, p') = evalPerp (a, b)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            dotP  = dx1 * (fst p  - fst a) + dy1 * (snd p  - snd a)
            dotP' = dx1 * (fst p' - fst a) + dy1 * (snd p' - snd a)
        approxEqualD "dot product p" 0 dotP
        approxEqualD "dot product p'" 0 dotP'

    , testCase "works on diagonal segment" $ do
        let (a, b) = ((0, 0), (1, 1))
            (p, _) = evalPerp (a, b)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            dot = dx1 * (fst p - fst a) + dy1 * (snd p - snd a)
        approxEqualD "dot product" 0 dot

    , testCase "works with non-origin start" $ do
        let (a, b) = ((2, 3), (4, 3))
            (p, _) = evalPerp (a, b)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            dot = dx1 * (fst p - fst a) + dy1 * (snd p - snd a)
        approxEqualD "dot product" 0 dot
        approxEqualD "distance" (dist a b) (dist a p)
    ]

  , testGroup "bisectAngle"
    [ testCase "returns line through vertex" $ do
        let o = (0, 0)
            a = (1, 0)
            b = (0, 1)
            (p, _) = evalBisect (o, (a, b))
        approxEqual "vertex" o p

    , testCase "bisects right angle" $ do
        let o = (0, 0)
            a = (1, 0)
            b = (0, 1)
            (_, q) = evalBisect (o, (a, b))
            (dxA, dyA) = (fst a - fst o, snd a - snd o)
            (dxB, dyB) = (fst b - fst o, snd b - snd o)
            (dxQ, dyQ) = (fst q - fst o, snd q - snd o)
            cos1 = (dxA * dxQ + dyA * dyQ) / (dist o a * dist o q)
            cos2 = (dxB * dxQ + dyB * dyQ) / (dist o b * dist o q)
        approxEqualD "angles equal" cos1 cos2
    ]
  , testGroup "parallel"
    [ testCase "result is parallel to input line" $ do
        -- parallel ((a,b), p) should return (p, q) where p->q is parallel to a->b
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (_, q) = evalPar ((a, b), p)
            -- Direction of a->b
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            -- Direction of p->q
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
            -- Cross product should be 0 for parallel
            cross = dx1 * dy2 - dy1 * dx2
        approxEqualD "cross product should be 0" 0 cross

    , testCase "result preserves segment length" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (_, q) = evalPar ((a, b), p)
        approxEqualD "length" (dist a b) (dist p q)

    , testCase "result passes through the given point" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (r, _) = evalPar ((a, b), p)
        approxEqual "passes through p" p r

    , testCase "works on diagonal line" $ do
        let ((a, b), p) = (((0, 0), (1, 1)), (2, 0))
            (_, q) = evalPar ((a, b), p)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
            cross = dx1 * dy2 - dy1 * dx2
        approxEqualD "cross product should be 0" 0 cross

    , testCase "works with non-origin points" $ do
        let ((a, b), p) = (((1, 2), (3, 4)), (5, 0))
            (_, q) = evalPar ((a, b), p)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
            cross = dx1 * dy2 - dy1 * dx2
        approxEqualD "cross product should be 0" 0 cross
    ]
  , testGroup "midpoint"
    [ testCase "midpoint of horizontal segment" $
        approxEqual "mid" (0.5, 0) (evalMid ((0, 0), (1, 0)))

    , testCase "midpoint of vertical segment" $
        approxEqual "mid" (0, 0.5) (evalMid ((0, 0), (0, 1)))

    , testCase "midpoint of diagonal segment" $
        approxEqual "mid" (0.5, 0.5) (evalMid ((0, 0), (1, 1)))

    , testCase "midpoint with non-origin start" $
        approxEqual "mid" (2, 3) (evalMid ((1, 2), (3, 4)))

    , testCase "equidistant from endpoints" $ do
        let (a, b) = ((1, 2), (5, 8))
            m = evalMid (a, b)
        approxEqualD "equidistant" (dist a m) (dist m b)
    ]
  , testGroup "rationalMult"
    [ testCase "p=q returns second point" $
        evalRM 3 3 ((0, 0), (1, 0)) @?= (1, 0)

    , testCase "1/2 on horizontal unit segment" $
        approxEqual "1/2" (0.5, 0) (evalRM 1 2 ((0, 0), (1, 0)))

    , testCase "1/3 on horizontal unit segment" $
        approxEqual "1/3" (1/3, 0) (evalRM 1 3 ((0, 0), (1, 0)))

    , testCase "2/3 on horizontal unit segment" $
        approxEqual "2/3" (2/3, 0) (evalRM 2 3 ((0, 0), (1, 0)))

    , testCase "3/2 extends past endpoint" $
        approxEqual "3/2" (1.5, 0) (evalRM 3 2 ((0, 0), (1, 0)))

    , testCase "1/2 on vertical segment" $
        approxEqual "1/2 vertical" (0, 0.5) (evalRM 1 2 ((0, 0), (0, 1)))

    , testCase "2/5 on diagonal segment" $
        approxEqual "2/5 diagonal" (0.4, 0.4) (evalRM 2 5 ((0, 0), (1, 1)))

    , testCase "1/2 with non-origin start" $
        approxEqual "1/2 offset" (2, 0) (evalRM 1 2 ((1, 0), (3, 0)))
    ]
  , testGroup "translate"
    [ testCase "result is translated vector" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (_, q) = evalTrans ((a, b), p)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
            cross = dx1 * dy2 - dy1 * dx2
        approxEqualD "cross product should be 0" 0 cross

    , testCase "result preserves segment length" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (_, q) = evalTrans ((a, b), p)
        approxEqualD "length" (dist a b) (dist p q)

    , testCase "result passes through the given point" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (r, _) = evalTrans ((a, b), p)
        approxEqual "passes through p" p r
    ]

  , localOption (mkTimeout 100000) $ testCase "IntersectCC regression: radical-heavy input should not hang (timeout 0.1s)" $ do
      -- Input taken from bug report: exercise IntersectCC on radicals with
      -- nested Ext values.  We force numeric evaluation via `toDouble` so
      -- any lazy normalization / arithmetic is performed inside the test.
      let c1 = (Rational (0 % 1), Rational ((-3) % 1))
          e1 = (Rational (0 % 1), Rational (0 % 1))
          c2 = (Ext (Rational (0 % 1)) (Rational (6 % 5)) (Rational (5 % 1)) 2,
                Ext (Rational (0 % 1)) (Rational ((-3) % 5)) (Rational (5 % 1)) 2)
          e2 = (Rational (0 % 1), Rational (0 % 1))

      let input = ((c1, e1), (c2, e2))
      let (p1, _p2) = evalIntersectCC' input
      _ <- evaluate (toDouble (fst p1))
      assertBool "IntersectCC completed" True

  , localOption (mkTimeout 100000) $ testCase "IntersectCC step: r1 and r2 compute quickly" $ do
      let c1 = (Rational (0 % 1), Rational ((-3) % 1))
          e1 = (Rational (0 % 1), Rational (0 % 1))
          c2 = (Ext (Rational (0 % 1)) (Rational (6 % 5)) (Rational (5 % 1)) 2,
                Ext (Rational (0 % 1)) (Rational ((-3) % 5)) (Rational (5 % 1)) 2)
          e2 = (Rational (0 % 1), Rational (0 % 1))
          r1 = dist c1 e1
          r2 = dist c2 e2
      -- both radii should be exactly 3
      approxEqualD "r1" 3 r1
      approxEqualD "r2" 3 r2
      assertBool "r1 == r2" (r1 == r2)

  , localOption (mkTimeout 100000) $ testCase "IntersectCC step: compute d (center distance)" $ do
      let c1 = (Rational (0 % 1), Rational ((-3) % 1))
          c2 = (Ext (Rational (0 % 1)) (Rational (6 % 5)) (Rational (5 % 1)) 2,
                Ext (Rational (0 % 1)) (Rational ((-3) % 5)) (Rational (5 % 1)) 2)
          d = dist c1 c2
      -- force numeric evaluation only
      _ <- evaluate (toDouble d)
      assertBool "computed d" True

  , localOption (mkTimeout 100000) $ testCase "IntersectCC step: compute a = (r1^2 - r2^2 + d^2) / (2*d)" $ do
      let c1 = (Rational (0 % 1), Rational ((-3) % 1))
          e1 = (Rational (0 % 1), Rational (0 % 1))
          c2 = (Ext (Rational (0 % 1)) (Rational (6 % 5)) (Rational (5 % 1)) 2,
                Ext (Rational (0 % 1)) (Rational ((-3) % 5)) (Rational (5 % 1)) 2)
          e2 = (Rational (0 % 1), Rational (0 % 1))
          r1 = dist c1 e1
          r2 = dist c2 e2
          d  = dist c1 c2
          a  = (r1*r1 - r2*r2 + d*d) / (Rational (2 % 1) * d)
      -- since r1 == r2, a should simplify to d/2; force numeric evaluation
      _ <- evaluate (toDouble a)
      assertBool "computed a" True

  , localOption (mkTimeout 100000) $ testCase "IntersectCC step: compute inside = r1^2 - a^2 and sqrt" $ do
      let c1 = (Rational (0 % 1), Rational ((-3) % 1))
          e1 = (Rational (0 % 1), Rational (0 % 1))
          c2 = (Ext (Rational (0 % 1)) (Rational (6 % 5)) (Rational (5 % 1)) 2,
                Ext (Rational (0 % 1)) (Rational ((-3) % 5)) (Rational (5 % 1)) 2)
          e2 = (Rational (0 % 1), Rational (0 % 1))
          r1 = dist c1 e1
          r2 = dist c2 e2
          d  = dist c1 c2
          a  = (r1*r1 - r2*r2 + d*d) / (Rational (2 % 1) * d)
          inside = r1*r1 - a*a
          h = sqrt inside
      _ <- evaluate (toDouble inside)
      _ <- evaluate (toDouble h)
      assertBool "computed sqrt" True

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
          MinPolyExt{} -> assertFailure "sum unexpectedly MinPolyExt"
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