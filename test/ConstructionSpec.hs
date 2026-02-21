{-# LANGUAGE ScopedTypeVariables #-}

module ConstructionSpec (constructionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Flag.Constructions (naturalMult, rationalMult, perpendicular, parallel, midpoint, translate, bisectAngle)
import Flag.Construction.Interpreter (eval)
import Flag.Construction.Types (Point)
import Flag.Construction.Geometry (dist)
import Flag.Construction.Radical (Radical(..), toDouble)
import Data.Ratio (Ratio, (%))
import ArbitraryRadical ()

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

evalRM :: Ratio Int -> (Point, Point) -> Point
evalRM r = eval (rationalMult r)

constructionTests :: TestTree
constructionTests = testGroup "Constructions"
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
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
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
    , testCase "bisects angle not from origin" $ do
        let o = (0, -3)
            a = (0, 0)
            b = (3, (Rational ((-3) % 2)))
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
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (_, q) = evalPar ((a, b), p)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
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

    , testCase "midpoint of Rational and Ext point" $ do
        -- This previously hung: midpoint construction introduces √3
        -- from intersectCC alongside the input's √5, creating a
        -- multi-radicand denominator that caused divR non-termination.
        let a = (Rational 0, Rational 0) :: Point
            inner = Ext (Rational 1) (Rational 2) (Rational 5) 2
            b = (inner, Rational 3) :: Point
            m = evalMid (a, b)
            expectedX = (1 + 2 * sqrt 5) / 2
        assertBool "midpoint x"
          (abs (toDouble (fst m) - expectedX) < 1e-9)
        assertBool "midpoint y"
          (abs (toDouble (snd m) - 1.5) < 1e-9)
    ]
  , testGroup "rationalMult"
    [ testCase "p=q returns second point" $
        evalRM (3 % 3) ((0, 0), (1, 0)) @?= (1, 0)

    , testCase "1/2 on horizontal unit segment" $
        approxEqual "1/2" (0.5, 0) (evalRM (1 % 2) ((0, 0), (1, 0)))

    , testCase "1/3 on horizontal unit segment" $
        approxEqual "1/3" (1/3, 0) (evalRM (1 % 3) ((0, 0), (1, 0)))

    , testCase "1/2 on vertical segment" $
        approxEqual "1/2 vertical" (0, 0.5) (evalRM (1 % 2) ((0, 0), (0, 1)))

    , testCase "1/2 with non-origin start" $
        approxEqual "1/2 offset" (2, 0) (evalRM (1 % 2) ((1, 0), (3, 0)))
    ]
  , testGroup "translate"
    [ testProperty "result passes through the given point" $
        \(a :: Point) (b :: Point) (p :: Point) ->
          let mid = ((fst b + fst p) / 2, (snd b + snd p) / 2)
          in a /= b && mid /= a ==>
               let (r, _) = evalTrans ((a, b), p)
               in (abs (toDouble (fst r - fst p)) < 1e-9)
                  && (abs (toDouble (snd r - snd p)) < 1e-9)

    , testCase "result is translated vector (unit)" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (_, q) = evalTrans ((a, b), p)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
            cross = dx1 * dy2 - dy1 * dx2
        approxEqualD "cross product should be 0" 0 cross

    , testCase "result preserves segment length (unit)" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (_, q) = evalTrans ((a, b), p)
        approxEqualD "length" (dist a b) (dist p q)

    , testCase "result passes through the given point (unit)" $ do
        let ((a, b), p) = (((0, 0), (1, 0)), (0, 1))
            (r, _) = evalTrans ((a, b), p)
        approxEqual "passes through p" p r

    , testCase "works on diagonal line" $ do
        let ((a, b), p) = (((0, 0), (1, 1)), (2, 0))
            (_, q) = evalTrans ((a, b), p)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
            cross = dx1 * dy2 - dy1 * dx2
        approxEqualD "cross product should be 0" 0 cross

    , testCase "works with non-origin points" $ do
        let ((a, b), p) = (((1, 2), (3, 4)), (5, 0))
            (_, q) = evalTrans ((a, b), p)
            (dx1, dy1) = (fst b - fst a, snd b - snd a)
            (dx2, dy2) = (fst q - fst p, snd q - snd p)
            cross = dx1 * dy2 - dy1 * dx2
        approxEqualD "cross product should be 0" 0 cross

    , testCase "zero-length segment returns (p,p)" $
        let a = (0,0) :: Point
            b = a
            p = (1,2) :: Point
        in evalTrans ((a,b), p) @?= (p, p)
    ]
  ]
