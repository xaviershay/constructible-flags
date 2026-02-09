module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Flag.Constructions (naturalMult, perpendicular, parallel, midpoint)
import Flag.Construction.Interpreter (eval)
import Flag.Construction.Types (Point)
import Flag.Construction.Geometry (dist)

main :: IO ()
main = defaultMain tests

-- | Approximate equality for points, to handle floating-point imprecision.
approxEqual :: String -> Point -> Point -> Assertion
approxEqual msg (x1, y1) (x2, y2) = do
  assertBool (msg ++ " x: expected " ++ show x1 ++ " got " ++ show x2) (abs (x1 - x2) < 1e-9)
  assertBool (msg ++ " y: expected " ++ show y1 ++ " got " ++ show y2) (abs (y1 - y2) < 1e-9)

-- | Assert two doubles are approximately equal.
approxEqualD :: String -> Double -> Double -> Assertion
approxEqualD msg expected actual =
  assertBool (msg ++ ": expected " ++ show expected ++ " got " ++ show actual) (abs (expected - actual) < 1e-9)

evalNM :: Int -> (Point, Point) -> Point
evalNM n = eval (naturalMult n)

evalPerp :: (Point, Point) -> (Point, Point)
evalPerp = eval perpendicular

evalPar :: ((Point, Point), Point) -> (Point, Point)
evalPar = eval parallel

evalMid :: (Point, Point) -> Point
evalMid = eval midpoint

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
  ]