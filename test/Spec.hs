module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Flag.Constructions (naturalMult)
import Flag.Construction.Interpreter (eval, steps, Step(..))
import Flag.Construction.Types (Point)

main :: IO ()
main = defaultMain tests

-- | Approximate equality for points, to handle floating-point imprecision.
approxEqual :: String -> Point -> Point -> Assertion
approxEqual msg (x1, y1) (x2, y2) = do
  assertBool (msg ++ " x: " ++ show x1 ++ " /= " ++ show x2) (abs (x1 - x2) < 1e-9)
  assertBool (msg ++ " y: " ++ show y1 ++ " /= " ++ show y2) (abs (y1 - y2) < 1e-9)

evalNM :: Int -> (Point, Point) -> Point
evalNM n = eval (naturalMult n)

tests :: TestTree
tests = testGroup "naturalMult"
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