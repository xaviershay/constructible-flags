{-# LANGUAGE ScopedTypeVariables #-}

module GeometrySpec (geometryTests) where

import ArbitraryFieldNumber ()
import Control.Exception (SomeException, evaluate, try)
import Data.Ratio ((%))
import Flag.Construction.FieldNumber (FieldNumber, fnInteger, fnRational, isZero, toDouble)
import Flag.Construction.Geometry (evalIntersectCC', evalIntersectLC', evalIntersectLL')
import Flag.Construction.Types (Point)
import Test.QuickCheck (Property, ioProperty, shrink)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (forAllShrink, testProperty)

-- | Approximate equality for points, converting FieldNumber to Double.
approxEqual :: String -> Point -> Point -> Assertion
approxEqual msg (x1, y1) (x2, y2) = do
  assertBool
    (msg ++ " x: expected " ++ show x1 ++ " got " ++ show x2)
    (abs (toDouble x1 - toDouble x2) < 1e-9)
  assertBool
    (msg ++ " y: expected " ++ show y1 ++ " got " ++ show y2)
    (abs (toDouble y1 - toDouble y2) < 1e-9)

-- ---------------------------------------------------------------------------
-- QuickCheck shrinking helpers for FieldNumber / line pairs
-- ---------------------------------------------------------------------------

type NumLine = (Point, Point)

shrinkPoint :: Point -> [Point]
shrinkPoint (x, y) =
  [(x', y) | x' <- shrink x]
    ++ [(x, y') | y' <- shrink y]

shrinkLine :: NumLine -> [NumLine]
shrinkLine (p1, p2) =
  [(p1', p2) | p1' <- shrinkPoint p1]
    ++ [(p1, p2') | p2' <- shrinkPoint p2]

shrinkLinePair :: (NumLine, NumLine) -> [(NumLine, NumLine)]
shrinkLinePair (l1, l2) =
  [(l1', l2) | l1' <- shrinkLine l1]
    ++ [(l1, l2') | l2' <- shrinkLine l2]

-- | Check evalIntersectLL' against double-precision arithmetic.
-- Returns True (pass/skip) when lines are near-parallel or the
-- FieldNumber result matches the Double baseline; False (fail) on a
-- wrong result or any thrown exception.
checkIntersectLL :: (NumLine, NumLine) -> IO Bool
checkIntersectLL (l1', l2') = do
  let ((x1, y1), (x2, y2)) = l1'
      ((x3, y3), (x4, y4)) = l2'
      dx1 = toDouble x1
      dy1 = toDouble y1
      dx2 = toDouble x2
      dy2 = toDouble y2
      dx3 = toDouble x3
      dy3 = toDouble y3
      dx4 = toDouble x4
      dy4 = toDouble y4
      dd = (dx1 - dx2) * (dy3 - dy4) - (dy1 - dy2) * (dx3 - dx4)
  if abs dd < 1e-12
    then return True -- near-parallel: skip
    else do
      let dt = ((dx1 - dx3) * (dy3 - dy4) - (dy1 - dy3) * (dx3 - dx4)) / dd
          epx = dx1 + dt * (dx2 - dx1)
          epy = dy1 + dt * (dy2 - dy1)
      result <-
        try
          ( do
              (px, py) <- evaluate (evalIntersectLL' (l1', l2'))
              _ <- evaluate (toDouble px)
              _ <- evaluate (toDouble py)
              return (px, py)
          ) ::
          IO (Either SomeException (FieldNumber, FieldNumber))
      case result of
        Left _ -> return False
        Right (px, py) ->
          return $
            abs (toDouble px - epx) < 1e-6
              && abs (toDouble py - epy) < 1e-6

geometryTests :: TestTree
geometryTests =
  testGroup
    "Geometry"
    [ testGroup
        "intersectLL"
        [ testCase "horizontal and vertical lines intersect at origin" $
            approxEqual "hv" (0, 0) (evalIntersectLL' (((0, 0), (1, 0)), ((0, 0), (0, 1)))),
          testCase "diagonal lines intersect at (1/2,1/2)" $
            approxEqual "diag" (0.5, 0.5) (evalIntersectLL' (((0, 0), (1, 1)), ((0, 1), (1, 0)))),
          testCase "parallel lines error (division by zero)" $ do
            r <- try (evaluate (evalIntersectLL' (((0, 0), (1, 0)), ((0, 1), (1, 1))))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v),
          testCase "degenerate first line errors" $ do
            let p = (fnInteger 0, fnInteger 0)
                q = (fnInteger 1, fnInteger 0)
            r <- try (evaluate (evalIntersectLL' ((p, p), (q, (fnInteger 2, fnInteger 0))))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v),
          testCase "degenerate second line errors" $ do
            let p = (fnInteger 0, fnInteger 0)
                q = (fnInteger 1, fnInteger 0)
            r <- try (evaluate (evalIntersectLL' ((p, (fnInteger 1, fnInteger 1)), (q, q)))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v)
        ],
      testGroup
        "intersectLC degenerate inputs"
        [ testCase "degenerate line errors" $ do
            let p = (fnInteger 0, fnInteger 0)
                cc = (fnInteger 3, fnInteger 0)
                ce = (fnInteger 4, fnInteger 0)
            r <- try (evaluate (fst (evalIntersectLC' ((p, p), (cc, ce))))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v),
          testCase "degenerate circle (zero radius) errors" $ do
            let lp1 = (fnInteger 0, fnInteger 0)
                lp2 = (fnInteger 1, fnInteger 0)
                cc = (fnInteger 2, fnInteger 0)
            r <- try (evaluate (fst (evalIntersectLC' ((lp1, lp2), (cc, cc))))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v)
        ],
      testGroup
        "intersectCC degenerate inputs"
        [ testCase "degenerate first circle (zero radius) errors" $ do
            let c1 = (fnInteger 0, fnInteger 0)
                c2 = (fnInteger 2, fnInteger 0)
                e2 = (fnInteger 3, fnInteger 0)
            r <- try (evaluate (fst (evalIntersectCC' ((c1, c1), (c2, e2))))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v),
          testCase "degenerate second circle (zero radius) errors" $ do
            let c1 = (fnInteger 0, fnInteger 0)
                e1 = (fnInteger 1, fnInteger 0)
                c2 = (fnInteger 2, fnInteger 0)
            r <- try (evaluate (fst (evalIntersectCC' ((c1, e1), (c2, c2))))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v)
        ],
      testGroup
        "IntersectCC diagnostics"
        [ testCase "one circle within the other (no intersection) errors" $ do
            let c1 = (fnInteger 0, fnInteger 0)
                e1 = (fnInteger 2, fnInteger 0) -- radius 2
                c2 = (fnInteger 0, fnInteger 0)
                e2 = (fnInteger 1, fnInteger 0) -- radius 1, same center
            r <- try (evaluate (fst (evalIntersectCC' ((c1, e1), (c2, e2))))) :: IO (Either SomeException (FieldNumber, FieldNumber))
            case r of
              Left _ -> assertBool "caught expected exception" True
              Right v -> assertFailure ("expected exception, got: " ++ show v),
          testCase "xdiff (x2 - x1) evaluates quickly" $ do
            let c1x = fnRational (0 % 1)
                c2x = fnRational (6 % 5)
                xdiff = c2x - c1x
            _ <- evaluate (toDouble xdiff)
            assertBool "xdiff ok" True,
          testCase "ydiff (y2 - y1) evaluates quickly" $ do
            let c1y = fnRational ((-3) % 1)
                c2y = fnRational ((-3) % 5)
                ydiff = c2y - c1y
            _ <- evaluate (toDouble ydiff)
            assertBool "ydiff ok" True,
          testCase "x2 = xdiff^2" $ do
            let c1x = fnRational (0 % 1)
                c2x = fnRational (6 % 5)
                x2 = (c2x - c1x) * (c2x - c1x)
            _ <- evaluate (toDouble x2)
            assertBool "x2 ok" True,
          testCase "y2 = ydiff^2" $ do
            let c1y = fnRational ((-3) % 1)
                c2y = fnRational ((-3) % 5)
                y2 = (c2y - c1y) * (c2y - c1y)
            _ <- evaluate (toDouble y2)
            assertBool "y2 ok" True,
          testCase "sum = x2 + y2" $ do
            let c1x = fnRational (0 % 1)
                c2x = fnRational (6 % 5)
                c1y = fnRational ((-3) % 1)
                c2y = fnRational ((-3) % 5)
                x2 = (c2x - c1x) * (c2x - c1x)
                y2 = (c2y - c1y) * (c2y - c1y)
                s = x2 + y2
            _ <- evaluate (toDouble s)
            assertBool "sum ok" True,
          testCase "sum constructor is printable" $ do
            let c1x = fnRational (0 % 1)
                c2x = fnRational (6 % 5)
                c1y = fnRational ((-3) % 1)
                c2y = fnRational ((-3) % 5)
                x2 = (c2x - c1x) * (c2x - c1x)
                y2 = (c2y - c1y) * (c2y - c1y)
                s = x2 + y2
            _ <- evaluate (show s)
            assertBool "showable" True,
          testCase "FOCUS sqrt(sum) (distance)" $ do
            let c1x = fnRational (0 % 1)
                c2x = fnRational (6 % 5)
                c1y = fnRational ((-3) % 1)
                c2y = fnRational ((-3) % 5)
                x2 = (c2x - c1x) * (c2x - c1x)
                y2 = (c2y - c1y) * (c2y - c1y)
                s = x2 + y2
                d = sqrt s
            _ <- evaluate (toDouble d)
            assertBool "distance ok" True
        ]
    ]
