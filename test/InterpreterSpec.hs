{-# LANGUAGE Arrows #-}

module InterpreterSpec (interpreterTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)

import Flag.Construction.Types (FlagA(..), Drawing)
import Flag.Constructions (fillTriangle, intersectLL, intersectCC)
import Flag.Construction.Interpreter (evalCollectNumbers, Step(..), steps)
import Flag.Construction.FieldNumber (fieldOf, Field(..))

white :: Colour Double
white = sRGB24 255 255 255

interpreterTests :: TestTree
interpreterTests = testGroup "evalCollectNumbers"
  [ testCase "collects numbers from all intersections, even if result is unused" $ do
      -- Two unit circles centered at (0,0) and (1,0) intersect at (1/2, ±√3/2).
      -- We discard the intersection result and fill a triangle with rational points.
      -- We assume the user always uses at least one point from each intersection,
      -- and both points share the same field, so we collect from all intersections.
      --
      -- i.e. this a degenerate case that we explicitly don't handle with the
      -- current data model and algorithm.
      let construction :: FlagA () Drawing
          construction = proc () -> do
            _ps <- intersectCC -< (((0,0),(1,0)),((1,0),(0,0)))
            fillTriangle white -< ((0,0),(1,0),(2,0))

          (_, nums) = evalCollectNumbers construction ()
          irrationalNums = filter (\n -> fieldOf n == FIrrational) nums

      assertBool
        ("Expected FIrrational numbers from intersection, but got fields: " ++ show (map fieldOf nums))
        (not (null irrationalNums))

  , testCase "collects numbers from an intersection whose result is used in a fill" $ do
      -- Same intersection producing (1/2, ±√3/2), but this time one of the
      -- intersection points is used as a triangle vertex.
      -- The FIrrational SHOULD appear in collected numbers.
      let construction :: FlagA () Drawing
          construction = proc () -> do
            (p, _) <- intersectCC -< (((0,0),(1,0)),((1,0),(0,0)))
            fillTriangle white -< ((0,0), (1,0), p)

          (_, nums) = evalCollectNumbers construction ()
          irrationalNums = filter (\n -> fieldOf n == FIrrational) nums

      assertBool
        ("Expected FIrrational numbers from used intersection, but got fields: " ++ show (map fieldOf nums))
        (not (null irrationalNums))

  , testCase "collects numbers from an intersection used transitively via another intersection" $ do
      -- IntersectCC produces (1/2, ±√3/2). One of those points is then used
      -- as input to an IntersectLL, whose result feeds a fill.
      -- The FIrrational should be collected because it's on the data path to the fill.
      let construction :: FlagA () Drawing
          construction = proc () -> do
            (p, _) <- intersectCC -< (((0,0),(1,0)),((1,0),(0,0)))
            q <- intersectLL -< (((0,0), p), ((0,0), (1,0)))
            fillTriangle white -< ((0,0), (1,0), q)

          (_, nums) = evalCollectNumbers construction ()
          irrationalNums = filter (\n -> fieldOf n == FIrrational) nums

      assertBool
        ("Expected FIrrational numbers from transitively used intersection, but got fields: " ++ show (map fieldOf nums))
        (not (null irrationalNums))

  , testCase "steps records SVG overlay" $ do
      steps (OverlaySVG "foo") @?= [StepSVGOverlay]
  ]
