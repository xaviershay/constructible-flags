{-# LANGUAGE Arrows #-}

module InterpreterSpec (interpreterTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)

import Flag.Construction.Types (FlagA(..), Drawing)
import Flag.Constructions (fillTriangle, intersectLL, intersectCC)
import Flag.Construction.Interpreter (evalCollectRadicals, Step(..), steps)
import Flag.Construction.Radical (radicands)

white :: Colour Double
white = sRGB24 255 255 255

interpreterTests :: TestTree
interpreterTests = testGroup "evalCollectRadicals"
  [ testCase "collects radicals from all intersections, even if result is unused" $ do
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

          (_, rads) = evalCollectRadicals construction ()
          allRadicands = concatMap radicands rads

      assertBool
        ("Expected √3 radicand from intersection, but got: " ++ show allRadicands)
        (not (null allRadicands))

  , testCase "collects radicals from an intersection whose result is used in a fill" $ do
      -- Same intersection producing (1/2, ±√3/2), but this time one of the
      -- intersection points is used as a triangle vertex.
      -- The √3 SHOULD appear in collected radicals.
      let construction :: FlagA () Drawing
          construction = proc () -> do
            (p, _) <- intersectCC -< (((0,0),(1,0)),((1,0),(0,0)))
            fillTriangle white -< ((0,0), (1,0), p)

          (_, rads) = evalCollectRadicals construction ()
          allRadicands = concatMap radicands rads

      assertBool
        ("Expected √3 radicand from used intersection, but got: " ++ show allRadicands)
        (not (null allRadicands))

  , testCase "collects radicals from an intersection used transitively via another intersection" $ do
      -- IntersectCC produces (1/2, ±√3/2). One of those points is then used
      -- as input to an IntersectLL, whose result feeds a fill.
      -- The √3 should be collected because it's on the data path to the fill.
      let construction :: FlagA () Drawing
          construction = proc () -> do
            (p, _) <- intersectCC -< (((0,0),(1,0)),((1,0),(0,0)))
            q <- intersectLL -< (((0,0), p), ((0,0), (1,0)))
            fillTriangle white -< ((0,0), (1,0), q)

          (_, rads) = evalCollectRadicals construction ()
          allRadicands = concatMap radicands rads

      assertBool
        ("Expected radicals from transitively used intersection, but got: " ++ show allRadicands)
        (not (null allRadicands))
  , testCase "steps records SVG overlay" $ do
      steps (OverlaySVG "foo") @?= [StepSVGOverlay]
  ]
