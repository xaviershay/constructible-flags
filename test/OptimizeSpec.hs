module OptimizeSpec (optimizeTests) where

import Data.Colour.SRGB (sRGB)
import Flag.Construction.Optimize (optimize)
import Flag.Construction.Types (Drawing (..), MaskMode (..))
import Test.Tasty
import Test.Tasty.HUnit

-- Minimal tests: two triangles forming a square should become a single DrawPath
optimizeTests :: TestTree
optimizeTests =
  testGroup
    "optimize"
    [ testCase "two triangles -> single DrawPath" $ do
        let red = sRGB 1 0 0
            t1 = DrawTriangle red (0, 0) (1, 0) (1, 1)
            t2 = DrawTriangle red (0, 0) (1, 1) (0, 1)
            input = t1 <> t2
            expected = DrawPath red [(0, 0), (0, 1), (1, 1), (1, 0), (0, 0)]
        show (optimize input) @?= show expected,
      testCase "DrawCircle is preserved" $ do
        let red = sRGB 1 0 0
            c = DrawCircle red (0, 0) 1
        show (optimize c) @?= show c,
      testCase "circle after triangles preserves order" $ do
        let red = sRGB 1 0 0
            t1 = DrawTriangle red (0, 0) (1, 0) (1, 1)
            t2 = DrawTriangle red (0, 0) (1, 1) (0, 1)
            c = DrawCircle red (0, 0) 1
            input = t1 <> t2 <> c
            expected = DrawPath red [(0, 0), (0, 1), (1, 1), (1, 0), (0, 0)] <> c
        show (optimize input) @?= show expected,
      testCase "different colours do not merge and preserve order" $ do
        let red = sRGB 1 0 0
            blue = sRGB 0 0 1
            t1 = DrawTriangle red (0, 0) (1, 0) (1, 1)
            t2 = DrawTriangle blue (0, 0) (1, 1) (0, 1)
            input = t1 <> t2
            -- helper to flatten an Overlay drawing into a list preserving order
            letFlatten d = case d of
              EmptyDrawing -> []
              Overlay a b -> letFlatten a ++ letFlatten b
              other -> [other]
            out = optimize input
            flat = letFlatten out
        case flat of
          [DrawPath c1 _, DrawPath c2 _] -> do
            c1 @?= red
            c2 @?= blue
          _ -> assertFailure ("unexpected optimize output: " ++ show out),
      testCase "DrawMasked content triangles are optimized" $ do
        let red = sRGB 1 0 0
            blue = sRGB 0 0 1
            t1 = DrawTriangle red (0, 0) (1, 0) (1, 1)
            t2 = DrawTriangle red (0, 0) (1, 1) (0, 1)
            mask = DrawCircle blue (0, 0) 1
            input = DrawMasked Clip (t1 <> t2) mask
            expected = DrawMasked Clip (DrawPath red [(0, 0), (0, 1), (1, 1), (1, 0), (0, 0)]) mask
        show (optimize input) @?= show expected,
      testCase "DrawMasked mask triangles are optimized" $ do
        let red = sRGB 1 0 0
            blue = sRGB 0 0 1
            content = DrawCircle red (0, 0) 1
            t1 = DrawTriangle blue (0, 0) (1, 0) (1, 1)
            t2 = DrawTriangle blue (0, 0) (1, 1) (0, 1)
            input = DrawMasked Mask content (t1 <> t2)
            expected = DrawMasked Mask content (DrawPath blue [(0, 0), (0, 1), (1, 1), (1, 0), (0, 0)])
        show (optimize input) @?= show expected,
      testCase "DrawMasked nested inside Overlay is optimized" $ do
        let red = sRGB 1 0 0
            blue = sRGB 0 0 1
            t1 = DrawTriangle red (0, 0) (1, 0) (1, 1)
            t2 = DrawTriangle red (0, 0) (1, 1) (0, 1)
            masked = DrawMasked Clip (t1 <> t2) EmptyDrawing
            circ = DrawCircle blue (0, 0) 1
            input = masked <> circ
            expectedMasked = DrawMasked Clip (DrawPath red [(0, 0), (0, 1), (1, 1), (1, 0), (0, 0)]) EmptyDrawing
            expected = expectedMasked <> circ
        show (optimize input) @?= show expected,
      testCase "DrawMasked content and mask both optimized independently" $ do
        let red = sRGB 1 0 0
            blue = sRGB 0 0 1
            ct1 = DrawTriangle red (0, 0) (1, 0) (1, 1)
            ct2 = DrawTriangle red (0, 0) (1, 1) (0, 1)
            mt1 = DrawTriangle blue (0, 0) (1, 0) (1, 1)
            mt2 = DrawTriangle blue (0, 0) (1, 1) (0, 1)
            input = DrawMasked Mask (ct1 <> ct2) (mt1 <> mt2)
            expected =
              DrawMasked
                Mask
                (DrawPath red [(0, 0), (0, 1), (1, 1), (1, 0), (0, 0)])
                (DrawPath blue [(0, 0), (0, 1), (1, 1), (1, 0), (0, 0)])
        show (optimize input) @?= show expected
    ]
