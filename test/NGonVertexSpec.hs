{-# LANGUAGE ScopedTypeVariables #-}

module NGonVertexSpec (ngonVertexTests) where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryFieldNumber ()
import Flag.Construction.Types (Point)
import Flag.Construction.Interpreter (eval)
import Flag.Construction.Geometry (evalNGonVertex)
import Flag.Construction.FieldNumber (toDouble)

approxEq :: Double -> Double -> Double -> Bool
approxEq eps a b = abs (a - b) <= eps

ngonVertexTests :: TestTree
ngonVertexTests = testGroup "NGonVertex"
    [ testProperty "heptagon: preserves radius for any k" $ \(k :: Int, center :: Point, first :: Point) ->
        let n = 7
            k' = abs k `mod` n
            (x, y) = evalNGonVertex n k' (center, first)
            r1 = sqrt ( (toDouble (fst first) - toDouble (fst center))^2 + (toDouble (snd first) - toDouble (snd center))^2 )
            r2 = sqrt ( (toDouble x - toDouble (fst center))^2 + (toDouble y - toDouble (snd center))^2 )
        in abs (r1 - r2) < 1e-9
    , testCase "heptagon: k=n returns first vertex" $ do
        let n = 7
            center :: Point
            center = (0, 0)
            first  = (1, 0)
            v = evalNGonVertex n n (center, first)
        assertBool "same x" (approxEq 1e-9 (toDouble (fst v)) (toDouble (fst first)))
        assertBool "same y" (approxEq 1e-9 (toDouble (snd v)) (toDouble (snd first)))

    , testProperty "evaluation matches trig calculations" $ \(k0 :: Int, center :: Point, first :: Point) ->
        let
            n = 7
            k = abs k0 `mod` n
            vx = toDouble (fst first) - toDouble (fst center)
            vy = toDouble (snd first) - toDouble (snd center)
            r = sqrt (vx*vx + vy*vy)
            theta0 = atan2 vy vx
            theta = theta0 + 2 * pi * fromIntegral k / fromIntegral n
            expectedX = toDouble (fst center) + r * cos theta
            expectedY = toDouble (snd center) + r * sin theta
            (xR, yR) = evalNGonVertex n k (center, first)
        in r > 0 ==> (abs (toDouble xR - expectedX) < 1e-8 && abs (toDouble yR - expectedY) < 1e-8)
  ]
