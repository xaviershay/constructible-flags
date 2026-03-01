module FieldNumberBenchSpec (fieldNumberBenchmarks) where

import Test.Tasty.Bench
import Data.Ratio ((%))

import Flag.Construction.FieldNumber

fieldNumberBenchmarks :: Benchmark
fieldNumberBenchmarks = bgroup "Benchmarks/FieldNumber"
  [ bgroup "Addition"
    [ bench "FInteger + FInteger"       $ whnf (\x -> x + x) (fnInteger 42)
    , bench "FRational + FRational"     $ whnf (\x -> x + x) (fnRational (1%3))
    , bench "FIrrational + FIrrational" $ whnf (\x -> x + x) (sqrt (fnInteger 2))
    , bench "FCyclomatic + FInteger"    $ whnf (\x -> x + fnInteger 1) (fnCos (fnInteger 1))
    , bench "FReal + FIrrational"       $ whnf (\x -> x + sqrt (fnInteger 2)) (pi :: FieldNumber)
    ]

  , bgroup "Multiplication"
    [ bench "FInteger * FInteger"              $ whnf (\x -> x * x) (fnInteger 7)
    , bench "FIrrational * FIrrational (same)" $ whnf (\x -> x * x) (sqrt (fnInteger 2))
    , bench "FIrrational * FIrrational (diff)" $
        let y = sqrt (fnInteger 3)
        in whnf (\x -> x * y) (sqrt (fnInteger 2))
    , bench "FCyclomatic * FRational"          $
        whnf (\x -> x * fnRational (1%2)) (fnCos (fnInteger 1))
    ]

  , bgroup "Division"
    [ bench "FInteger / FInteger -> FInteger"  $ whnf (fnInteger 6 /) (fnInteger 3)
    , bench "FInteger / FInteger -> FRational" $ whnf (fnInteger 1 /) (fnInteger 3)
    , bench "FIrrational / FIrrational"        $
        let y = sqrt (fnInteger 3)
        in whnf (sqrt (fnInteger 2) /) y
    , bench "FReal / FReal"                    $ whnf ((pi :: FieldNumber) /) (pi :: FieldNumber)
    ]

  , bgroup "sqrt"
    [ bench "sqrt FInteger (perfect square: 4)"   $ whnf sqrt (fnInteger 4)
    , bench "sqrt FInteger (non-perfect: 2)"      $ whnf sqrt (fnInteger 2)
    , bench "sqrt FInteger (non-perfect: 5)"      $ whnf sqrt (fnInteger 5)
    , bench "sqrt FRational (perfect: 1%4)"       $ whnf sqrt (fnRational (1%4))
    , bench "sqrt FRational (non-perfect: 1%2)"   $ whnf sqrt (fnRational (1%2))
    , bench "sqrt FIrrational"                    $ whnf sqrt (sqrt (fnInteger 2))
    , bench "sqrt FCyclomatic"                    $ whnf sqrt (abs (fnCos (fnInteger 1)))
    ]

  , bgroup "Trig wrappers"
    [ bench "fnCos FInteger"   $ whnf fnCos (fnInteger 1)
    , bench "fnSin FRational"  $ whnf fnSin (fnRational (1%4))
    , bench "fnCos pi"         $ whnf fnCos (pi :: FieldNumber)
    , bench "exp FInteger"     $ whnf exp (fnInteger 1 :: FieldNumber)
    , bench "log FInteger"     $ whnf log (fnInteger 2 :: FieldNumber)
    ]

  , bgroup "Geometry kernels"
    -- Inline the core computation from evalIntersectLL' and evalIntersectCC'
    -- to benchmark FieldNumber in the actual geometric use case.
    [ bench "line-line intersection" $ whnf lineLine ()
    , bench "circle-circle intersection" $ whnf circleCircle ()
    , bench "sum of squares (distance^2)" $ whnf distSq ()
    ]
  ]

-- | Mimic evalIntersectLL' with small rational inputs.
lineLine :: () -> FieldNumber
lineLine _ =
  let x1 = fnInteger 0; y1 = fnInteger 0
      x2 = fnInteger 1; y2 = fnInteger 1
      x3 = fnInteger 0; y3 = fnInteger 1
      x4 = fnInteger 1; y4 = fnInteger 0
      denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
      t = ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom
  in x1 + t*(x2-x1)

-- | Mimic evalIntersectCC' with two unit circles offset by a rational.
circleCircle :: () -> FieldNumber
circleCircle _ =
  let x1 = fnInteger 0; y1 = fnInteger 0
      x2 = fnRational (6%5); y2 = fnInteger 0
      dx = x2-x1; dy = y2-y1
      d2 = dx*dx + dy*dy
      r1sq = fnInteger 1
      r2sq = fnInteger 1
      ad = (r1sq - r2sq + d2) / (2*d2)
      hd = sqrt (r1sq/d2 - ad*ad)
  in x1 + ad*dx + hd*dy

-- | Sum of squares (the d² step in evalIntersectCC').
distSq :: () -> FieldNumber
distSq _ =
  let cx = fnRational (6%5); cy = fnRational ((-3)%5)
      dx = cx; dy = cy - fnInteger (-3)
  in dx*dx + dy*dy
