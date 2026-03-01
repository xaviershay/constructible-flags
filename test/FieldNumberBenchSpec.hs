module FieldNumberBenchSpec (fieldNumberBenchmarks) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Ratio ((%))
import Flag.Construction.FieldNumber

fieldNumberBenchmarks :: Benchmark
-- Since most operations shouldn't change with field, only selected combinations are chosen.
fieldNumberBenchmarks =
  bgroup
    "Benchmarks/FieldNumber"
    [ bench "Addition: FCyclomatic + FInteger" $ whnf (\x -> x + fnInteger 1) (fnCos (fnInteger 1)),
      bench "Multiplication: FCyclomatic * FRational" $ whnf (\x -> x * fnRational (1 % 2)) (fnCos (fnInteger 1)),
      bgroup
        "Division"
        [ bench "FInteger / FInteger -> FInteger" $ whnf (fnInteger 6 /) (fnInteger 3),
          bench "FInteger / FInteger -> FRational" $ whnf (fnInteger 1 /) (fnInteger 3)
        ],
      bgroup
        "sqrt"
        [ bench "sqrt FInteger (perfect square: 4)" $ whnf sqrt (fnInteger 4),
          bench "sqrt FInteger (non-perfect: 2)" $ whnf sqrt (fnInteger 2),
          bench "sqrt FRational (perfect: 1%4)" $ whnf sqrt (fnRational (1 % 4)),
          bench "sqrt FRational (non-perfect: 1%2)" $ whnf sqrt (fnRational (1 % 2)),
          bench "sqrt FIrrational" $ whnf sqrt (sqrt (fnInteger 2))
        ]
    ]
