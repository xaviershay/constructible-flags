module Main (main) where

import Criterion.Main (defaultMain)

import FieldNumberBenchSpec (fieldNumberBenchmarks)

main :: IO ()
main = defaultMain
  [ fieldNumberBenchmarks
  ]
