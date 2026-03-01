module Main (main) where

import Test.Tasty.Bench

import FieldNumberBenchSpec (fieldNumberBenchmarks)

main :: IO ()
main = defaultMain
  [ fieldNumberBenchmarks
  ]
