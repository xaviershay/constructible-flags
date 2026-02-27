{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Char (toLower)
import Effectful (runPureEff)
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Flag.Construction.Types (Point)
import Flag.Construction.Tree (ConstructionTree, evalTree)
import Flag.Definition (Flag(..))
import Flag.Registry (allCountryFlags)
import Flag.Render.DebugV2 (writeConstructionJson)
import Flag.Source (Sourced, runSourcedPure)

-- | Pre-evaluate a flag's construction tree (outside the timed section).
evalFlag :: Flag (Sourced : '[]) -> (String, String, (Point, Point), [ConstructionTree])
evalFlag flag =
  let flagArrow = runPureEff $ runSourcedPure $ flagDesign flag
      input = ((0, 0), (1, 0)) :: (Point, Point)
      (_, tree) = evalTree flagArrow input
  in (flagName flag, flagId flag, input, tree)

main :: IO ()
main = do
  putStrLn "=== JSON Generation Benchmark ==="
  putStrLn ""

  -- Pre-evaluate all trees (not timed)
  let evaluated = map evalFlag allCountryFlags

  -- Warm up (force lazy evaluation + IO)
  putStrLn "Warm-up run..."
  mapM_ (\(n, iso, inp, tree) -> writeConstructionJson n iso inp tree) evaluated
  putStrLn ""

  -- Timed runs
  putStrLn $ padR 8 "Flag" ++ padR 14 "Wall (ms)" ++ "CPU (ms)"
  putStrLn $ replicate 36 '-'

  totalWallStart <- getMonotonicTimeNSec
  totalCpuStart  <- getCPUTime

  mapM_ timeFlag evaluated

  totalWallEnd <- getMonotonicTimeNSec
  totalCpuEnd  <- getCPUTime

  let totalWallMs = fromIntegral (totalWallEnd - totalWallStart) / 1e6 :: Double
      totalCpuMs  = fromIntegral (totalCpuEnd - totalCpuStart) / 1e9 :: Double

  putStrLn $ replicate 36 '-'
  printf "%-8s%10.1f ms %10.1f ms\n" ("TOTAL" :: String) totalWallMs totalCpuMs

timeFlag :: (String, String, (Point, Point), [ConstructionTree]) -> IO ()
timeFlag (_, iso, input, tree) = do
  wallStart <- getMonotonicTimeNSec
  cpuStart  <- getCPUTime

  writeConstructionJson (iso) iso input tree

  wallEnd <- getMonotonicTimeNSec
  cpuEnd  <- getCPUTime

  let wallMs = fromIntegral (wallEnd - wallStart) / 1e6 :: Double
      cpuMs  = fromIntegral (cpuEnd - cpuStart) / 1e9 :: Double

  printf "%-8s%10.1f ms %10.1f ms\n" (map toLower iso) wallMs cpuMs

padR :: Int -> String -> String
padR n s = s ++ replicate (n - length s) ' '
