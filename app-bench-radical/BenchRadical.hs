-- Benchmark for Radical arithmetic operations.
--
-- Design note: pure Haskell micro-benchmarks are prone to GHC sharing
-- (CAF-ification) when the inputs are compile-time constants.  To prevent
-- this, every benchmark reads its inputs from an IORef at runtime.  GHC
-- cannot determine the IORef's contents at compile time, so it must
-- re-execute the computation on each iteration.
--
-- The cost of one IORef read is ~5-10 ns.  All Radical operations measured
-- here are substantially more expensive, so this overhead is negligible.

module Main (main) where

import Control.Monad (replicateM_)
import Data.IORef
import Data.Ratio ((%))
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Flag.Construction.Radical

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

rat :: Integer -> Radical
rat = Rational . fromInteger

sqrtR :: Integer -> Radical
sqrtR = sqrtC . rat

-- A source of Radical inputs that cycles through a pre-computed list.
-- Reading from an IORef at runtime prevents GHC from sharing the
-- computation results across loop iterations.
data InputCycle = InputCycle { cycleRef :: IORef [Radical] }

mkCycle :: [Radical] -> IO InputCycle
mkCycle xs = InputCycle <$> newIORef (cycle xs)

next :: InputCycle -> IO Radical
next (InputCycle ref) = do
  (x:rest) <- readIORef ref
  writeIORef ref rest
  return x

-- Time @n@ iterations of @action@, printing wall and CPU time.
-- The returned Double is accumulated to prevent dead-code elimination.
bench :: String -> Int -> IO Double -> IO ()
bench label n action = do
  -- Warm up: one run outside the timed section.
  _ <- action

  acc      <- newIORef (0.0 :: Double)
  wallStart <- getMonotonicTimeNSec
  cpuStart  <- getCPUTime

  replicateM_ n $ do
    d <- action
    modifyIORef' acc (+ d)

  wallEnd <- getMonotonicTimeNSec
  cpuEnd  <- getCPUTime

  _ <- readIORef acc  -- prevent the accumulator from being eliminated

  let wallMs = fromIntegral (wallEnd - wallStart) / 1e6 :: Double
      cpuMs  = fromIntegral (cpuEnd  - cpuStart)  / 1e9 :: Double

  printf "%-44s %6d  %8.1f ms  %8.1f ms\n" label n wallMs cpuMs

-- ---------------------------------------------------------------------------
-- Input sets
-- ---------------------------------------------------------------------------

-- Small square roots of primes — representative radicands for flag geometry.
sqrtPrimes :: [Radical]
sqrtPrimes = map sqrtR [2, 3, 5, 7, 11, 13, 17, 19]

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Radical Arithmetic Benchmark ==="
  putStrLn "(inputs read from IORef each iteration to defeat GHC CAF sharing)"
  putStrLn ""
  printf "%-44s %6s  %10s  %10s\n"
    ("Operation" :: String) ("iters" :: String)
    ("Wall" :: String) ("CPU" :: String)
  putStrLn $ replicate 74 '-'

  -- -------------------------------------------------------------------------
  -- 1. Baseline: single-radicand operations
  --    Uses a cycle of 8 different sqrt-of-prime inputs.
  -- -------------------------------------------------------------------------
  c1 <- mkCycle sqrtPrimes
  bench "normalize: sqrt(prime)"       100000 $ toDouble <$> next c1

  c2 <- mkCycle sqrtPrimes
  bench "add: sqrt(p) + sqrt(p)"        50000 $ do
    r <- next c2; return $! toDouble (r + r)

  c3 <- mkCycle sqrtPrimes
  bench "mul: sqrt(p) * sqrt(p)"        50000 $ do
    r <- next c3; return $! toDouble (r * r)

  c4 <- mkCycle sqrtPrimes
  bench "div: rat(1) / sqrt(p)"         50000 $ do
    r <- next c4; return $! toDouble (rat 1 / r)

  putStrLn ""

  -- -------------------------------------------------------------------------
  -- 2. Multi-radicand sums — exercises canonicalOrder
  --
  --    Both orderings are timed with fresh inputs.  Descending order requires
  --    more canonicalOrder swaps (each addition of a new radicand triggers a
  --    re-normalize to bubble it into canonical position), so it should be
  --    noticeably slower if canonicalOrder has significant overhead.
  -- -------------------------------------------------------------------------

  -- Two distinct radicands, ascending vs descending.
  cPairs <- mkCycle sqrtPrimes
  bench "add asc:  a + b  (a < b)"      50000 $ do
    a <- next cPairs; b <- next cPairs
    let (lo, hi) = if toDouble a <= toDouble b then (a, b) else (b, a)
    return $! toDouble (lo + hi)

  cPairs2 <- mkCycle sqrtPrimes
  bench "add desc: b + a  (a < b)"      50000 $ do
    a <- next cPairs2; b <- next cPairs2
    let (lo, hi) = if toDouble a <= toDouble b then (a, b) else (b, a)
    return $! toDouble (hi + lo)   -- reversed

  -- Four radicands, ascending.
  c5 <- mkCycle sqrtPrimes
  bench "sum 4 asc:  a+b+c+d"           10000 $ do
    rs <- mapM (\_ -> next c5) [1..4::Int]
    let sorted = map snd $ sortBy (\(x,_) (y,_) -> compare x y)
                   [(toDouble r, r) | r <- rs]
    return $! toDouble (foldl1 (+) sorted)

  -- Four radicands, descending (worst case for canonicalOrder).
  c6 <- mkCycle sqrtPrimes
  bench "sum 4 desc: d+c+b+a"           10000 $ do
    rs <- mapM (\_ -> next c6) [1..4::Int]
    let sorted = map snd $ sortBy (\(x,_) (y,_) -> compare y x)
                   [(toDouble r, r) | r <- rs]   -- reversed
    return $! toDouble (foldl1 (+) sorted)

  -- Six radicands.
  c7 <- mkCycle sqrtPrimes
  bench "sum 6 asc"                       5000 $ do
    rs <- mapM (\_ -> next c7) [1..6::Int]
    let sorted = map snd $ sortBy (\(x,_) (y,_) -> compare x y)
                   [(toDouble r, r) | r <- rs]
    return $! toDouble (foldl1 (+) sorted)

  c8 <- mkCycle sqrtPrimes
  bench "sum 6 desc (worst case)"         5000 $ do
    rs <- mapM (\_ -> next c8) [1..6::Int]
    let sorted = map snd $ sortBy (\(x,_) (y,_) -> compare y x)
                   [(toDouble r, r) | r <- rs]
    return $! toDouble (foldl1 (+) sorted)

  putStrLn ""

  -- -------------------------------------------------------------------------
  -- 3. Products — exercises consolidation + canonicalOrder together
  -- -------------------------------------------------------------------------
  c9 <- mkCycle sqrtPrimes
  bench "mul: (1+a)*(1+b)"               20000 $ do
    a <- next c9; b <- next c9
    let ea = rat 1 + a; eb = rat 1 + b
    return $! toDouble (ea * eb)

  c10 <- mkCycle sqrtPrimes
  bench "mul: (1+a)*(1+b)*(1+c)"          5000 $ do
    a <- next c10; b <- next c10; c <- next c10
    return $! toDouble ((rat 1 + a) * (rat 1 + b) * (rat 1 + c))

  c11 <- mkCycle sqrtPrimes
  bench "sq:  (a+b)^2"                   20000 $ do
    a <- next c11; b <- next c11
    let e = a + b in return $! toDouble (e * e)

  c12 <- mkCycle sqrtPrimes
  bench "sq:  (a+b+c)^2"                 10000 $ do
    a <- next c12; b <- next c12; c <- next c12
    let e = a + b + c in return $! toDouble (e * e)

  c13 <- mkCycle sqrtPrimes
  bench "^4: (1+a+b)^4"                   2000 $ do
    a <- next c13; b <- next c13
    let e = rat 1 + a + b; e2 = e * e
    return $! toDouble (e2 * e2)

  putStrLn ""

  -- -------------------------------------------------------------------------
  -- 4. Division with irrational denominators
  -- -------------------------------------------------------------------------
  c14 <- mkCycle sqrtPrimes
  bench "div: 1/(a+b)"                    5000 $ do
    a <- next c14; b <- next c14
    return $! toDouble (rat 1 / (a + b))

  c15 <- mkCycle sqrtPrimes
  bench "div: 1/(a+b+c)"                  1000 $ do
    a <- next c15; b <- next c15; c <- next c15
    return $! toDouble (rat 1 / (a + b + c))

  putStrLn ""

  -- -------------------------------------------------------------------------
  -- 5. New passes: composeRoots and root index reduction
  --    Use a cycle of rational inputs rather than sqrt inputs.
  -- -------------------------------------------------------------------------
  let squarePowers   = map rat [4, 9, 16, 25, 36, 49, 64, 81]
  let fourthPowSeeds = map rat [4, 9, 16, 25]          -- 4^(1/4) = √2, etc.
  let cubeRoots      = map (nthRootC 3 . rat) [2,3,5,7]

  c16 <- mkCycle (map (nthRootC 2) squarePowers)    -- pre-normalized sqrt inputs
  bench "sqrt(sqrt(r))  (composeRoots)"  50000 $ do
    r <- next c16; return $! toDouble (sqrtC r)

  c17 <- mkCycle (map (nthRootC 3 . rat) [2,3,5,7,11,13])
  bench "sqrt(cbrt(r))  (composeRoots)"  50000 $ do
    r <- next c17; return $! toDouble (sqrtC r)

  c18 <- mkCycle fourthPowSeeds
  bench "nthRoot 4 (p^2) (index reduce)" 50000 $ do
    r <- next c18; return $! toDouble (nthRootC 4 r)

  c19 <- mkCycle (map rat [8,27,32,64])   -- p^3 and p^5 under 6th and 10th roots
  bench "nthRoot 6 (p^3) (index reduce)" 50000 $ do
    r <- next c19; return $! toDouble (nthRootC 6 r)

  putStrLn ""

-- ---------------------------------------------------------------------------
-- Minimal sort helper (avoid Data.List import for clarity)
-- ---------------------------------------------------------------------------

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ []     = []
sortBy cmp (x:xs) =
  sortBy cmp [y | y <- xs, cmp y x /= GT]
  ++ [x]
  ++ sortBy cmp [y | y <- xs, cmp y x == GT]
