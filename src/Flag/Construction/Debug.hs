{-# LANGUAGE GADTs #-}

-- | Tracing interpreter for FlagA constructions.
-- Prints each geometric step with its inputs and outputs,
-- making it easy to spot where a construction goes wrong.
module Flag.Construction.Debug
  ( trace,
    tracePoints,
  )
where

import qualified Debug.Trace as DT
import Flag.Construction.FieldNumber (toKaTeX)
import Flag.Construction.Geometry
import Flag.Construction.Optimize
import Flag.Construction.Types

-- | Evaluate a construction arrow, printing every geometric step
-- with labeled inputs and outputs.
trace :: (Show a) => FlagA a Drawing -> a -> IO Drawing
trace fa input = do
  putStrLn $ "Input: " ++ show input
  result <- go 0 fa input
  putStrLn $ "Output: \n" ++ show (optimize result)
  pure result
  where
    indent n = replicate (n * 2) ' '

    go :: Int -> FlagA a b -> a -> IO b
    go _ (Arr _ f) a =
      let b = f a in b `seq` pure b
    go n (Compose f g) a = do
      b <- go n f a
      go n g b
    go n (First f) (a, c) = do
      b <- go n f a
      pure (b, c)
    go n (Par f g) (a, c) = do
      b <- go n f a
      d <- go n g c
      pure (b, d)
    go n IntersectLL input' = do
      let result = evalIntersectLL' input'
      putStrLn $ indent n ++ "IntersectLL " ++ show input' ++ " => " ++ show result
      pure result
    go n IntersectLC input' = do
      let result = evalIntersectLC' input'
      putStrLn $ indent n ++ "IntersectLC " ++ show input' ++ " => " ++ show result
      pure result
    go n IntersectCC input' = do
      let result = evalIntersectCC' input'
      putStrLn $ indent n ++ "IntersectCC " ++ show input' ++ " => " ++ show result
      pure result
    go _ (FillTriangle c) (p1, p2, p3) =
      pure $ DrawTriangle c p1 p2 p3
    go _ (FillCircle c) (center, edge) =
      pure $ DrawCircle c center (dist center edge)
    go _ (MaskDrawing mode) (content, maskD) =
      pure $ DrawMasked mode content maskD
    go n (NGonVertex n' k') input' = do
      let result = evalNGonVertex n' k' input'
      putStrLn $ indent n ++ "NGonVertex " ++ show n' ++ " " ++ show k' ++ " " ++ show input' ++ " => " ++ show result
      pure result
    go n (Group label f) a = do
      putStrLn $ indent n ++ ">> " ++ label
      go (n + 1) f a
    go n (LabelPoint label) p = do
      putStrLn $ indent n ++ "Label " ++ show label ++ " => " ++ show p
      pure p

-- | Trace a list of points to stderr as KaTeX expressions, returning
-- the list unchanged. For use in proc blocks:
-- @_ <- arr tracePoints -< [p1, p2, ...]@
tracePoints :: [Point] -> [Point]
tracePoints pts = DT.trace (unwords (map showPt pts)) pts
  where
    showPt (x, y) = "(" ++ toKaTeX x ++ ", " ++ toKaTeX y ++ ")"
