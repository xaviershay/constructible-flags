module Main (main) where

import Flag.RAL.Cli
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

ralJsonPath :: FilePath
ralJsonPath = "data/ral.json"

usage :: IO a
usage = die "Usage: ral-sample <CODE> <R> <G> <B>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [code, rs, gs, bs] -> case (readMaybe rs, readMaybe gs, readMaybe bs) of
      (Just r, Just g, Just b) -> do
        updateRALJson ralJsonPath code (r, g, b)
        putStrLn $ "Updated " ++ ralJsonPath ++ " — RAL " ++ code ++ " = (" ++ rs ++ "," ++ gs ++ "," ++ bs ++ ")"
      _ -> usage
    _ -> usage
