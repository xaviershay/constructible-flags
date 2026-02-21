{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Effectful (runPureEff)

import Flag.Construction.Types (Point)
import Flag.Construction.Debug (trace)
import Flag.Source (runSourcedPure)
import Flag.Registry
import Flag.Definition (Flag(..))

main :: IO ()
main = do
    let flagArrow = runPureEff $ runSourcedPure $ flagDesign unitedKingdom
        flagInput = ((0, 0), (1, 0)) :: (Point, Point)
    _ <- trace flagArrow flagInput
    pure ()
