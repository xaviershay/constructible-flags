{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( frenchFlag
    , Construction(..)
    , natural
    , runConstructionSVG
    , runConstructionTrace
    ) where

import Diagrams.Prelude hiding (trace, Dynamic)
import Diagrams.Backend.SVG
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (runState, modify)

-- | Effect for geometric constructions
data Construction :: Effect where
  -- | Get a natural number (1, 2, 3, ...)
  Natural :: Int -> Construction m Double

type instance DispatchOf Construction = 'Dynamic

-- | Get a natural number as a constructible value
natural :: Construction :> es => Int -> Eff es Double
natural n = send (Natural n)

-- | Interpreter that produces an SVG diagram
runConstructionSVG :: Eff (Construction : es) (Diagram B) -> Eff es (Diagram B)
runConstructionSVG = interpret_ $ \case
  Natural n -> pure (fromIntegral n)

-- | Interpreter that traces all construction operations
runConstructionTrace :: Eff (Construction : es) a -> Eff es (a, [String])
runConstructionTrace = reinterpret_ (runState @[String] []) $ \case
  Natural n -> do
    modify (++ ["natural " ++ show n])
    pure (fromIntegral n)

-- French flag: three vertical stripes (blue, white, red)
frenchFlag :: Construction :> es => Eff es (Diagram B)
frenchFlag = do
  w <- natural 1
  h <- natural 2
  let blueStripe = rect w h # fc blue # lw none
      whiteStripe = rect w h # fc white # lw none
      redStripe = rect w h # fc red # lw none
  pure $ blueStripe ||| whiteStripe ||| redStripe
