{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.JOR
    ( jordan
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag(..))

jordan :: Sourced :> es => Flag es
jordan = CountryFlag
  { flagIsoCode = "JOR"
  , flagName = "Jordan"
  , flagDescription = pure "TODO" -- reference "Description" constitution "TODO"
  , flagDesign = design
  }

  where
    _constructedAt = "2026-02-14"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- _ <- editorial "2:3 proportion" references ()
        pure $ proc (a, b) -> do
            d1 <- fillBox (sRGB24 200 200 200) 2 1 -< (a, b)
            (_, p) <- perpendicular -< (a, b)
            d2 <- fillStar7x2 (sRGB24 255 0 0) -< (a, p)
            --returnA -< d1 <> d2
            returnA -< d2
