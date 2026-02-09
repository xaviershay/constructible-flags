{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Definition
    ( Flag(..)
    ) where

import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)

-- | A flag with its metadata and construction
data Flag es = CountryFlag
  { flagIsoCode     :: String
  , flagName        :: String
  , flagDescription :: Eff es String
  , flagDesign      :: Eff es (FlagA (Point, Point) Drawing)
  }
