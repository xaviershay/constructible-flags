{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Definition
    ( Flag(flagIsoCode, flagName, flagDescription, flagDesign, flagEditorNote)
    , mkCountryFlag
    , editorNote
    ) where

import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)

-- | A flag with its metadata and construction
data Flag es = CountryFlag
  { flagIsoCode     :: String
  , flagName        :: String
  , flagDescription :: Eff es String
  , flagDesign      :: Eff es (FlagA (Point, Point) Drawing)
  , flagEditorNote  :: String
  }

mkCountryFlag
  :: String
  -> String
  -> Eff es String
  -> Eff es (FlagA (Point, Point) Drawing)
  -> Flag es
mkCountryFlag isoCode name desc design = CountryFlag
  { flagIsoCode     = isoCode
  , flagName        = name
  , flagDescription = desc
  , flagDesign      = design
  , flagEditorNote  = ""
  }

editorNote x f = f { flagEditorNote = x }