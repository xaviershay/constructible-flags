{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Definition
    ( Flag(flagIsoCode, flagName, flagDescription, flagDesign, flagEditorNote, flagUpdatedAt)
    , mkCountryFlag
    , editorNote
    ) where

import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)

-- | A flag with its metadata and construction
data Flag es = CountryFlag
  { flagIsoCode     :: String
  , flagName        :: String
  , flagUpdatedAt   :: String
  , flagDescription :: Eff es String
  , flagDesign      :: Eff es (FlagA (Point, Point) Drawing)
  , flagEditorNote  :: String
  }

mkCountryFlag
  :: String
  -> String
  -> String
  -> Eff es String
  -> Eff es (FlagA (Point, Point) Drawing)
  -> Flag es
mkCountryFlag isoCode name updatedAt desc design = CountryFlag
  { flagIsoCode     = isoCode
  , flagName        = name
  , flagUpdatedAt   = updatedAt
  , flagDescription = desc
  , flagDesign      = design
  , flagEditorNote  = ""
  }

editorNote x f = f { flagEditorNote = x }