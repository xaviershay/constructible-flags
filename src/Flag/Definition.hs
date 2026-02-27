{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Definition
    ( Flag(flagId, flagName, flagDescription, flagDesign, flagEditorNote, flagUpdatedAt, flagCategory)
    , FlagCategory(..)
    , mkCountryFlag
    , mkOtherFlag
    , editorNote
    ) where

import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)

-- | Broad category of a flag, used for grouping in the index.
data FlagCategory = Country | Pride | Cultural
  deriving (Eq, Show)

-- | A flag with its metadata and construction
data Flag es = Flag'
  { flagId          :: String
  , flagName        :: String
  , flagUpdatedAt   :: String
  , flagDescription :: Eff es String
  , flagDesign      :: Eff es (FlagA (Point, Point) Drawing)
  , flagEditorNote  :: String
  , flagCategory    :: FlagCategory
  }

mkCountryFlag
  :: String
  -> String
  -> String
  -> Eff es String
  -> Eff es (FlagA (Point, Point) Drawing)
  -> Flag es
mkCountryFlag id_ name updatedAt desc design = Flag'
  { flagId          = id_
  , flagName        = name
  , flagUpdatedAt   = updatedAt
  , flagDescription = desc
  , flagDesign      = design
  , flagEditorNote  = ""
  , flagCategory    = Country
  }

mkOtherFlag
  :: FlagCategory
  -> String
  -> String
  -> String
  -> Eff es String
  -> Eff es (FlagA (Point, Point) Drawing)
  -> Flag es
mkOtherFlag category id_ name updatedAt desc design = Flag'
  { flagId          = id_
  , flagName        = name
  , flagUpdatedAt   = updatedAt
  , flagDescription = desc
  , flagDesign      = design
  , flagEditorNote  = ""
  , flagCategory    = category
  }

editorNote x f = f { flagEditorNote = x }
