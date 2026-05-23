{-# LANGUAGE OverloadedStrings #-}

-- | Shared sources used in flag constructions.
module Flag.SharedSources
  ( londonOlympicsFlagsManual
  , tokyoOlympicsFlagsManual
  ) where

import Flag.Source (Agent, Entity, mkAgentOrg, mkEntity, attributeTo)

-- | The London Organising Committee of the Olympic Games and Paralympic Games Limited
locEntity :: Agent
locEntity = mkAgentOrg "loc2012" "London Organising Committee of the Olympic Games and Paralympic Games Limited"

-- | Flags and Anthems Manual, London 2012 (without screenshot)
londonOlympicsFlagsManual :: Entity
londonOlympicsFlagsManual =
  attributeTo locEntity $
    mkEntity
      "Flags and Anthems Manual, London 2012"
      "https://library.olympics.com/Default/doc/SYRACUSE/34593/flags-and-anthems-manual-london-2012-spp-final-version-london-organising-committee-of-the-olympic-ga?_lg=en-GB"

-- | The Tokyo Organising Committee of the Olympic and Paralympic Games
tocEntity :: Agent
tocEntity = mkAgentOrg "toc2020" "Tokyo Organising Committee of the Olympic and Paralympic Games"

-- | Flags and Anthems Manual, Tokyo 2020 (without screenshot)
tokyoOlympicsFlagsManual :: Entity
tokyoOlympicsFlagsManual =
  attributeTo tocEntity $
    mkEntity
      "Flags and Anthems Manual, Tokyo 2020"
      "https://library.olympics.com/Default/doc/SYRACUSE/1568069/flags-and-anthems-manual-tokyo-2020-the-tokyo-organising-committee-of-the-olympic-and-paralympic-gam?_lg=en-GB"
