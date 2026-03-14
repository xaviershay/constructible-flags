{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , australia
    , bangladesh
    , bhutan
    , botswana
    , algeria
    , france
    , unitedKingdom
    , greenland
    , jordan
    , japan
    , marshallIslands
    , nepal
    , seychelles
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)

import Flag.Country.AUS (australia)
import Flag.Country.BGD (bangladesh)
import Flag.Country.BTN (bhutan)
import Flag.Country.BWA (botswana)
import Flag.Country.DZA (algeria)
import Flag.Country.FRA (france)
import Flag.Country.GBR (unitedKingdom)
import Flag.Country.GRL (greenland)
import Flag.Country.JOR (jordan)
import Flag.Country.JPN (japan)
import Flag.Country.MHL (marshallIslands)
import Flag.Country.NPL (nepal)
import Flag.Country.SYC (seychelles)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ australia
    , bangladesh
    , bhutan
    , botswana
    , algeria
    , france
    , unitedKingdom
    , greenland
    , jordan
    , japan
    , marshallIslands
    , nepal
    , seychelles
    ]
