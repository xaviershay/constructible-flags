{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , alandIslands
    , antiguaAndBarbuda
    , australia
    , bangladesh
    , bhutan
    , botswana
    , algeria
    , france
    , unitedKingdom
    , greenland
    , jamaica
    , jordan
    , japan
    , marshallIslands
    , northMacedonia
    , nepal
    , rwanda
    , seychelles
    , trinidadAndTobago
    , tanzania
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)

import Flag.Country.ALA (alandIslands)
import Flag.Country.ATG (antiguaAndBarbuda)
import Flag.Country.AUS (australia)
import Flag.Country.BGD (bangladesh)
import Flag.Country.BTN (bhutan)
import Flag.Country.BWA (botswana)
import Flag.Country.DZA (algeria)
import Flag.Country.FRA (france)
import Flag.Country.GBR (unitedKingdom)
import Flag.Country.GRL (greenland)
import Flag.Country.JAM (jamaica)
import Flag.Country.JOR (jordan)
import Flag.Country.JPN (japan)
import Flag.Country.MHL (marshallIslands)
import Flag.Country.MKD (northMacedonia)
import Flag.Country.NPL (nepal)
import Flag.Country.RWA (rwanda)
import Flag.Country.SYC (seychelles)
import Flag.Country.TTO (trinidadAndTobago)
import Flag.Country.TZA (tanzania)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ alandIslands
    , antiguaAndBarbuda
    , australia
    , bangladesh
    , bhutan
    , botswana
    , algeria
    , france
    , unitedKingdom
    , greenland
    , jamaica
    , jordan
    , japan
    , marshallIslands
    , northMacedonia
    , nepal
    , rwanda
    , seychelles
    , trinidadAndTobago
    , tanzania
    ]
