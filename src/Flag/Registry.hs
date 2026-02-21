{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , bangladesh
    , bhutan
    , botswana
    , france
    , unitedKingdom
    , jordan
    , japan
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)

import Flag.Country.BGD (bangladesh)
import Flag.Country.BTN (bhutan)
import Flag.Country.BWA (botswana)
import Flag.Country.FRA (france)
import Flag.Country.GBR (unitedKingdom)
import Flag.Country.JOR (jordan)
import Flag.Country.JPN (japan)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ bangladesh
    , bhutan
    , botswana
    , france
    , unitedKingdom
    , jordan
    , japan
    ]
