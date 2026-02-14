{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , bangladesh
    , botswana
    , japan
    , france
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)

import Flag.Country.BGD (bangladesh)
import Flag.Country.BWA (botswana)
import Flag.Country.JPN (japan)
import Flag.Country.FRA (france)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ bangladesh
    , botswana
    , japan
    , france
    ]
