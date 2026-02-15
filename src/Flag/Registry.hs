{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , bangladesh
    , botswana
    , france
    , japan
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)

import Flag.Country.BGD (bangladesh)
import Flag.Country.BWA (botswana)
import Flag.Country.FRA (france)
import Flag.Country.JPN (japan)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ bangladesh
    , botswana
    , france
    , japan
    ]
