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

import Flag.Country.Bangladesh (bangladesh)
import Flag.Country.Botswana (botswana)
import Flag.Country.France (france)
import Flag.Country.Japan (japan)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ bangladesh
    , botswana
    , france
    , japan
    ]
