{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , france
    , japan
    , botswana
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)
import Flag.Country.France (france)
import Flag.Country.Japan (japan)
import Flag.Country.Botswana (botswana)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ france
    , japan
    , botswana
    ]
