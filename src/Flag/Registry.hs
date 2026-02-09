{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , france
    , japan
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)
import Flag.Country.France (france)
import Flag.Country.Japan (japan)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ france
    , japan
    ]
