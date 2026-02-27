{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Registry
    ( allCountryFlags
    , allOtherFlags
    , allFlags
    , australia
    , bangladesh
    , bhutan
    , botswana
    , france
    , unitedKingdom
    , jordan
    , japan
    , seychelles
    , aboriginal
    , lgbtq
    , transgender
    , torresStraitIslander
    ) where

import Flag.Source (Sourced)
import Flag.Definition (Flag)

import Flag.Country.AUS (australia)
import Flag.Country.BGD (bangladesh)
import Flag.Country.BTN (bhutan)
import Flag.Country.BWA (botswana)
import Flag.Country.FRA (france)
import Flag.Country.GBR (unitedKingdom)
import Flag.Country.JOR (jordan)
import Flag.Country.JPN (japan)
import Flag.Country.SYC (seychelles)
import Flag.Other.Aboriginal (aboriginal)
import Flag.Other.LGBTQ (lgbtq)
import Flag.Other.TRANS (transgender)
import Flag.Other.TSI (torresStraitIslander)

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ australia
    , bangladesh
    , bhutan
    , botswana
    , france
    , unitedKingdom
    , jordan
    , japan
    , seychelles
    ]

allOtherFlags :: [Flag (Sourced : '[])]
allOtherFlags =
    [ aboriginal
    , lgbtq
    , transgender
    , torresStraitIslander
    ]

allFlags :: [Flag (Sourced : '[])]
allFlags = allCountryFlags ++ allOtherFlags
