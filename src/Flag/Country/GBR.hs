{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.GBR
    ( unitedKingdom
    ) where

import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Source
import Flag.Definition (Flag, mkCountryFlag, editorNote)
import Flag.Design.UnionJack (unionJack5to3, unionJackGazette, unionJackFlagInstitute, unionJackFlagSpec53, unionJackBlueRGB, unionJackRedRGB)

unitedKingdom :: Sourced :> es => Flag es
unitedKingdom = editorNote (
       "A 5:3 proportion is used here for a flag to be flown on land. "
    ++ "2:1 would also be appropriate for one to be flown at sea. "
    ++ "This is potentially a controversial choice, 2:1 is often quoted as the \"official\" proportions ... by non-official sources. "
    ++ "The Flag Institute could have been a compelling alternate source, being the "
    ++ "origin of a 2008 bill (not passed) to better formalise the flag in law. "
    ++ "It specifies slightly different RGB approximations of the same "
    ++ "Pantone colors, but proscribes the same 5:3 proportions for use on land."
  ) $ mkCountryFlag
  "GBR"
  "United Kingdom"
  constructedAt
  (do
    p1 <- reference "Description" unionJackGazette "\"And that the Union Flag shall be Azure, the Crosses Saltires of St. Andrew and St. Patrick Quarterly per Saltire, counterchanged Argent and Gules; the latter fimbriated of the Second, surmounted by the Cross of St. George of the Third, fimbriated as the Saltire.\""
    p2 <- reference "Description" unionJackFlagInstitute "The Union Flag comprises three crosses on a royal blue background: a red St George's cross a white St Andrew's saltire a red St Patrick's saltire."
    return (p1 ++ "\n\n" ++ p2)
  )
  (do
    blueC <- reference "Royal Blue" unionJackFlagSpec53 unionJackBlueRGB
    redC  <- reference "Red"        unionJackFlagSpec53 unionJackRedRGB
    unionJack5to3 blueC redC
  )

  where
    constructedAt = "2026-02-22"
