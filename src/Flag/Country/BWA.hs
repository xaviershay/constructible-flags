{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.BWA
    ( botswana
    ) where

import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Pantone
import Flag.Definition (Flag(..))

botswana :: Sourced :> es => Flag es
botswana = CountryFlag
  { flagIsoCode = "BWA"
  , flagName = "Botswana"
  , flagDescription = sourced "Description" law
      ( "Five horizontal stripes having colour and width as follows, that is to say taken from the top \8212\n"
     ++ "1st Stripe \8212 azure blue having a width equal to 9/24ths of the total depth of the flag.\n"
     ++ "2nd Stripe \8212 white having a width equal to 1/24th of such depth.\n"
     ++ "3rd Stripe \8212 black having a width equal to 4/24ths of such depth.\n"
     ++ "4th Stripe \8212 white having a width equal to 1/24th of such depth.\n"
     ++ "5th Stripe \8212 azure blue having a width equal to 9/24ths of such depth." )
  , flagDesign = design
  }

  where
    standards :: Source
    standards = SourceAuthoritativeWebsite
      "National Flag Specification"
      ""
    -- 2015, Botswana Bureau of Standards

    law :: Source
    law = SourceLaw
        "Botswana Statute Law, Act 25"
        "https://sherloc.unodc.org/cld/uploads/res/document/immigration-consolidation-law--1966_html/Botswana_Statute_Law_1966.pdf"
        -- 1966

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        azureP <- sourced "Azure Pantone" standards PMS154225TCX
        azureC <- pmsToRGB azureP
        whiteC <- sourced "White" standards (sRGB24 255 255 255)
        blackC   <- sourced "Black"  standards (sRGB24 0 0 0)
        proportions <- sourced "Stripe Heights" standards [9, 1, 4, 1, 9] -- TODO: Verify source
        let colors = [azureC, whiteC, blackC, whiteC, azureC]

        _ <- sourced "2:3 proportion" standards ()

        pure $ horizontalStripes 36 (zip proportions colors)
