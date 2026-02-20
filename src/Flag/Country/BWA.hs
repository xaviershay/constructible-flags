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
import Flag.Definition (Flag, mkCountryFlag)

botswana :: Sourced :> es => Flag es
botswana = mkCountryFlag
  "BWA"
  "Botswana"
  ( reference "Description" law
      ( "Five horizontal stripes having colour and width as follows, that is to say taken from the top \8212\n"
     ++ "1st Stripe \8212 azure blue having a width equal to 9/24ths of the total depth of the flag.\n"
     ++ "2nd Stripe \8212 white having a width equal to 1/24th of such depth.\n"
     ++ "3rd Stripe \8212 black having a width equal to 4/24ths of such depth.\n"
     ++ "4th Stripe \8212 white having a width equal to 1/24th of such depth.\n"
     ++ "5th Stripe \8212 azure blue having a width equal to 9/24ths of such depth." ) )
  design

  where
    constructedAt = "2026-02-13"
    gov = mkAgentOrg "bwa_gov" "Botswanan Government"

    standards :: Entity
    standards = attributeTo gov $ mkEntity
      "National Flag Specification (2015)"
      ""

    law = screenshot constructedAt "bwa/statute.png" $ attributeTo gov $ mkEntity
            "Botswana Statute Law, Act 25"
            "https://sherloc.unodc.org/cld/uploads/res/document/immigration-consolidation-law--1966_html/Botswana_Statute_Law_1966.pdf"

    references =
      [ screenshot constructedAt "bwa/fotw.png" $ mkEntity
          "Botswana (Flags of the World)"
          "https://www.crwflags.com/fotw/flags/bw.html"
      ]

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        azureP <- unsightedReference "Azure" standards references "15-4225-TCX"
        azureC <- pantoneToRGB "Azure" azureP
        whiteC <- unsightedReference "White" standards references (sRGB24 255 255 255)
        blackC   <- unsightedReference "Black"  standards references (sRGB24 0 0 0)
        proportions <- reference "Stripe Heights" law [9, 1, 4, 1, 9]
        let colors = [azureC, whiteC, blackC, whiteC, azureC]

        _ <- editorial "2:3 proportion" references ()

        pure $ horizontalStripes 36 (zip proportions colors)
