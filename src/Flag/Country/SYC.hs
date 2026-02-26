{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultilineStrings #-}

module Flag.Country.SYC
    ( seychelles
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Pantone
import Flag.Definition (Flag, mkCountryFlag)

seychelles :: Sourced :> es => Flag es
seychelles = mkCountryFlag
  "SYC"
  "Seychelles"
  (reference "Description" guidelines """
The National Flag is made up of five oblique bands of blue, yellow, red,
white and green, radiating from the bottom of the hoist side of the flag. The
oblique bands symbolise a dynamic country moving into the future.
""")
  design

  where
    constructedAt = "2026-02-26"
    gov = mkAgentOrg "syc_gov" "Government of Seychelles"
    
    guidelines = screenshot constructedAt "syc/national-symbols-guidelines.png" $ attributeTo gov $ mkEntity
      "Guidelines on Specifications and Correct Usage of the Seychelles' National Symbols"
      "https://mfa.gov.sc/wp-content/uploads/2023/04/Guidelines-for-Proper-use-of-National-Symbols-.pdf"
      -- April 2023

    act = screenshot constructedAt "syc/national-symbols-act.png" $ attributeTo gov $ mkEntity
        "National Symbols Act"
        "https://seylii.org/akn/sc/act/1996/3/eng@1996-06-18"
        -- 1996

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- TODO: source dimensions from flagSpec
        blueC <- referencePantoneAsRGB act ("Blue", "294-C")
        yellowC <- referencePantoneAsRGB act ("Yellow", "122-C")
        redC <- referencePantoneAsRGB act ("Red", "1795-C")
        greenC <- referencePantoneAsRGB act ("Green", "356-C")
        whiteC <- editorial "White" [] (sRGB24 255 255 255)

        pure $ proc (origin, unit) -> do
            -- TODO: implement actual flag design
            (tl, tr, br, bl) <- boxNatural 3 2 -< (origin, unit)
            bg <- fillRectangle whiteC -< (tl, tr, br, bl)
            returnA -< bg
