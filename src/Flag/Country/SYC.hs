{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultilineStrings #-}

module Flag.Country.SYC
    ( seychelles
    ) where

import Data.Ratio
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

The proportions of the blue, yellow and red colours along the top length of
the flag is at a ratio of 1:1:1. This is the same for the red, white, green
colours on the fly side.
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
        blueC <- referencePantoneAsRGB act ("Blue", "294-C")
        yellowC <- referencePantoneAsRGB act ("Yellow", "122-C")
        redC <- referencePantoneAsRGB act ("Red", "1795-C")
        greenC <- referencePantoneAsRGB act ("Green", "356-C")
        whiteC <- editorial "White" [] (sRGB24 255 255 255)
        aspect <- reference "Proportions" guidelines (1 % 2)

        pure $ proc (origin, unit) -> do
            let unitV = (origin, unit)
            let t1 = unit
            (_, t2) <- intersectLC -< (unitV, (t1, origin))
            (_, t3) <- intersectLC -< (unitV, (t2, t1))

            (downUnit, _) <- perpendicular -< unitV
            l1 <- rationalMult aspect -< (origin, downUnit)

            (_, r1) <- translate   -< ((origin, l1), t3)
            (_, r2) <- intersectLC -< ((t3, r1), (r1, t3))
            (_, r3) <- intersectLC -< ((r1, r2), (r2, r1))

            (_, bl) <- translate -< ((r1, r3), l1)
            
            blueT   <- fillTriangle blueC   -< (origin, t1, bl)
            yellowT <- fillTriangle yellowC -< (t1, t2, bl)
            redT1   <- fillTriangle redC    -< (t2, t3, bl)
            redT2   <- fillTriangle redC    -< (t3, r1, bl)
            whiteT  <- fillTriangle whiteC  -< (r1, r2, bl)
            greenT  <- fillTriangle greenC  -< (r2, r3, bl)

            returnA -< blueT <> yellowT <> redT1 <> redT2 <> whiteT <> greenT
