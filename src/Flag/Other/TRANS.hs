{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Other.TRANS
    ( transgender
    ) where

import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, FlagCategory(..), mkOtherFlag)

transgender :: Sourced :> es => Flag es
transgender = mkOtherFlag
  Pride
  "TRANS"
  "Transgender Pride Flag"
  constructedAt
  (editorial "Description" [] "Five equal horizontal stripes: light blue, light pink, white, light pink, light blue.")
  design

  where
    constructedAt = "2026-02-27"

    -- Designed by Monica Helms in 1999.
    colorSpec = mkEntity
        "Transgender Pride Flag (Wikipedia)"
        "https://en.wikipedia.org/wiki/Transgender_pride_flag"

    references = [colorSpec]

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- Colours from the widely-documented specification.
        lightBlueC <- editorial "Light Blue" references (sRGB24 0x55 0xCD 0xFC)
        lightPinkC <- editorial "Light Pink" references (sRGB24 0xF7 0xA8 0xB8)
        whiteC     <- editorial "White"      references (sRGB24 0xFF 0xFF 0xFF)

        -- Aspect ratio 2:3 (height:width). Five equal stripes give total height
        -- of 5 units; width of 7.5 would give exact 2:3; using 10:15 preserves
        -- integer arithmetic.
        _ <- editorial "2:3 proportion" references ()

        pure $ horizontalStripes 15
            [ (2, lightBlueC)
            , (2, lightPinkC)
            , (2, whiteC)
            , (2, lightPinkC)
            , (2, lightBlueC)
            ]
