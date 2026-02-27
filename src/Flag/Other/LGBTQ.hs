{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Other.LGBTQ
    ( lgbtq
    ) where

import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, FlagCategory(..), mkOtherFlag)

lgbtq :: Sourced :> es => Flag es
lgbtq = mkOtherFlag
  Pride
  "LGBTQ"
  "Rainbow Pride Flag"
  constructedAt
  (editorial "Description" [] "Six equal horizontal stripes: red, orange, yellow, green, blue, and violet.")
  design

  where
    constructedAt = "2026-02-27"

    -- The six-stripe design was created by Gilbert Baker in 1978. The modern
    -- colour values are widely documented.
    colorSpec = mkEntity
        "Rainbow flag (pride) (Wikipedia)"
        "https://en.wikipedia.org/wiki/Rainbow_flag_(pride)"

    references = [colorSpec]

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- Colours taken from the widely-cited modern six-stripe specification.
        -- Original flag was hand-dyed; these RGB values are an editorial
        -- approximation of the canonical digital reproduction.
        redC    <- editorial "Red"    references (sRGB24 0xE4 0x03 0x03)
        orangeC <- editorial "Orange" references (sRGB24 0xFF 0x8C 0x00)
        yellowC <- editorial "Yellow" references (sRGB24 0xFF 0xED 0x00)
        greenC  <- editorial "Green"  references (sRGB24 0x00 0x80 0x26)
        blueC   <- editorial "Blue"   references (sRGB24 0x00 0x4D 0xFF)
        violetC <- editorial "Violet" references (sRGB24 0x75 0x07 0x87)

        -- Aspect ratio 2:3 (height:width). Six equal stripes give total height
        -- of 6 units; width of 9 units → 6:9 = 2:3.
        _ <- editorial "2:3 proportion" references ()

        pure $ horizontalStripes 9
            [ (1, redC)
            , (1, orangeC)
            , (1, yellowC)
            , (1, greenC)
            , (1, blueC)
            , (1, violetC)
            ]
