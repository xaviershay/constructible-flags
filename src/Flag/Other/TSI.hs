{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Other.TSI
    ( torresStraitIslander
    ) where

import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, FlagCategory(..), mkOtherFlag, editorNote)

torresStraitIslander :: Sourced :> es => Flag es
torresStraitIslander =
  editorNote
    ( "STUB: the white dhari (headdress) and five-pointed star have not yet "
   ++ "been constructed. Only the five horizontal bands are currently rendered." )
  $ mkOtherFlag
    Cultural
    "TSI"
    "Torres Strait Islander Flag"
    constructedAt
    (editorial "Description" []
        ( "Five horizontal bands (green, black, blue, black, green) with a white "
       ++ "dhari (traditional headdress) and a five-pointed star centred on the "
       ++ "blue band." ))
    design

  where
    constructedAt = "2026-02-27"

    -- Designed by the Island Coordinating Council in 1992.
    flagSpec = mkEntity
        "Torres Strait Islander Flag (Wikipedia)"
        "https://en.wikipedia.org/wiki/Torres_Strait_Islander_Flag"

    references = [flagSpec]

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        greenC <- editorial "Green" references (sRGB24 0x00 0x99 0x57)
        blackC <- editorial "Black" references (sRGB24 0x00 0x00 0x00)
        blueC  <- editorial "Blue"  references (sRGB24 0x00 0x70 0xB7)

        -- Aspect ratio 1:2 (height:width). Five equal bands give total height
        -- of 5 units; width = 10 units → 5:10 = 1:2.
        _ <- editorial "1:2 proportion" references ()

        -- TODO: construct the dhari (traditional headdress) and five-pointed
        -- star. The dhari is an irregular organic shape best represented as an
        -- SVG overlay; the star can be built with fillStar5Inner once
        -- proportions are sourced.

        pure $ horizontalStripes 10
            [ (1, greenC)
            , (1, blackC)
            , (1, blueC)
            , (1, blackC)
            , (1, greenC)
            ]
