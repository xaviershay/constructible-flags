{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.ALA
  ( alandIslands,
  )
where

import Control.Arrow (arr, returnA, (>>>))
import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, mkCountryFlag)
import Flag.Source

alandIslands :: (Sourced :> es) => Flag es
alandIslands =
  mkCountryFlag
    "ALA"
    "Åland Islands"
    constructedAt
    ( reference
        "Description"
        flagSpec
        """
        The flag of Åland has a right-angled golden-yellow cross on a medium-blue background, in the middle of which is a red cross. A regional ordinance may provide more detailed guidelines for the colours of the flag.

        The flag is rectangular, its height is 17 units and its length is 26 units. The height of the medium-blue fields is 6 units, the length of the inner fields is 8 units and the length of the outer fields is 13 units. The width of the yellow cross is 5 units, with the width of the golden-yellow cross border being 1.5 units. The width of the red cross is 2 units.
        """
    )
    design
  where
    constructedAt = "2026-06-05"
    gov = mkAgentOrg "ala_gov" "Government of Åland Islands"

    ncsEntity = mkEntity "NCS" "https://www.ncscolorguide.com/"

    flagSpec =
      translated constructedAt $
        screenshot constructedAt "ala/aland-islands-spec.png" $
          attributeTo gov $
            mkEntity
              "Åland Self-Government Act (1991:71), Chapter 2, §2"
              "https://www.regeringen.ax/sites/default/files/law/code/a_1-26.pdf"

    flagColors =
      screenshot constructedAt "ala/aland-islands-colors.png" $
        attributeTo gov $
          mkEntity
            "Regional Ordinance (2004:15) on Guidelines for the Colours of the Åland Flag, §2"
            "https://www.regeringen.ax/sites/default/files/law/code/a_1-26.pdf"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      _ <- reference "Blue" flagColors "3065-R90B"
      blueC <- derivedFrom "Blue (RGB)" "Blue" ncsEntity (sRGB24 0 78 153)
      _ <- reference "Yellow" flagColors "0580-Y10R"
      yellowC <- derivedFrom "Yellow (RGB)" "Yellow" ncsEntity (sRGB24 255 194 0)
      _ <- reference "Red" flagColors "1085-Y90R"
      redC <- derivedFrom "Red (RGB)" "Red" ncsEntity (sRGB24 205 20 37)
      pure $ proc unit -> do
        let t0 = fst unit
        t1 <- rationalMult 8 >>> label "T1" -< unit
        t2 <- rationalMult (8 + (3 % 2)) >>> label "T2" -< unit
        t3 <- rationalMult (8 + (3 % 2) + 2) >>> label "T3" -< unit
        t4 <- translate >>> labelSecond "T4" >>> arr snd -< ((t1, t2), t3)
        t5 <- rationalMult (8 + (3 % 2) + 2 + (3 % 2) + 13) >>> label "T5" -< unit

        p <- perpendicular >>> arr fst -< unit
        let downUnit = (fst unit, p)

        let l0 = fst downUnit
        l1 <- rationalMult 6 >>> label "L1" -< downUnit
        l2 <- rationalMult (6 + 3 % 2) -< downUnit
        l3 <- rationalMult (6 + 3 % 2 + 2) -< downUnit
        (_, l4) <- translate -< ((l1, l2), l3)
        (_, l5) <- translate -< ((l0, l1), l4)

        let r0 = t5
        let top = (t0, t5)
        r1 <- translate >>> arr snd >>> label "R1" -< (top, l1)
        r2 <- translate >>> arr snd -< (top, l2)
        r3 <- translate >>> arr snd -< (top, l3)
        r4 <- translate >>> arr snd -< (top, l4)
        r5 <- translate >>> arr snd -< (top, l5)

        let b0 = l5
        let left = (l0, l5)
        b1 <- translate >>> arr snd >>> label "B1" -< (left, t1)
        b2 <- translate >>> arr snd -< (left, t2)
        b3 <- translate >>> arr snd -< (left, t3)
        b4 <- translate >>> arr snd -< (left, t4)
        b5 <- translate >>> arr snd -< (left, t5)

        l1xt1 <- intersectLL -< ((t1, b1), (l1, r1))
        l1xt4 <- intersectLL -< ((t4, b4), (l1, r1))
        l4xt1 <- intersectLL -< ((t1, b1), (l4, r4))
        l4xt4 <- intersectLL -< ((t4, b4), (l4, r4))

        l2xt2 <- intersectLL -< ((t2, b2), (l2, r2))
        l2xt3 <- intersectLL -< ((t3, b3), (l2, r2))
        l3xt2 <- intersectLL -< ((t2, b2), (l3, r3))
        l3xt3 <- intersectLL -< ((t3, b3), (l3, r3))

        blue1 <- fillRectangle blueC -< (t0, t1, l1xt1, l1)
        blue2 <- fillRectangle blueC -< (t4, t5, r1, l1xt4)
        blue3 <- fillRectangle blueC -< (b0, b1, l4xt1, l4)
        blue4 <- fillRectangle blueC -< (b4, b5, r4, l4xt4)

        let blue = blue1 <> blue2 <> blue3 <> blue4

        y1 <- fillRectangle yellowC -< (l1, l1xt1, l2xt2, l2)
        y2 <- fillRectangle yellowC -< (t1, l1xt1, l2xt2, t2)
        y3 <- fillRectangle yellowC -< (r1, l1xt4, l2xt3, r2)
        y4 <- fillRectangle yellowC -< (t4, l1xt4, l2xt3, t3)

        y5 <- fillRectangle yellowC -< (l4, l4xt1, l3xt2, l3)
        y6 <- fillRectangle yellowC -< (b1, l4xt1, l3xt2, b2)
        y7 <- fillRectangle yellowC -< (r4, l4xt4, l3xt3, r3)
        y8 <- fillRectangle yellowC -< (b4, l4xt4, l3xt3, b3)
        --
        let yellow = y1 <> y2 <> y3 <> y4 <> y5 <> y6 <> y7 <> y8

        red1 <- fillRectangle redC -< (t2, t3, l2xt3, l2xt2)
        red2 <- fillRectangle redC -< (l2, l3, l3xt2, l2xt2)
        red3 <- fillRectangle redC -< (b2, b3, l3xt3, l3xt2)
        red4 <- fillRectangle redC -< (r2, r3, l3xt3, l2xt3)
        red5 <- fillRectangle redC -< (l2xt2, l2xt3, l3xt3, l3xt2)

        let red = red1 <> red2 <> red3 <> red4 <> red5

        returnA -< blue <> yellow <> red
