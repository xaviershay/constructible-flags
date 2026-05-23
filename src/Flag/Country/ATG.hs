{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.ATG
  ( antiguaAndBarbuda,
  )
where

import Control.Arrow (returnA)
import Data.Colour.SRGB (sRGB24)
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, mkCountryFlag)
import Flag.Pantone
import Flag.SharedSources (tokyoOlympicsFlagsManual)
import Flag.Source

antiguaAndBarbuda :: (Sourced :> es) => Flag es
antiguaAndBarbuda =
  mkCountryFlag
    "ATG"
    "Antigua and Barbuda"
    constructedAt
    ( pure $
        """
        There is no official specification, and even the
        government website uses two different versions.
        Many construction sheets have a pointier star, I instead used a narrower
        geometry measured from the Tokyo 2020 games flag where the inner radius
        is the same as the height of the blue bar. This is similar to that used
        in the government website header. Simarly for colors, I used ones on the
        deeper end of the spectrum.
        """
    )
    design
  where
    constructedAt = "2026-05-23"
    gov = mkAgentOrg "atg_gov" "Government of Antigua and Barbuda"

    tokyoManual = screenshot constructedAt "atg/atg-tokyo-flags-and-anthems.png" tokyoOlympicsFlagsManual

    iconFlag =
      screenshot constructedAt "atg/icon_flag.png" $
        screenshot constructedAt "atg/symbol_flag.jpg" $
          attributeTo gov $
            mkEntity "Flag of Antigua and Barbuda (ab.gov.ag)" "https://ab.gov.ag/images/icon_flag.png"

    refs = [tokyoManual, iconFlag]

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      whiteC <- editorial "White" refs (sRGB24 255 255 255)
      redP <- editorial "Red" refs "186-C"
      redC <- pantoneToRGB "Red" redP
      blueP <- editorial "Blue" refs "300-C"
      blueC <- pantoneToRGB "Blue" blueP
      yellowP <- editorial "Yellow" refs "116-C"
      yellowC <- pantoneToRGB "Yellow" yellowP
      blackC <- editorial "Black" refs (sRGB24 0 0 0)
      _ <- editorial "Geometry" refs ()
      pure $ proc origin -> do
        -- TODO: implement actual flag design
        (tl, tr, br, bl) <- boxNatural 150 100 -< origin
        bottomMid <- midpoint -< (bl, br)

        baseP <- naturalMult 3 -< origin
        let base = (fst origin, baseP)

        redL <- fillTriangle redC -< (tl, bl, bottomMid)
        redR <- fillTriangle redC -< (tr, br, bottomMid)

        (down, _) <- perpendicular -< base
        l1 <- naturalMult 13 -< (fst origin, down)
        (_, l1p) <- perpendicular -< (l1, fst base)
        l2 <- naturalMult (13 + 7) -< (fst origin, down)
        (_, l2p) <- perpendicular -< (l2, fst base)

        dl1 <- intersectLL -< ((tl, bottomMid), (l1, l1p))
        dr1 <- intersectLL -< ((tr, bottomMid), (l1, l1p))
        dl2 <- intersectLL -< ((tl, bottomMid), (l2, l2p))
        dr2 <- intersectLL -< ((tr, bottomMid), (l2, l2p))

        starMid <- midpoint -< (dl1, dr1)

        starUnit <- translate -< (base, starMid)
        starInner <- naturalMult 7 -< starUnit
        starOuter <- naturalMult 11 -< starUnit

        blue <- fillRectangle blueC -< (dl1, dr1, dr2, dl2)
        black <- fillRectangle blackC -< (tl, dl1, dr1, tr)
        white <- fillTriangle whiteC -< (dl2, dr2, bottomMid)
        star <- fillStar16InnerC yellowC -< (starMid, starInner, starOuter)
        yellow <- clipDrawing -< (star, black)

        returnA -< black <> blue <> white <> yellow <> redL <> redR
