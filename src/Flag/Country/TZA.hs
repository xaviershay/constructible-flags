{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultilineStrings #-}

module Flag.Country.TZA
    ( tanzania
    ) where

import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Control.Arrow (returnA, (>>>))
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag)
import Flag.Pantone (referencePantoneAsRGB)

tanzania :: Sourced :> es => Flag es
tanzania = mkCountryFlag
  "TZA"
  "Tanzania"
  constructedAt
  (reference "Description" flagAct
    "A rectangular flag divided into two equal portions diagonally from the right-hand upper corner to the left-hand lower corner; the upper portion of green colour and the lower portion of blue colour; the dividing bands being of golden, black and golden collars; the two golden bands of equal size and smaller than the central black band"
    )
  design

  where
    constructedAt = "2026-07-11"
    gov = mkAgentOrg "tza_gov" "Government of Tanzania"

    flagAct = screenshot constructedAt "tza/emblems-act.png" $ attributeTo gov $ mkEntity
        "National Emblems Act, Chapter 10"
        "https://www.moha.go.tz/storage/laws_uploads/1752583375.pdf"

    flagSpec = screenshot constructedAt "tza/spec.png" $ attributeTo gov $ mkEntity
        "Tanzania National Symbols"
        "https://web.archive.org/web/20180927162049/https://www.tanzania.go.tz/index.php/home/pages/258/pages/258"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      blueC <- referencePantoneAsRGB flagSpec ("Blue", "299-C")
      greenC <- referencePantoneAsRGB flagSpec ("Green", "361-C")
      goldC <- referencePantoneAsRGB flagSpec ("Gold", "116-C")
      blackC <- reference "Black" flagSpec (sRGB24 0 0 0)

      (w, h, barWidth, stripeWidth) <- reference "Dimensions" flagSpec (6, 4, 13 % 48, 1 % 16)

      pure $ proc unit -> do
        (tl, tr, br, bl) <- boxNatural w h -< unit
        radius <- rationalMult (barWidth / 2) -< (bl, tl)
        radius2 <-  rationalMult (barWidth / 2 + stripeWidth) -< (bl, tl)

        (_, x1) <- intersectLC -< ((bl, tr), (bl, radius))
        (blb, blt) <- perpendicular >>> labelPair "BLB" "BLT" -< (bl, x1)

        (_, x2) <- intersectLC -< ((bl, tr), (bl, radius2))
        (blbs, blts) <- perpendicular >>> labelPair "BLBS" "BLTS" -< (bl, x2)

        (_, trb) <- translate -< ((bl, blb), tr)
        (_, trt) <- translate -< ((bl, blt), tr)
        (_, trbs) <- translate -< ((bl, blbs), tr)
        (_, trts) <- translate -< ((bl, blts), tr)

        t1 <- intersectLL >>> label "T1" -< ((blts, trts), (tl, tr))
        t2 <- intersectLL >>> label "T2" -< ((blt, trt), (tl, tr))
        b1 <- intersectLL >>> label "B1" -< ((blb, trb), (bl, br))
        b2 <- intersectLL >>> label "B2" -< ((blbs, trbs), (bl, br))
        l1 <- intersectLL >>> label "L1" -< ((blts, trts), (tl, bl))
        l2 <- intersectLL >>> label "L2" -< ((blt, trt), (tl, bl))
        r1 <- intersectLL >>> label "R1" -< ((blb, trb), (tr, br))
        r2 <- intersectLL >>> label "R2" -< ((blbs, trbs), (tr, br))

        green <- fillTriangle greenC -< (l1, tl, t1)
        gold1 <- fillRectangle goldC -< (l1, t1, t2, l2)
        gold2 <- fillRectangle goldC -< (b1, b2, r2, r1)
        blue <- fillTriangle blueC -< (b2, br, r2)
        black1 <- fillRectangle blackC -< (l2, bl, b1, r1)
        black2 <- fillRectangle blackC -< (r1, l2, t2, tr)

        returnA -< green <> gold1 <> gold2 <> blue <> black1 <> black2
