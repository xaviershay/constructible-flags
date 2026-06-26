{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultilineStrings #-}

module Flag.Country.JAM
    ( jamaica
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

jamaica :: Sourced :> es => Flag es
jamaica = mkCountryFlag
  "JAM"
  "Jamaica"
  constructedAt
  (reference "Description" flagSpec "The emblem has a diagonal cross or saltire with four triangles in juxtaposition. The cross is in GOLD and the width of each of its bends (arms) is one-sixth of the length of the fly of the flag. The top and bottom triangles are in GREEN, and the hoist and fly triangles are in BLACK. It follows the ‘Admiralty Pattern’ and the width-to-length ratio of the flag is 1:2.")
  design

  where
    constructedAt = "2026-06-27"
    gov = mkAgentOrg "jam_gov" "Jamaican Information Service"

    flagSpec = screenshot constructedAt "jam/spec-description.png" $ screenshot constructedAt "jam/spec-colors.png" $ attributeTo gov $ mkEntity
        "Jamaican Flag"
        "https://jis.gov.jm/information/symbols/jamaica-national-flag/"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blackC <- referencePantoneAsRGB flagSpec ("Black", "BLACK-C")
        greenC <- referencePantoneAsRGB flagSpec ("Green", "3415-C")
        goldC <- referencePantoneAsRGB flagSpec ("Gold", "PQ-1235C")
        pure $ proc origin -> do
            (tl, tr, br, bl) <- boxNatural 2 1 -< origin
            r <- rationalMult (1 % 6 / 2) -< origin

            (_, x1) <- intersectLC -< ((tl, br), (tl, r))
            (tlb, tlt) <- perpendicular >>> labelPair "TLB" "TLT" -< (tl, x1)

            (_, brb) <- translate -< ((tl, tlb), br)
            (_, brt) <- translate -< ((tl, tlt), br)

            (_, r2) <- translate -< ((tl, r), bl)

            (_, x2) <- intersectLC -< ((bl, tr), (bl, r2))
            (blb, blt) <- perpendicular >>> labelPair "BLB" "BLT" -< (bl, x2)

            (_, trb) <- translate -< ((bl, blb), tr)
            (_, trt) <- translate -< ((bl, blt), tr)

            t1 <- intersectLL >>> label "T1" -< ((tl, tr), (tlt, brt))
            t2 <- intersectLL >>> label "T2" -< ((tl, tr), (blt, trt))
            b1 <- intersectLL >>> label "B1" -< ((bl, br), (blb, trb))
            b2 <- intersectLL >>> label "B2" -< ((bl, br), (brb, tlb))
            l1 <- intersectLL >>> label "L1" -< ((tl, bl), (tlb, brb))
            l2 <- intersectLL >>> label "L2" -< ((tl, bl), (blt, trt))
            r1 <- intersectLL >>> label "R1" -< ((tr, br), (trb, blb))
            r2 <- intersectLL >>> label "R2" -< ((tr, br), (tlt, brt))

            tx <- intersectLL >>> label "TX" -< ((tlt, brt), (trt, blt))
            bx <- intersectLL >>> label "BX" -< ((blb, trb), (tlb, brb))
            lx <- intersectLL >>> label "LX" -< ((tlb, brb), (blt, trt))
            rx <- intersectLL >>> label "RX" -< ((blb, trb), (tlt, brt))

            black1 <- fillTriangle blackC -< (l1, l2, lx)
            black2 <- fillTriangle blackC -< (r1, r2, rx)
            green1 <- fillTriangle greenC -< (t1, t2, tx)
            green2 <- fillTriangle greenC -< (b1, b2, bx)

            g1 <- fillRectangle goldC -< (tx, rx, bx, lx)
            g2 <- fillRectangle goldC -< (l1, t1, tx, lx)
            g3 <- fillRectangle goldC -< (t2, r1, rx, tx)
            g4 <- fillRectangle goldC -< (r2, b2, bx, rx)
            g5 <- fillRectangle goldC -< (b1, l2, lx, bx)
            g6 <- fillTriangle goldC -< (tl, t1, l1)
            g7 <- fillTriangle goldC -< (tr, t2, r1)
            g8 <- fillTriangle goldC -< (br, r2, b2)
            g9 <- fillTriangle goldC -< (bl, l2, b1)

            returnA -< black1 <> black2 <> green1 <> green2 <> g1 <> g2 <> g3 <> g4 <> g5 <> g6 <> g7 <> g8 <> g9
