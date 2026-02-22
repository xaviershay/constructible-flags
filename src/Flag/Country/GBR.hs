{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.GBR
    ( unitedKingdom
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag, editorNote)

unitedKingdom :: Sourced :> es => Flag es
unitedKingdom = editorNote (
       "A 5:3 proportion is used here for a flag to be flown on land. "
    ++ "2:1 would also be appropriate for one to be flown at sea. "
    ++ "This is potentially a controversial choice, 2:1 is often quoted as the \"official\" proportions ... by non-official sources. "
    ++ "The Flag Institute could have been a compelling alternate source, being the "
    ++ "origin of a 2008 bill (not passed) to better formalise the flag in law. "
    ++ "It specifies slightly different RGB approximations of the same "
    ++ "Pantone colors, but proscribes the same 5:3 proportions for use on land."
  ) $ mkCountryFlag
  "GBR"
  "United Kingdom"
  (do
    p1 <- reference "Description" gazette "\"And that the Union Flag shall be Azure, the Crosses Saltires of St. Andrew and St. Patrick Quarterly per Saltire, counterchanged Argent and Gules; the latter fimbriated of the Second, surmounted by the Cross of St. George of the Third, fimbriated as the Saltire.\""
    p2 <- reference "Description" flagInstitute "The Union Flag comprises three crosses on a royal blue background: a red St George's cross a white St Andrew's saltire a red St Patrick's saltire."
    return (p1 ++ "\n\n" ++ p2)
  )
  design

  where
    constructedAt = "2026-02-22"

    flagInstitute = screenshot constructedAt "gbr/flag-institute.png" $ mkEntity
      "The Flag Institute"
      "https://www.flaginstitute.org/wp/uk-flags/union-flag-specification/"

    gazette = screenshot constructedAt "gbr/gazette.png" $ mkEntity
      "King's Proclamation, reported in The London Gazette"
      "https://www.thegazette.co.uk/London/issue/15324/page/3"
      -- Dec 30, 1800

    flagSpec = screenshot constructedAt "gbr/construction-sheet.png" $ mkEntity
        "College of Arms"
        "https://www.college-of-arms.gov.uk/images/downloads/Union_Flag_5-3_guide_v3.pdf"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        (w, h, bigWide, mediumWide) <- reference "Dimensions" flagSpec (50, 30, 3, 2)
        blueC <- reference "Royal Blue" flagSpec (sRGB24 1 33 105)
        redC <- reference "Red" flagSpec (sRGB24 200 16 46)
        whiteC <- reference "White" flagSpec (sRGB24 255 255 255)

        let mkDiagLines = proc (corner, center', n2, n3) -> do
                (_, a) <- intersectLC -< ((corner, center'), (corner, n3))
                (b, b') <- perpendicular -< (a, corner)
                top <- parallel -< ((corner, a), b)
                bottom <- parallel -< ((corner, a), b')
                (_, c) <- intersectLC -< ((corner, center'), (corner, n2))
                (d, d') <- perpendicular -< (c, corner)
                midTop <- parallel -< ((corner, a), d)
                midBottom <- parallel -< ((corner, a), d')
                returnA -< (top, bottom, midTop, midBottom)
        pure $ proc (origin, unit) -> do
            (tl, tr, br, bl) <- boxNatural w h -< (origin, unit)
            let diagNWtoSE = (tl, br)
            center <- intersectLL -< (diagNWtoSE, (tr, bl))
            (_, unitDown) <- perpendicular -< (origin, unit)

            topMid <- midpoint -< (tl, tr)
            leftMid <- midpoint -< (tl, bl)

           -- Middle vertical stripe
            v4' <- naturalMult 5 -< (origin, unit)
            (_, v4) <- translate -< ((origin, v4'), topMid)
            (_, v4Down) <- parallel -< ((tl, bl), v4)

            (v1, _) <- intersectLC -< ((origin, topMid), (topMid, v4))
            (_, v1Down) <- parallel -< ((tl, bl), v1)

            v3' <- naturalMult 3 -< (origin, unit)
            (_, v3) <- translate -< ((origin, v3'), topMid)
            (_, v3Down) <- parallel -< ((tl, bl), v3)

            (v2, _) <- intersectLC -< ((origin, topMid), (topMid, v3))
            (_, v2Down) <- parallel -< ((tl, bl), v2)

           -- Middle horizontal stripe
            h3' <- naturalMult 3 -< (origin, unitDown)
            (_, h3) <- translate -< ((origin, h3'), leftMid)
            (_, h3Down) <- parallel -< ((tl, tr), h3)

            (_, h2) <- intersectLC -< ((origin, leftMid), (leftMid, h3))
            (_, h2Down) <- parallel -< ((tl, tr), h2)

            h1' <- naturalMult (bigWide + mediumWide) -< (origin, unitDown)
            (_, h1) <- translate -< ((origin, h1'), leftMid)
            (_, h1Down) <- parallel -< ((tl, tr), h1)

            (_, h4) <- intersectLC -< ((origin, leftMid), (leftMid, h1))
            (_, h4Down) <- parallel -< ((tl, tr), h4)

            -- NW to SE diagonal guide lines
            two <- naturalMult mediumWide -< (origin, unit)
            three <- naturalMult bigWide -< (origin, unit)
            (topLineNWtoSE, bottomLineNWtoSE, midTopLineNWtoSE, midBottomLineNWtoSE) <- mkDiagLines -< (tl, center, two, three)

            -- NE to SW diagonal guide lines
            let diagNEtoSW = (tr, bl)
            (_, twoFromTR) <- translate -< ((origin, two), tr)
            (_, threeFromTR) <- translate -< ((origin, three), tr)
            (bottomLineNEtoSW, topLineNEtoSW, midBottomLineNEtoSW, midTopLineNEtoSW) <- mkDiagLines -< (tr, center, twoFromTR, threeFromTR)

            -- Stripe boundary edge crossings
            h4_x_right <- intersectLL -< ((h4, h4Down), (tr, br))
            v1_x_bottom <- intersectLL -< ((v1, v1Down), (bl, br))
            v4_x_bottom <- intersectLL -< ((v4, v4Down), (bl, br))

            bg <- fillRectangle whiteC -< (tl, tr, br, bl)

            -- Red Stripes
            rVert <- fillRectangle redC -< (v2, v3, v3Down, v2Down)
            rHorz <- fillRectangle redC -< (h2, h3, h3Down, h2Down)

            -- === NW corner (NW→SE diagonal) ===
            topLineNWtoSE_x_top <- intersectLL -< ((tl, tr), topLineNWtoSE)
            bottomLineNWtoSE_x_left <- intersectLL -< ((tl, bl), bottomLineNWtoSE)
            topLineNWtoSE_x_v1 <- intersectLL -< ((v1, v1Down), topLineNWtoSE)
            bottomLineNWtoSE_x_h1 <- intersectLL -< (bottomLineNWtoSE, (h1, h1Down))
            midBottomLineNWtoSE_x_left <- intersectLL -< (midBottomLineNWtoSE, (tl, bl))
            midBottomLineNWtoSE_x_h1 <- intersectLL -< (midBottomLineNWtoSE, (h1, h1Down))
            diagNWtoSE_x_h1 <- intersectLL -< (diagNWtoSE, (h1, h1Down))

            bNWBig <- fillTriangle blueC -< (bottomLineNWtoSE_x_left, bottomLineNWtoSE_x_h1, h1)
            bNWSmall <- fillTriangle blueC -< (topLineNWtoSE_x_v1, v1, topLineNWtoSE_x_top)
            rNW <- fillRectangle redC -< (origin, diagNWtoSE_x_h1, midBottomLineNWtoSE_x_h1, midBottomLineNWtoSE_x_left)

            -- === SE corner (NW→SE diagonal) ===
            topLineNWtoSE_x_right <- intersectLL -< ((tr, br), topLineNWtoSE)
            topLineNWtoSE_x_h4 <- intersectLL -< ((h4, h4Down), topLineNWtoSE)
            bottomLineNWtoSE_x_v4 <- intersectLL -< ((v4, v4Down), bottomLineNWtoSE)
            bottomLineNWtoSE_x_bottom <- intersectLL -< (bottomLineNWtoSE, (bl, br))
            midTopLineNWtoSE_x_right <- intersectLL -< (midTopLineNWtoSE, (tr, br))
            midTopLineNWtoSE_x_h4 <- intersectLL -< (midTopLineNWtoSE, (h4, h4Down))
            diagNWtoSE_x_h4 <- intersectLL -< (diagNWtoSE, (h4, h4Down))

            bSEBig <- fillTriangle blueC -< (topLineNWtoSE_x_right, topLineNWtoSE_x_h4, h4_x_right)
            bSESmall <- fillTriangle blueC -< (bottomLineNWtoSE_x_v4, v4_x_bottom, bottomLineNWtoSE_x_bottom)
            rSE <- fillRectangle redC -< (br, diagNWtoSE_x_h4, midTopLineNWtoSE_x_h4, midTopLineNWtoSE_x_right)

            -- === NE corner (NE→SW diagonal) ===
            topLineNEtoSW_x_top <- intersectLL -< ((tl, tr), topLineNEtoSW)
            topLineNEtoSW_x_v4 <- intersectLL -< ((v4, v4Down), topLineNEtoSW)

            bottomLineNEtoSW_x_h1 <- intersectLL -< (bottomLineNEtoSW, (h1, h1Down))
            bottomLineNEtoSW_x_right <- intersectLL -< (bottomLineNEtoSW, (tr, br))
            midTopLineNEtoSW_x_v4 <- intersectLL -< (midTopLineNEtoSW, (v4, v4Down))
            midTopLineNEtoSW_x_top <- intersectLL -< (midTopLineNEtoSW, (tl, tr))
            diagNEtoSW_x_h1 <- intersectLL -< (diagNEtoSW, (h1, h1Down))
            v4_x_h1 <- intersectLL -< ((v4, v4Down), (h1, h1Down))

            bNEBig <- fillTriangle blueC -< (topLineNEtoSW_x_top, topLineNEtoSW_x_v4, v4)
            bNESmall <- fillTriangle blueC -< (bottomLineNEtoSW_x_h1, h1Down, bottomLineNEtoSW_x_right)

            -- TODO: Think about how to make this resilient to proportion changes
            rNE1 <- fillRectangle redC -< (tr, diagNEtoSW_x_h1, v4_x_h1, midTopLineNEtoSW_x_top)
            rNE2 <- fillTriangle redC -< (v4_x_h1, midTopLineNEtoSW_x_v4, midTopLineNEtoSW_x_top)

            -- === SW corner (NE→SW diagonal) ===
            topLineNEtoSW_x_left <- intersectLL -< ((tl, bl), topLineNEtoSW)
            topLineNEtoSW_x_h4 <- intersectLL -< ((h4, h4Down), topLineNEtoSW)
            bottomLineNEtoSW_x_v1 <- intersectLL -< ((v1, v1Down), bottomLineNEtoSW)
            bottomLineNEtoSW_x_bottom <- intersectLL -< (bottomLineNEtoSW, (bl, br))
            midBottomLineNEtoSW_x_bottom <- intersectLL -< (midBottomLineNEtoSW, (bl, br))
            midBottomLineNEtoSW_x_v1 <- intersectLL -< (midBottomLineNEtoSW, (v1, v1Down))
            diagNEtoSW_x_h4 <- intersectLL -< (diagNEtoSW, (h4, h4Down))
            v1_x_h4 <- intersectLL -< ((v1, v1Down), (h4, h4Down))

            bSWBig <- fillTriangle blueC -< (topLineNEtoSW_x_left, topLineNEtoSW_x_h4, h4)
            bSWSmall <- fillTriangle blueC -< (bottomLineNEtoSW_x_v1, v1_x_bottom, bottomLineNEtoSW_x_bottom)
            rSW1 <- fillRectangle redC -< (bl, diagNEtoSW_x_h4, v1_x_h4, midBottomLineNEtoSW_x_bottom)
            rSW2 <- fillTriangle redC -< (v1_x_h4, midBottomLineNEtoSW_x_v1, midBottomLineNEtoSW_x_bottom)


            returnA -< bg <> rVert <> rHorz
                <> bNWBig <> bNWSmall <> rNW
                <> bSEBig <> bSESmall <> rSE
                <> bNEBig <> bNESmall <> rNE1 <> rNE2
                <> bSWBig <> bSWSmall <> rSW1 <> rSW2
