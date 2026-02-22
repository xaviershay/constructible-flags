{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Design.UnionJack
    ( unionJack5to3
    , unionJack2to1
    , unionJackGazette
    , unionJackFlagInstitute
    , unionJackFlagSpec53
    , unionJackFlagSpec21
    , unionJackBlueRGB
    , unionJackRedRGB
    ) where

import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)
import Data.Ratio ((%))
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source

unionJackBlueRGB = sRGB24 1 33 105
unionJackRedRGB = sRGB24 200 16 46

constructedAt :: String
constructedAt = "2026-02-22"

unionJackFlagInstitute :: Entity
unionJackFlagInstitute = screenshot constructedAt "gbr/flag-institute.png" $ mkEntity
    "The Flag Institute"
    "https://www.flaginstitute.org/wp/uk-flags/union-flag-specification/"

unionJackGazette :: Entity
unionJackGazette = screenshot constructedAt "gbr/gazette.png" $ mkEntity
    "King's Proclamation, reported in The London Gazette"
    "https://www.thegazette.co.uk/London/issue/15324/page/3"
    -- Dec 30, 1800

unionJackFlagSpec53 :: Entity
unionJackFlagSpec53 = screenshot constructedAt "gbr/construction-sheet.png" $ mkEntity
    "College of Arms"
    "https://www.college-of-arms.gov.uk/images/downloads/Union_Flag_5-3_guide_v3.pdf"

unionJackFlagSpec21 :: Entity
unionJackFlagSpec21 = screenshot constructedAt "gbr/college-of-arms-21.png" $ mkEntity
    "College of Arms"
    "https://www.college-of-arms.gov.uk/images/downloads/Union_Flag_2-1_guide_v3.pdf"

-- | The Union Jack in 5:3 proportions (for use on land).
unionJack5to3 :: Sourced :> es => Colour Double -> Colour Double -> Eff es (FlagA (Point, Point) Drawing)
unionJack5to3 blue red = mkUnionJack unionJackFlagSpec53 50 30 blue red

-- | The Union Jack in 2:1 proportions (for use at sea, or embedded in cantons).
unionJack2to1 :: Sourced :> es => Colour Double -> Colour Double -> Eff es (FlagA (Point, Point) Drawing)
unionJack2to1 blue red = mkUnionJack unionJackFlagSpec21 60 30 blue red

mkUnionJack :: Sourced :> es => Entity -> Int -> Int -> Colour Double -> Colour Double -> Eff es (FlagA (Point, Point) Drawing)
mkUnionJack spec w h blueC redC = do
    (bigWide, mediumWide) <- reference "Stripe Widths" spec (3 :: Int, 2 :: Int)
    whiteC <- reference "White" spec (sRGB24 255 255 255)

    let
        prop53 = w == 50 && h == 30
        prop21 = w == 60 && h == 30
    
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

        -- Statically selected based on flag proportions: narrower flags
        -- have the mid-line intersect h1 before v4, requiring a rectangle + triangle;
        -- wider flags use a single rectangle.
        rNEArrow
          | prop53 = proc (tr', diag_x_h1, v4_h1, mid_x_v4, _, mid_x_top) -> do
              r1 <- fillRectangle redC -< (tr', diag_x_h1, v4_h1, mid_x_top)
              r2 <- fillTriangle redC -< (v4_h1, mid_x_v4, mid_x_top)
              returnA -< r1 <> r2
          | prop21 = proc (tr', diag_x_h1, _v4_h1, _mid_x_v4, mid_x_h1, mid_x_top) ->
              fillRectangle redC -< (tr', diag_x_h1, mid_x_h1, mid_x_top)
          | otherwise = error "Unsupported proportions"

        rSWArrow
          | w == 50 && h == 30 = proc (bl', diag_x_h4, v1_h4, mid_x_v1, mid_x_bottom) -> do
              r1 <- fillRectangle redC -< (bl', diag_x_h4, v1_h4, mid_x_bottom)
              r2 <- fillTriangle redC -< (v1_h4, mid_x_v1, mid_x_bottom)
              returnA -< r1 <> r2
          | w == 60 && h == 30 = proc (bl', diag_x_h4, _v1_h4, mid_x_v1, mid_x_bottom) ->
              fillRectangle redC -< (bl', diag_x_h4, mid_x_v1, mid_x_bottom)
          | otherwise = error "Unsupported proportions"

    pure $ proc (tl, tr) -> do
        unit <- rationalMult (1 % w) -< (tl, tr)
        (downUnit, _) <- perpendicular -< (tl, unit)
        bl <- naturalMult 30 -< (tl, downUnit)
        br <- quad -< (tl, tr, bl)

        let diagNWtoSE = (tl, br)
        center <- intersectLL -< (diagNWtoSE, (tr, bl))
        (_, unitDown) <- perpendicular -< (tl, unit)

        topMid <- midpoint -< (tl, tr)
        leftMid <- midpoint -< (tl, bl)

       -- Middle vertical stripe
        v4' <- naturalMult 5 -< (tl, unit)
        (_, v4) <- translate -< ((tl, v4'), topMid)
        (_, v4Down) <- parallel -< ((tl, bl), v4)
                                                                                                                                                                   
        (v1, _) <- intersectLC -< ((tl, topMid), (topMid, v4))
        (_, v1Down) <- parallel -< ((tl, bl), v1)

        v3' <- naturalMult 3 -< (tl, unit)
        (_, v3) <- translate -< ((tl, v3'), topMid)
        (_, v3Down) <- parallel -< ((tl, bl), v3)

        (v2, _) <- intersectLC -< ((tl, topMid), (topMid, v3))
        (_, v2Down) <- parallel -< ((tl, bl), v2)

       -- Middle horizontal stripe
        h3' <- naturalMult 3 -< (tl, unitDown)
        (_, h3) <- translate -< ((tl, h3'), leftMid)
        (_, h3Down) <- parallel -< ((tl, tr), h3)

        (_, h2) <- intersectLC -< ((tl, leftMid), (leftMid, h3))
        (_, h2Down) <- parallel -< ((tl, tr), h2)

        h1' <- naturalMult (bigWide + mediumWide) -< (tl, unitDown)
        (_, h1) <- translate -< ((tl, h1'), leftMid)
        (_, h1Down) <- parallel -< ((tl, tr), h1)

        (_, h4) <- intersectLC -< ((tl, leftMid), (leftMid, h1))
        (_, h4Down) <- parallel -< ((tl, tr), h4)

        -- NW to SE diagonal guide lines
        two <- naturalMult mediumWide -< (tl, unit)
        three <- naturalMult bigWide -< (tl, unit)
        (topLineNWtoSE, bottomLineNWtoSE, midTopLineNWtoSE, midBottomLineNWtoSE) <- mkDiagLines -< (tl, center, two, three)

        -- NE to SW diagonal guide lines
        let diagNEtoSW = (tr, bl)
        (_, twoFromTR) <- translate -< ((tl, two), tr)
        (_, threeFromTR) <- translate -< ((tl, three), tr)
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
        rNW <- fillRectangle redC -< (tl, diagNWtoSE_x_h1, midBottomLineNWtoSE_x_h1, midBottomLineNWtoSE_x_left)

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
        midTopLineNEtoSW_x_h1 <- intersectLL -< (midTopLineNEtoSW, (h1, h1Down))
        midTopLineNEtoSW_x_top <- intersectLL -< (midTopLineNEtoSW, (tl, tr))
        diagNEtoSW_x_h1 <- intersectLL -< (diagNEtoSW, (h1, h1Down))
        v4_x_h1 <- intersectLL -< ((v4, v4Down), (h1, h1Down))

        bNEBig <- fillTriangle blueC -< (topLineNEtoSW_x_top, topLineNEtoSW_x_v4, v4)
        bNESmall <- fillTriangle blueC -< (bottomLineNEtoSW_x_h1, h1Down, bottomLineNEtoSW_x_right)

        rNE <- rNEArrow -< (tr, diagNEtoSW_x_h1, v4_x_h1, midTopLineNEtoSW_x_v4, midTopLineNEtoSW_x_h1, midTopLineNEtoSW_x_top)

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
        rSW <- rSWArrow -< (bl, diagNEtoSW_x_h4, v1_x_h4, midBottomLineNEtoSW_x_v1, midBottomLineNEtoSW_x_bottom)

        returnA -< bg <> rVert <> rHorz
            <> bNWBig <> bNWSmall <> rNW
            <> bSEBig <> bSESmall <> rSE
            <> bNEBig <> bNESmall <> rNE
            <> bSWBig <> bSWSmall <> rSW
