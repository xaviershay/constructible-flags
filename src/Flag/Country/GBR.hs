{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.GBR
    ( unitedKingdom
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA, arr)
import Debug.Trace (trace)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Construction.Radical (toKaTeX)
import Flag.Definition (Flag, mkCountryFlag)

unitedKingdom :: Sourced :> es => Flag es
unitedKingdom = mkCountryFlag
  "GBR"
  "United Kingdom"
  (reference "Description" flagSpec "TODO: add official flag description")
  design

  where
    constructedAt = "2026-02-21"
    gov = mkAgentOrg "gbr_gov" "Government of United Kingdom"

    flagSpec = attributeTo gov $ mkEntity
        "TODO: add official flag specification title"
        "TODO: add URL"

    tracePoints :: [Point] -> [Point]
    tracePoints pts = trace (unwords (map showPt pts)) pts
      where showPt (x, y) = "(" ++ toKaTeX x ++ ", " ++ toKaTeX y ++ ")"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- TODO: source dimensions from flagSpec
        blueC <- impliedReference "White" flagSpec (sRGB24 0 0 255)
        redC <- impliedReference "White" flagSpec (sRGB24 255 0 0)
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
            -- TODO: implement actual flag design
            (tl, tr, br, bl) <- boxNatural 50 30 -< (origin, unit)
            let diagNWtoSE = (tl, br)
            center <- intersectLL -< (diagNWtoSE, (tr, bl))
            (_, unitDown) <- perpendicular -< (origin, unit)

            topMid <- midpoint -< (tl, tr)
            leftMid <- midpoint -< (tl, bl)


            --(c, _) <- intersectLC -< ((a, b), (a, origin))

            
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

            h1' <- naturalMult 5 -< (origin, unitDown)
            (_, h1) <- translate -< ((origin, h1'), leftMid)
            (_, h1Down) <- parallel -< ((tl, tr), h1)

            (_, h4) <- intersectLC -< ((origin, leftMid), (leftMid, h1))
            (_, h4Down) <- parallel -< ((tl, tr), h4)

            -- NW to SE diagonal guide lines
            two <- naturalMult 2 -< (origin, unit)
            three <- naturalMult 3 -< (origin, unit)
            (topLineNWtoSE, bottomLineNWtoSE, midTopLineNWtoSE, midBottomLineNWtoSE) <- mkDiagLines -< (tl, center, two, three)

            -- NE to SW diagonal guide lines
            let diagNEtoSW = (tr, bl)
            (_, twoFromTR) <- translate -< ((origin, two), tr)
            (_, threeFromTR) <- translate -< ((origin, three), tr)
            (bottomLineNEtoSW, topLineNEtoSW, midBottomLineNEtoSW, midTopLineNEtoSW) <- mkDiagLines -< (tr, center, twoFromTR, threeFromTR)

            -- Stripe boundary edge crossings
            h1_x_right <- intersectLL -< ((h1, h1Down), (tr, br))
            h4_x_right <- intersectLL -< ((h4, h4Down), (tr, br))
            v1_x_bottom <- intersectLL -< ((v1, v1Down), (bl, br))
            v4_x_bottom <- intersectLL -< ((v4, v4Down), (bl, br))

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

            bottomLineNEtoSW_x_v4 <- intersectLL -< ((v4, v4Down), bottomLineNEtoSW)
            bottomLineNEtoSW_x_h1 <- intersectLL -< (bottomLineNEtoSW, (h1, h1Down))
            bottomLineNEtoSW_x_right <- intersectLL -< (bottomLineNEtoSW, (tr, br))
            midTopLineNEtoSW_x_h1 <- intersectLL -< (midTopLineNEtoSW, (h1, h1Down))
            midTopLineNEtoSW_x_top <- intersectLL -< (midTopLineNEtoSW, (tl, tr))
            diagNEtoSW_x_h1 <- intersectLL -< (diagNEtoSW, (h1, h1Down))

            bNEBig <- fillTriangle blueC -< (topLineNEtoSW_x_top, topLineNEtoSW_x_v4, v4)
            bNESmall <- fillTriangle blueC -< (bottomLineNEtoSW_x_h1, h1Down, bottomLineNEtoSW_x_right)

            -- TODO: Need to crop the end of this red triangle for 3:5 proportions
            rNE <- fillRectangle redC -< (tr, diagNEtoSW_x_h1, midTopLineNEtoSW_x_h1, midTopLineNEtoSW_x_top)

            -- === SW corner (NE→SW diagonal) ===
            topLineNEtoSW_x_left <- intersectLL -< ((tl, bl), topLineNEtoSW)
            topLineNEtoSW_x_h4 <- intersectLL -< ((h4, h4Down), topLineNEtoSW)
            bottomLineNEtoSW_x_v1 <- intersectLL -< ((v1, v1Down), bottomLineNEtoSW)
            bottomLineNEtoSW_x_bottom <- intersectLL -< (bottomLineNEtoSW, (bl, br))
            midBottomLineNEtoSW_x_bottom <- intersectLL -< (midBottomLineNEtoSW, (bl, br))
            midBottomLineNEtoSW_x_h4 <- intersectLL -< (midBottomLineNEtoSW, (h4, h4Down))
            diagNEtoSW_x_h4 <- intersectLL -< (diagNEtoSW, (h4, h4Down))

            bSWBig <- fillTriangle blueC -< (topLineNEtoSW_x_left, topLineNEtoSW_x_h4, h4)
            bSWSmall <- fillTriangle blueC -< (bottomLineNEtoSW_x_v1, v1_x_bottom, bottomLineNEtoSW_x_bottom)
            rSW <- fillRectangle redC -< (bl, diagNEtoSW_x_h4, midBottomLineNEtoSW_x_h4, midBottomLineNEtoSW_x_bottom)

            returnA -< rVert <> rHorz
                <> bNWBig <> bNWSmall <> rNW
                <> bSEBig <> bSESmall <> rSE
                <> bNEBig <> bNESmall <> rNE
                <> bSWBig <> bSWSmall <> rSW
