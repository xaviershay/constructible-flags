{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.AUS
    ( australia
    ) where

import Data.Colour.SRGB (sRGB24)
import Data.Ratio ((%))
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag)
import Flag.Design.UnionJack (unionJack2to1)

australia :: Sourced :> es => Flag es
australia = mkCountryFlag
  "AUS"
  "Australia"
  (reference "Description" flagSpec "TODO: add official flag description")
  design

  where
    constructedAt = "2026-02-22"
    gov = mkAgentOrg "aus_gov" "Government of Australia"

    flagSpec = attributeTo gov $ mkEntity
        "TODO: add official flag specification title"
        "TODO: add URL"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- TODO: Proper colors, and line up with Union Jack
        whiteC <- impliedReference "White" flagSpec (sRGB24 255 255 255)
        blueC <- impliedReference "Blue" flagSpec (sRGB24 0 0 255)
        redC <- impliedReference "Red" flagSpec (sRGB24 255 0 0)
        jackArrow <- unionJack2to1

        pure $ proc (origin, unit) -> do
            let unitV = (origin, unit)
            -- TODO: implement actual flag design
            (tl, tr, br, bl) <- boxNatural 2 1 -< (origin, unit)
            topMid <- midpoint -< (tl, tr)
            leftMid <- midpoint -< (tl, bl)

            top14L  <- generateLine midpoint -< (tl, topMid)
            top34L  <- generateLine midpoint -< (topMid, tr)
            top5_8L  <- generateLine midpoint -< (topMid, fst top34L)
            top4_5L <- generateLine (rationalMult (3 % 4 + (1 % 10) / 2)) -< (tl, tr)
            top31_36L <- generateLine (rationalMult (3 % 4 + (2 % 9) / 2)) -< (tl, tr)

            left34L <- generateLine midpoint -< (leftMid, bl)
            left1_6L <- generateLine (rationalMult (1 % 6)) -< (tl, bl)
            left56L <- generateLine (rationalMult (5 % 6)) -< (tl, bl)
            left7_16L <- generateLine (rationalMult (7 % 16)) -< (tl, bl)
            left13_24L <- generateLine (rationalMult (1 % 2 + 1 % 24)) -< (tl, bl)
            left89_240L <- generateLine (rationalMult (1 % 2 - 1 % 15 - 1 % 16)) -< (tl, bl)

            -- TODO: Stars are likely half-width
            fedStar <- drawInCircleAt ((3 % 20) / 2) (fillStar7Inner (4 % 9) whiteC)
                -< ((left34L, top14L), unitV)

            alphaStar <- drawInCircleAt ((1 % 14) / 2) (fillStar7Inner (4 % 9) whiteC)
                -< ((left56L, top34L), unitV)

            betaStar <- drawInCircleAt ((1 % 14) / 2) (fillStar7Inner (4 % 9) whiteC)
                -< ((left7_16L, top5_8L), unitV)

            gammaStar <- drawInCircleAt ((1 % 14) / 2) (fillStar7Inner (4 % 9) whiteC)
                -< ((left1_6L, top34L), unitV)

            deltaStar <- drawInCircleAt ((1 % 14) / 2) (fillStar7Inner (4 % 9) whiteC)
                -< ((left89_240L, top31_36L), unitV)

            -- TODO: 5 pointed star, not 7
            epsilonStar <- drawInCircleAt ((1 % 24) / 2) (fillStar7Inner (4 % 9) whiteC)
                -< ((left13_24L, top4_5L), unitV)

            bg <- fillRectangle blueC -< (tl, tr, br, bl)

            -- The canton is the upper-left quarter of a 2:1 flag.
            jack <- jackArrow -< (tl, topMid)

            returnA -< bg <> jack <> fedStar <> alphaStar <> betaStar <> gammaStar <> deltaStar <> epsilonStar

    generateLine pointArrow = proc (a, b) -> do
        p      <- pointArrow -< (a, b)
        (_, q) <- perpendicular -< (p, b)
        returnA -< (p, q)

    drawInCircleAt radiusRatio fillOp = proc ((line1, line2), (orig, scaleUnit)) -> do
        center     <- intersectLL -< (line1, line2)
        radius     <- rationalMult radiusRatio -< (orig, scaleUnit)
        (_, edge)  <- translate -< ((orig, radius), center)
        (_, edgeN) <- perpendicular -< (center, edge)
        fillOp -< (center, edgeN)
