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
        -- TODO: source dimensions from flagSpec
        whiteC <- impliedReference "White" flagSpec (sRGB24 255 255 255)
        blueC <- impliedReference "Blue" flagSpec (sRGB24 0 0 255)
        redC <- impliedReference "Red" flagSpec (sRGB24 255 0 0)
        jackArrow <- unionJack2to1

        pure $ proc (origin, unit) -> do
            -- TODO: implement actual flag design
            (tl, tr, br, bl) <- boxNatural 2 1 -< (origin, unit)
            topMid <- midpoint -< (tl, tr)
            top14 <- midpoint -< (tl, topMid)
            (top14Down, _) <- perpendicular -< (top14, origin)
            top34 <- midpoint -< (topMid, tr)
            (_, top34Down) <- perpendicular -< (top34, tr)
            
            leftMid <- midpoint -< (tl, bl)
            left34 <- midpoint -< (leftMid, bl)
            (_, left34Down) <- perpendicular -< (left34, bl)

            fedCenter <- intersectLL -< ((left34, left34Down), (top14, top14Down))
            fedRadius <- rationalMult ((3 % 20) / 2) -< (origin, unit)
            (_, fedEdge) <- translate -< ((origin, fedRadius), fedCenter)
            (_, fedEdgeN) <- perpendicular -< (fedCenter, fedEdge)

            fedStar <- fillStar7Inner (4 % 9) redC -< (fedCenter, fedEdgeN)

            --alphaCenter

            bg <- fillRectangle blueC -< (tl, tr, br, bl)

            -- The canton is the upper-left quarter of a 2:1 flag.
            --jack <- jackArrow -< (tl, topMid)

            returnA -< bg -- <> jack
