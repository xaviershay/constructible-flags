{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.DZA
    ( algeria
    ) where

import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Pantone
import Flag.Definition (Flag, mkCountryFlag)

algeria :: Sourced :> es => Flag es
algeria = mkCountryFlag
  "DZA"
  "Algeria"
  constructedAt
  (reference "Description" flagSpec "TODO: add official flag description")
  design

  where
    constructedAt = "2026-02-28"
    gov = mkAgentOrg "dza_gov" "Government of Algeria"

    flagSpec = attributeTo gov $ mkEntity
        "TODO: add official flag specification title"
        "TODO: add URL"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- TODO: source dimensions from flagSpec
        whiteC <- editorial "White" [] (sRGB24 255 255 255)
        greenC <- referencePantoneAsRGB flagSpec ("Green", "356-C")
        redC <- referencePantoneAsRGB flagSpec ("Red", "186-C")

        pure $ proc (o, unit) -> do
            -- TODO: implement actual flag design
            star <- fillStar5 redC -< (o, unit)

            -- Find a vector we can translate to center to find midpoint of star
            starRadius <- rationalMult (1 % 8) -< (o, unit)
            (q, _) <- perpendicular -< (o, starRadius)

            mq <- midpoint -< (o, q)
            (n1, n2) <- intersectLC -< ((o, q), (mq, starRadius))

            (_, f1) <- intersectLC -< ((o, starRadius), (o, n1))
            g1      <- midpoint    -< (o, f1)

            (f2, _) <- intersectLC -< ((o, starRadius), (o, n2))
            g2      <- midpoint    -< (o, f2)


            (_, g1q) <- translate -< ((o, q), g1)
            (_, g2q) <- translate -< ((o, q), g2)

            let v0 = unit
            (v1, v4) <- intersectLC -< ((g1, g1q), (o, unit))   -- 72° and 288°
            (v2, v3) <- intersectLC -< ((g2, g2q), (o, unit))   -- 144° and 216°

            (tl, tr, br, bl) <- boxNatural 2 1 -< (o, unit)

            topMid <- midpoint -< (tl, tr)
            bottomMid <- midpoint -< (bl, br)
            center <- midpoint -< (topMid, bottomMid)

            (_, starCenter) <- translate -< ((g2, o), center)
            (_, starEdge) <- translate -< ((o, starRadius), starCenter)


            bgL <- fillRectangle greenC -< (tl, topMid, bottomMid, bl)
            bgR <- fillRectangle whiteC -< (topMid, bottomMid, br, tr)
            --star <- fillStar5 redC -< (starCenter, starEdge)
            star <- fillCircle redC -< (starCenter, starEdge)
            returnA -< bgL <> bgR -- <> star
