{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.DZA
  ( algeria,
  )
where

import Control.Arrow (returnA)
import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, mkCountryFlag)
import Flag.Pantone
import Flag.Source

algeria :: (Sourced :> es) => Flag es
algeria =
  mkCountryFlag
    "DZA"
    "Algeria"
    constructedAt
    (reference "Description" flagSpec "TODO: add official flag description")
    design
  where
    constructedAt = "2026-02-28"
    gov = mkAgentOrg "dza_gov" "Government of Algeria"

    flagSpec =
      attributeTo gov $
        mkEntity
          "TODO: add official flag specification title"
          "TODO: add URL"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      -- TODO: source dimensions from flagSpec
      whiteC <- editorial "White" [] (sRGB24 255 255 255)
      greenC <- referencePantoneAsRGB flagSpec ("Green", "356-C")
      redC <- referencePantoneAsRGB flagSpec ("Red", "186-C")

      pure $ proc (o, unit) -> do
        (tl, tr, br, bl) <- boxNatural 3 2 -< (o, unit)

        outerCrescentRadius <- rationalMult (1 % 4) -< (tl, bl)
        innerCrescentRadius <- rationalMult (1 % 5) -< (tl, bl)

        -- Find a vector we can translate to center to find midpoint of star
        starRadiusDown <- rationalMult (1 % 8) -< (tl, bl)
        (_, starRadius) <- perpendicular -< (tl, starRadiusDown)

        (q, _) <- perpendicular -< (o, starRadius)

        mq <- midpoint -< (o, q)
        (_, n2) <- intersectLC -< ((o, q), (mq, starRadius))
        (f2, _) <- intersectLC -< ((o, starRadius), (o, n2))
        g2 <- midpoint -< (o, f2)

        topMid <- midpoint -< (tl, tr)
        bottomMid <- midpoint -< (bl, br)
        center <- midpoint -< (topMid, bottomMid)

        (_, starCenter) <- translate -< ((g2, o), center)
        (_, starEdge) <- translate -< ((o, starRadius), starCenter)
        (_, outerCrescentEdge) <- translate -< ((tl, outerCrescentRadius), center)

        let outerCrescentCenter = center

        (_, p) <- intersectCC -< ((outerCrescentCenter, outerCrescentEdge), (outerCrescentEdge, outerCrescentCenter))
        (_, q) <- intersectCC -< ((outerCrescentCenter, p), (p, outerCrescentCenter))

        (_, pEdge) <- translate -< ((o, innerCrescentRadius), p)
        (_, qEdge) <- translate -< ((o, innerCrescentRadius), q)

        (_, innerCrescentCenter) <- intersectCC -< ((p, pEdge), (q, qEdge))
        (_, innerCrescentEdge) <- translate -< ((tl, innerCrescentRadius), innerCrescentCenter)

        bgL <- fillRectangle greenC -< (tl, topMid, bottomMid, bl)
        bgR <- fillRectangle whiteC -< (topMid, bottomMid, br, tr)
        star <- fillStar5 redC -< (starCenter, starEdge)
        -- outerCrescent <- fillCircle redC -< (outerCrescentCenter, outerCrescentEdge)
        -- innerCrescent <- fillCircle whiteC -< (innerCrescentCenter, innerCrescentEdge)
        crescent <- fillCrescent redC -< ((outerCrescentCenter, outerCrescentEdge), (innerCrescentCenter, innerCrescentEdge))
        returnA -< bgL <> bgR <> star <> crescent
