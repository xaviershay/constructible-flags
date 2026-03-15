{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.MKD
  ( northMacedonia,
  )
where

import Control.Arrow (arr, returnA, (>>>))
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone
import Flag.SharedSources (londonOlympicsFlagsManual)
import Flag.Source

northMacedonia :: (Sourced :> es) => Flag es
northMacedonia =
  editorNote
    """
    Dimensions measured from scan of the flag law. Outer ray dimensions match
    construction used on Wikipedia (and widely reproduced), but I find the ray
    convergence points implausible, even considering scan distortion. Flags of
    the World construction feels closer, but I opted for a different approach:
    give each ray the same arc length where it intersects with the circular
    mask. This seems more aethestically and geometrically consistent, with
    output very close to both the original law and the Flags of the World approach.
    """
    $ mkCountryFlag
      "MKD"
      "North Macedonia"
      constructedAt
      ( reference
          "Description"
          flagSpec
          """
          The flag of the Republic of Macedonia is red with a golden-yellow sun. The sun has eight rays that extend from the solar disc, widening toward the edges of the flag. The rays are interspersed diagonally, horizontally, and vertically.
          The diameter of the solar disc is one seventh of the length of the flag.
          The centre of the sun coincides with the point where the diagonals of the flag intersect.
          The ratio of the width to the length of the flag is one to two.
          """
      )
      design
  where
    constructedAt = "2026-03-15"
    gov = mkAgentOrg "mkd_gov" "Government of North Macedonia"

    album = mkEntity "Album des Pavillons" ""

    flagSpec =
      screenshot constructedAt "mkd/flag-law-1.png" $
        screenshot constructedAt "mkd/flag-law-2.png" $
          attributeTo gov $
            mkEntity
              "Official Gazette of the Republic. Macedonia No. 47/95"
              "https://zname.mk/doc/zakon-zname-1995.pdf"
    -- 1995

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      redP <- editorial "Red" [album, londonOlympicsFlagsManual] "485-C"
      redC <- pantoneToRGB "Red" redP
      yellowP <- editorial "Yellow" [album, londonOlympicsFlagsManual] "108-C"
      yellowC <- pantoneToRGB "Yellow" yellowP

      pAspectRatio <- reference "Aspect Ratio" flagSpec (2 :: Int)
      pSunDiameterVsLength <- reference "Sun Radius" flagSpec (1 % 7)
      (pMaskRadius, pConvergenceRatio, pDWidth, pHWidth) <- impliedReference "Proportions" flagSpec (36 % 200, 1 % 200, 3 % 10, 2 % 10)

      let convergeRayPoints = proc (converge, ctr, edge) -> do
            mp <- rationalMult pConvergenceRatio -< (converge, ctr)
            a <- perpendicular >>> arr fst -< (mp, converge)
            intersectLC -< ((a, mp), (ctr, edge))

      let pSunRadius = (pSunDiameterVsLength * (pAspectRatio % 1)) / 2
      pure $ proc unit@(origin, _) -> do
        (tl, tr, br, bl) <- boxNatural pAspectRatio 1 -< unit

        topMid <- midpoint -< (tl, tr)
        leftMid <- midpoint -< (tl, bl)

        negativeUnit <- intersectLC >>> arr fst -< (unit, unit)
        let negativeOrigin = (origin, negativeUnit)
        downUnit' <- perpendicular >>> arr snd -< unit
        let downUnit = (origin, downUnit')

        center <- intersectLL -< ((tl, br), (tr, bl))
        centerUnit <- translate -< (unit, center)
        sunEdge <- rationalMult pSunRadius -< centerUnit
        maskEdge <- rationalMult pMaskRadius -< centerUnit

        bg <- fillRectangle redC -< (tl, tr, br, bl)
        sun <- fillCircle yellowC -< (center, sunEdge)
        sunMask <- fillCircle yellowC -< (center, maskEdge)

        topA <- rationalMult pDWidth -< unit
        trNegativeOrigin <- translate -< (negativeOrigin, tr)
        topD <- rationalMult pDWidth -< trNegativeOrigin

        bottomA <- translate >>> arr snd -< ((origin, topA), bl)
        bottomD <- translate >>> arr snd -< ((tr, topD), br)

        topMidUnit <- translate -< (unit, topMid)
        topC <- rationalMult (pHWidth / 2) -< topMidUnit
        topB <- intersectLC >>> arr fst -< ((topMid, topC), (topMid, topC))
        bottomB <- translate >>> arr snd -< ((tl, topB), bl)
        bottomC <- translate >>> arr snd -< ((tl, topC), bl)

        leftMidUnit <- translate -< (downUnit, leftMid)
        leftB <- rationalMult (pHWidth / 2) -< leftMidUnit
        leftA <- intersectLC >>> arr fst -< ((leftMid, leftB), (leftMid, leftB))
        rightB <- translate >>> arr snd -< ((tl, leftB), tr)
        rightA <- translate >>> arr snd -< ((tl, leftA), tr)

        (convergeN, convergeS) <- intersectLC -< ((topMid, center), (center, maskEdge))
        (convergeW, convergeE) <- intersectLC -< ((leftMid, center), (center, maskEdge))

        convergeNMidpoint <- rationalMult (1 % 200) -< (convergeN, center)
        a <- perpendicular >>> arr fst -< (convergeNMidpoint, convergeN)
        (rayN_b, rayN_a) <- intersectLC -< ((a, convergeNMidpoint), (center, maskEdge))

        convergeSMidpoint <- rationalMult (1 % 200) -< (convergeS, center)
        a <- perpendicular >>> arr fst -< (convergeSMidpoint, convergeS)
        (rayS_b, rayS_a) <- intersectLC -< ((a, convergeSMidpoint), (center, maskEdge))

        (rayE_b, rayE_a) <- convergeRayPoints -< (convergeE, center, maskEdge)
        (rayW_b, rayW_a) <- convergeRayPoints -< (convergeW, center, maskEdge)

        (rayNW_a, raySE_a) <- intersectLC -< ((tl, br), (center, maskEdge))
        (rayNE_a, raySW_a) <- intersectLC -< ((tr, bl), (center, maskEdge))

        circleNW <- translate -< ((rayN_b, rayN_a), rayNW_a)
        rayNW_b <- intersectCC >>> arr snd -< (circleNW, (center, maskEdge))

        circleSE <- translate -< ((rayN_b, rayN_a), raySE_a)
        raySE_b <- intersectCC >>> arr snd -< (circleSE, (center, maskEdge))

        circleNE <- translate -< ((rayN_b, rayN_a), rayNE_a)
        rayNE_b <- intersectCC >>> arr fst -< (circleNE, (center, maskEdge))

        circleSW <- translate -< ((rayN_b, rayN_a), raySW_a)
        raySW_b <- intersectCC >>> arr fst -< (circleSW, (center, maskEdge))

        rayNW <- fillQuadMasked yellowC -< (tl, topA, rayNW_b, rayNW_a, sunMask)
        rayNE <- fillQuadMasked yellowC -< (tr, topD, rayNE_b, rayNE_a, sunMask)
        raySW <- fillQuadMasked yellowC -< (bl, bottomA, raySW_b, raySW_a, sunMask)
        raySE <- fillQuadMasked yellowC -< (br, bottomD, raySE_b, raySE_a, sunMask)

        rayN <- fillQuadMasked yellowC -< (topB, topC, rayN_b, rayN_a, sunMask)
        rayS <- fillQuadMasked yellowC -< (bottomB, bottomC, rayS_a, rayS_b, sunMask)
        rayW <- fillQuadMasked yellowC -< (leftA, leftB, rayW_b, rayW_a, sunMask)
        rayE <- fillQuadMasked yellowC -< (rightA, rightB, rayE_a, rayE_b, sunMask)
        let rays = rayNW <> rayNE <> raySW <> raySE <> rayN <> rayS <> rayW <> rayE
        returnA -< bg <> sun <> rays

    fillQuadMasked col = proc (p1, p2, p3, p4, mask) -> do
      tri <- fillRectangle col -< (p1, p2, p3, p4)
      maskDrawing -< (tri, mask)
