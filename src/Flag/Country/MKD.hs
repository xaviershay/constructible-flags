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
import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone
import Flag.Source

northMacedonia :: (Sourced :> es) => Flag es
northMacedonia =
  editorNote
    """
    Dimensions measured from scan of the flag law. Outer ray dimensions match construction used on Wikipedia (and widely reproduced), but I find the diagonal ray convergence points lying on same circle of top and bottom rays implausible, even considering scan distortion. Refer to the difference between the red circle and the green quadrangle in the trace. Center convergence (used in this construction) matches more closely. Flags of the World construction sheet feels closer, but overspecified.
    """
    $ mkCountryFlag
      "MKD"
      "North Macedonia"
      constructedAt
      (reference "Description" flagSpec "TODO: add official flag description")
      design
  where
    constructedAt = "2026-03-15"
    gov = mkAgentOrg "mkd_gov" "Government of North Macedonia"

    flagSpec =
      attributeTo gov $
        mkEntity
          "TODO: add official flag specification title"
          "TODO: add URL"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      -- TODO: source dimensions from flagSpec
      redC <- referencePantoneAsRGB flagSpec ("Red", "485-C")
      yellowC <- referencePantoneAsRGB flagSpec ("Yellow", "108-C")
      whiteColor <- impliedReference "White" flagSpec (sRGB24 255 255 255)

      (pSunRadius, pMaskRadius, pNSConvergenceRadius, pDWidth, pHWidth) <- impliedReference "Proportions" flagSpec ((29 % 200, 36 % 200, 24 % 200, 3 % 10, 2 % 10))

      pure $ proc unit@(origin, _) -> do
        -- TODO: implement actual flag design
        (tl, tr, br, bl) <- boxNatural 2 1 -< unit

        topMid <- midpoint -< (tl, tr)
        leftMid <- midpoint -< (tl, bl)
        bottomMid <- midpoint -< (bl, br)

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

        centerDownUnit <- translate -< (downUnit, center)
        inner <- rationalMult pNSConvergenceRadius -< centerDownUnit
        (nConvergence, sConvergence) <- intersectLC -< ((topMid, bottomMid), (center, inner))

        rayNW <- fillTriangleMasked yellowC -< (tl, topA, center, sunMask)
        rayNE <- fillTriangleMasked yellowC -< (tr, topD, center, sunMask)
        raySW <- fillTriangleMasked yellowC -< (bl, bottomA, center, sunMask)
        raySE <- fillTriangleMasked yellowC -< (br, bottomD, center, sunMask)

        rayN <- fillTriangleMasked yellowC -< (topB, topC, nConvergence, sunMask)
        rayS <- fillTriangleMasked yellowC -< (bottomB, bottomC, sConvergence, sunMask)
        rayW <- fillTriangleMasked yellowC -< (leftA, leftB, center, sunMask)
        rayE <- fillTriangleMasked yellowC -< (rightA, rightB, center, sunMask)
        let rays = rayNW <> rayNE <> raySW <> raySE <> rayN <> rayS <> rayW <> rayE
        returnA -< bg <> sun <> rays

    fillTriangleMasked col = proc (p1, p2, p3, mask) -> do
      tri <- fillTriangle col -< (p1, p2, p3)
      maskDrawing -< (tri, mask)
