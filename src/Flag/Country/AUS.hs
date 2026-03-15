{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.AUS
  ( australia,
  )
where

import Control.Arrow (returnA)
import Control.Monad (unless)
import Data.Colour.SRGB (sRGB24)
import Data.Ratio (Ratio, (%))
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Design.UnionJack (unionJack2to1, unionJackBlueRGB, unionJackFlagSpec21, unionJackRedRGB)
import Flag.Source

australia :: (Sourced :> es) => Flag es
australia =
  editorNote
    """
    For consistency with GBR, the Pantone approximations provided by the College of Arms are used rather than the Pantone sourced ones typically used.
    """
    $ mkCountryFlag
      "AUS"
      "Australia"
      constructedAt
      ( reference
          "Description"
          flagSpec
          """
          The Australian National Flag is a blue flag, and the Australian Red Ensign is a red flag.  Each of the flags has:

          (a)  the Union Jack occupying the upper quarter next the staff;
          (b)  a large white star (representing the 6 States of Australia and the Territories) in the centre of the lower quarter next the staff and pointing direct to the centre of St George's Cross in the Union Jack, and
          (c)  5 white stars (representing the Southern Cross) in the half of the flag further from the staff.
          """
      )
      design
  where
    constructedAt = "2026-02-22"
    gov = mkAgentOrg "aus_gov" "Government of Australia"

    pmcFlagBooklet =
      screenshot constructedAt "aus/pmc.png" $
        attributeTo gov $
          mkEntity
            "Australian Flags"
            "https://www.pmc.gov.au/resources/australian-flags-booklet"

    flagSpec =
      screenshot constructedAt "aus/flags-act.png" $
        attributeTo gov $
          mkEntity
            "Flags Act 1953"
            "https://www.austlii.edu.au/cgi-bin/viewdoc/au/legis/cth/consol_act/fa195361/sch1.html"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      _ <- reference "Royal Blue" pmcFlagBooklet "280-C"
      _ <- reference "Red" pmcFlagBooklet "186-C"

      let blueRGB = unionJackBlueRGB
          redRGB = unionJackRedRGB
      whiteC <- reference "White" unionJackFlagSpec21 (sRGB24 255 255 255)
      blueC <- derivedFrom "Royal Blue (RGB)" "Royal Blue" unionJackFlagSpec21 blueRGB
      redC <- derivedFrom "Red (RGB)" "Red" unionJackFlagSpec21 redRGB

      jackArrow <- unionJack2to1 blueC redC

      (l, w) <- reference "Proportions" flagSpec (2, 1)
      (innerStarRatio, fedDiameter, starDiameter, tinyStarDiameter) <- reference "Star Sizes" flagSpec (4 % 9, 3 % 10, 1 % 7, 1 % 12)
      ( (fedX, fedY),
        (alphaX, alphaY),
        (betaX, betaY),
        (gammaX, gammaY),
        (deltaX, deltaY),
        (epsilonX, epsilonY)
        ) <-
        reference
          "Star Positions"
          flagSpec
          ( -- "On middle line of Union Jack, one-quarter width of flag from
            -- bottom edge of flag."
            (1 % 4 :: Ratio Int, 3 % 4 :: Ratio Int),
            -- "On middle line, one-sixth from bottom edge."
            (0 :: Ratio Int, 1 % 6),
            -- "One-quarter from middle line, at right angles on left to a
            -- point on middle line one-sixteenth above centre of fly.
            (-1 % 4 :: Ratio Int, -1 % 16),
            -- "On middle line one-sixth from top edge."
            (0 :: Ratio Int, 1 % 6),
            -- "Two-ninths from middle line at right angles on right to a
            -- point one-fifteenth above a point on middle line
            -- one-sixteenth above centre of fly."
            (2 % 9, -(1 % 15 + 1 % 16) :: Ratio Int),
            -- "One-tenth from middle line at right angles on right to a point on middle line
            -- one twenty-fourth below centre of fly"
            (1 % 10, 1 % 24 :: Ratio Int)
          )

      -- Most dimensions are specified in relation to the width, but our unit
      -- vector is for the length, hence many need to be multiplied by the
      -- aspect.
      let aspect = w % l
      let starRadius = starDiameter * aspect
      let flyCenter = 1 % 2
      let middleLine = 3 % 4

      unless (fedX == 1 % 4 && fedY == 3 % 4 && alphaX == 0 && betaX == -1 % 4 && gammaX == 0) $
        error "Unsupported star positioning due to construction optimisations"

      pure $ proc (origin, unit) -> do
        let unitV = (origin, unit)
        (tl, tr, br, bl) <- boxNatural l w -< (origin, unit)
        topMid <- midpoint -< (tl, tr)
        leftMid <- midpoint -< (tl, bl)

        topFedL <- generateLine midpoint -< (tl, topMid)
        topMiddleL <- generateLine midpoint -< (topMid, tr)

        let topAlphaL = topMiddleL
            topGammaL = topMiddleL

        topBetaL <- generateLine midpoint -< (topMid, fst topMiddleL)
        topDeltaL <- generateLine (rationalMult (middleLine + deltaX * aspect)) -< (tl, tr)
        topEpsilonL <- generateLine (rationalMult (middleLine + epsilonX * aspect)) -< (tl, tr)

        leftFedL <- generateLine midpoint -< (leftMid, bl)
        leftAlphaL <- generateLine (rationalMult (1 - alphaY)) -< (tl, bl)
        leftBetaL <- generateLine (rationalMult (flyCenter + betaY)) -< (tl, bl)
        leftGammaL <- generateLine (rationalMult gammaY) -< (tl, bl)
        leftDeltaL <- generateLine (rationalMult (flyCenter + deltaY)) -< (tl, bl)
        leftEpsilonL <- generateLine (rationalMult (flyCenter + epsilonY)) -< (tl, bl)

        bg <- fillRectangle blueC -< (tl, tr, br, bl)

        fedStar <-
          drawInCircleAt (fedDiameter * aspect) (fillStar7Inner innerStarRatio whiteC)
            -<
              ((leftFedL, topFedL), unitV)

        alphaStar <-
          drawInCircleAt starRadius (fillStar7Inner innerStarRatio whiteC)
            -<
              ((leftAlphaL, topAlphaL), unitV)

        betaStar <-
          drawInCircleAt starRadius (fillStar7Inner innerStarRatio whiteC)
            -<
              ((leftBetaL, topBetaL), unitV)

        gammaStar <-
          drawInCircleAt starRadius (fillStar7Inner innerStarRatio whiteC)
            -<
              ((leftGammaL, topGammaL), unitV)

        deltaStar <-
          drawInCircleAt starRadius (fillStar7Inner innerStarRatio whiteC)
            -<
              ((leftDeltaL, topDeltaL), unitV)

        epsilonStar <-
          drawInCircleAt (tinyStarDiameter * aspect) (fillStar5Inner innerStarRatio whiteC)
            -<
              ((leftEpsilonL, topEpsilonL), unitV)

        canton <- jackArrow -< (tl, topMid)

        returnA -< bg <> canton <> fedStar <> alphaStar <> betaStar <> gammaStar <> deltaStar <> epsilonStar

    generateLine pointArrow = proc (a, b) -> do
      p <- pointArrow -< (a, b)
      (_, q) <- perpendicular -< (p, b)
      returnA -< (p, q)

    drawInCircleAt radiusRatio fillOp = proc ((line1, line2), (orig, scaleUnit)) -> do
      center <- intersectLL -< (line1, line2)
      radius <- rationalMult radiusRatio -< (orig, scaleUnit)
      (_, edge) <- translate -< ((orig, radius), center)
      (_, edgeN) <- perpendicular -< (center, edge)
      fillOp -< (center, edgeN)
