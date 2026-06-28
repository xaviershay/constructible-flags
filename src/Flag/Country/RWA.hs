{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultilineStrings #-}

module Flag.Country.RWA
    ( rwanda
    ) where

import Data.Ratio
import Control.Arrow (returnA, (>>>), arr)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone (referencePantoneAsRGB)
import Flag.RAL (referenceRALAsRGB)

rwanda :: Sourced :> es => Flag es
rwanda = editorNote "No width for blue sun rim is specified, chose arbitrarily. Law says star diameter is 0.125 but this is a mistake: calculating from the examples gives 0.215 which is used here." $ mkCountryFlag
  "RWA"
  "Rwanda"
  constructedAt
  (reference "Description" flagSpec
    """
    The national flag shall be made up of three (3) colours: green, yellow and blue. The flag shall comprise of the following colours from the bottom to the top: a green strip, followed by a yellow strip both of which cover half the flag.

    The upper half is blue and bears on its right hand side the image of the sun with its rays of golden yellow. The sun and its rays are separated by a blue ring.
    """
  )
  design

  where
    constructedAt = "2026-06-28"
    gov = mkAgentOrg "rwa_gov" "Rwandan Government"

    flagSpec = screenshot constructedAt "rwa/description.png" $ screenshot constructedAt "rwa/colors.png" $ screenshot constructedAt "rwa/construction-sheet.png" $ attributeTo gov $ mkEntity
        "Law N° 34/2008"
        "https://www.rlrc.gov.rw/index.php?eID=dumpFile&t=f&f=74762&token=cb41274d1a4b2d1626672542e40024a521270e26"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blueC <- referencePantoneAsRGB flagSpec ("Blue", "299-C")
        greenC <- referenceRALAsRGB flagSpec ("Green", 6029)
        yellowC <- referenceRALAsRGB flagSpec ("Yellow", 1023)
        goldC <- referenceRALAsRGB flagSpec ("Gold", 1003)
        pure $ proc unit -> do
            (tl, tr, br, bl) <- boxNatural 3 2 -< unit
            l1 <- midpoint -< (tl, bl)
            l2 <- midpoint -< (l1, bl)
            r1 <- midpoint -< (tr, br)
            r2 <- midpoint -< (r1, br)

            starDiameter <- rationalMult (215 % 1000) >>> label "starD" -< (tl, tr)
            starRadius <- rationalMult (1 % 2) >>> label "starR" -< (tl, starDiameter)
            starRadiusV <- rotate90CW -< (tl, starRadius)
            topMargin <- rationalMult (1 % 10) >>> label "sunD" -< (tl, bl)
            sunDiameter <- rotate90CCW -< (tl, topMargin)
            sunRadius <- rationalMult (1 % 2) >>> label "sunR" -< sunDiameter
            rightMargin <- rationalMult (1 % 3) -< (tl, starDiameter)

            b <- translate >>> arr snd >>> label "B" -< ((rightMargin, tl), tr)
            sx <- translate >>> arr snd >>> label "SX" -< ((starRadius, tl), b)
            a <- translate >>> arr snd >>> label "A" -< ((tl, topMargin), tr)
            sy <- translate >>> arr snd >>> label "SY" -< (starRadiusV, a)

            sunO <- quad >>> label "O" -< (tr, sx, sy)
            sunR <- translate >>> arr snd -< ((tl, sunRadius), sunO)
            sunRInner <- rationalMult (9 % 10) -< (sunO, sunR)
            starR <- translate >>> arr snd -< ((tl, starRadius), sunO)

            sun <- fillStar24InnerC goldC -< (sunO, sunR, starR, starR)
            sunOuterCircle <- fillCircle blueC -< (sunO, sunR)
            sunInnerCircle <- fillCircle blueC -< (sunO, sunRInner)
            sunRing <- maskDrawing -< (sunOuterCircle, sunInnerCircle)

            sky <- fillRectangle blueC -< (tl, l1, r1, tr)
            land <- fillRectangle yellowC -< (l1, l2, r2, r1)
            grass <- fillRectangle greenC -< (l2, bl, br, r2)

            returnA -< sky <> land <> grass <> sun <> sunRing
