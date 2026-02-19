{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.BGD
    ( bangladesh
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Pantone
import Flag.Definition (Flag, mkCountryFlag)

bangladesh :: Sourced :> es => Flag es
bangladesh = mkCountryFlag
  "BGD"
  "Bangladesh"
  ( reference "Description" flagRules
      ( "The ‘National Flag’ will be in bottle green and rectangular in size in the "
     ++ "proportion of length to width 10: 6 bearing a red circle on the body of the green. "
     ++ "The red circle will have a radius of one-fifth of the length of the flag. Its center will be "
     ++ "placed on the intersecting point of the perpendicular drawn from the nine-twentieth "
     ++ "part of the length of the flag and the horizontal line drawn through the middle of its "
     ++ "width." ) )
  design

  where
    constructedAt = "2026-02-13"
    gov = mkAgentOrg "bgd_gov" "Bangladeshi Government"

    flagRules = screenshot constructedAt "bgd/flag-rules.png" $ attributeTo gov $ mkEntity
        "Bangladesh Flag Rules, 1972 (revised 2005)"
        "https://web.archive.org/web/20180619074851/http://lib.pmo.gov.bd/legalms/pdf/national-flag-rules.pdf#page=2"

    references = [] -- TODO

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        (w, h) <- reference "Proportion" flagRules (10, 6)
        (pr, qr) <- reference "Disc/Length Ratio" flagRules (1, 5)
        _ <- reference "Procion Colors" flagRules ("Procion Brilliant Green H-2RS 50/1000", "Procion Brilliant Orange H-2RS 60/1000")
        (greenPms, redPms) <- editorial "Pantone Colors" references ("342-C", "485-C")

        -- Third: convert Pantone to RGB using the Pantone module
        greenColor <- pantoneToRGB greenPms
        redColor   <- pantoneToRGB redPms


        pure $ proc origin -> do
            (tl, tr, br, bl) <- boxNatural w h -< origin

            leftMid  <- midpoint -< (tl, bl)
            rightMid <- midpoint -< (tr, br)

            topNineTwentieth <- rationalMult 9 20 -< (tl, tr)

            p <- perpendicular -< (topNineTwentieth, tr)
            discCenter <- intersectLL -< (p, (leftMid, rightMid))
            radiusPoint <- rationalMult pr qr -< (leftMid, rightMid)
            (_, edgePoint) <- translate -< ((leftMid, radiusPoint), discCenter)

            bg   <- fillRectangle greenColor -< (tl, tr, br, bl)
            disc <- fillCircle redColor -< (discCenter, edgePoint)

            returnA -< bg <> disc
