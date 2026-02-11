{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.Bangladesh
    ( bangladesh
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Pantone
import Flag.Definition (Flag(..))

bangladesh :: Sourced :> es => Flag es
bangladesh = CountryFlag
  { flagIsoCode = "BGD"
  , flagName = "Bangladesh"
  , flagDescription = sourced "Description" flagLaw
      ( "The ‘National Flag’ will be in bottle green and rectangular in size in the "
     ++ "proportion of length to width 10: 6 bearing a red circle on the body of the green. "
     ++ "The red circle will have a radius of one-fifth of the length of the flag. Its center will be "
     ++ "placed on the intersecting point of the perpendicular drawn from the nine-twentieth "
     ++ "part of the length of the flag and the horizontal line drawn through the middle of its "
     ++ "width." )
  , flagDesign = design
  }

  where
    flagLaw :: Source
    flagLaw = SourceLaw
        "Bangladesh Flag Rules, 1972 (revised 2005)"
        "https://web.archive.org/web/20180619074851/http://lib.pmo.gov.bd/legalms/pdf/national-flag-rules.pdf#page=2"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        (w, h) <- sourced "Proportion" flagLaw (10, 6)

        _ <- sourced "Procion Colors" flagLaw ("Procion Brilliant Green H-2RS 50/1000", "Procion Brilliant Orange H-2RS 60/1000")
        discRadius <- sourced "Disc width" flagLaw (1, 5)

        (greenPms, redPms) <- sourced "Pantone Colors" SourceHabitual (PMS342C, PMS485C)

        -- Third: convert Pantone to RGB using the Pantone module
        greenColor <- pmsToRGB greenPms
        redColor   <- pmsToRGB redPms

        pure $ proc origin -> do
            (tl, tr, br, bl) <- boxNatural w h -< origin

            leftMid  <- midpoint -< (tl, bl)
            rightMid <- midpoint -< (tr, br)

            -- point on top edge at 9/20 of the length from the hoist
            topNineTwentieth <- rationalMult 9 20 -< (tl, tr)

            (p, p') <- perpendicular -< (topNineTwentieth, tr)

            center <- intersectLL -< ((p, p'), (leftMid, rightMid))

            -- edge point for circle chosen so that distance from center is 1/5 of full length
            edge <- rationalMult 13 20 -< (leftMid, rightMid)

            bg   <- fillRectangle greenColor -< (tl, tr, br, bl)
            disc <- fillCircle redColor -< (center, edge)

            returnA -< bg <> disc
