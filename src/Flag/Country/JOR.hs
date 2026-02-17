{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.JOR
    ( jordan
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag(..))

jordan :: Sourced :> es => Flag es
jordan = CountryFlag
  { flagIsoCode = "JOR"
  , flagName = "Jordan"
  , flagDescription = pure "TODO" -- reference "Description" constitution "TODO"
  , flagDesign = design
  }

  where
    _constructedAt = "2026-02-14"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blackC <- editorial "Black" [] (sRGB24 0 0 0)
        whiteC <- editorial "White" [] (sRGB24 255 255 255)
        greenC <- editorial "Green" [] (sRGB24 0 255 0)
        redC <- editorial "Red" [] (sRGB24 255 0 0)
        proportions <- editorial "Stripe Heights" [] [1, 1, 1]

        let colors = [blackC, whiteC, greenC]
        pure $ proc (a, b) -> do
            bg <- horizontalStripes 6 (zip proportions colors) -< (a, b)
            (tl, tr, br, bl) <- boxNatural 6 3 -< (a, b)

            center <- intersectLL -< ((tl, br), (tr, bl))

            t <- fillTriangle redC -< (tl, center, bl)

            b1 <- bisectAngle -< (tl, (bl, center))
            b2 <- bisectAngle -< (center, (tl, bl))
            triangleCenter <- intersectLL -< (b1, b2)

            r <- rationalMult 1 14 -< (tl, tr)
            (r', _) <- perpendicular -< (tl, r)
            starEdge <- translate -< ((tl, r'), triangleCenter)

            star <- fillStar7x3 whiteC -< starEdge

            returnA -< bg <> t <> star
