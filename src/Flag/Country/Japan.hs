{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.Japan
    ( japan
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag(..))

japan :: Sourced :> es => Flag es
japan = CountryFlag
  { flagIsoCode = "JPN"
  , flagName = "Japan"
  , flagDescription = sourced "Description" flagLaw "A white rectangular flag with a crimson disc at the center."
  , flagDesign = design
  }

  where
    flagLaw :: Source
    flagLaw = SourceLaw
        "Act on National Flag and Anthem (Law #127 of 1999)"
        "https://elaws.e-gov.go.jp/document?lawid=411AC0000000127"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        whiteColor <- sourced "White" flagLaw (sRGB24 255 2 255)
        redColor   <- sourced "Crimson" flagLaw (sRGB24 188 0 45)
        (h, w) <- sourced "2:3 proportion" flagLaw (2, 3)
        (n, d) <- sourced "Disc diameter 3/5 of height" flagLaw (3, 5)
        pure $ proc (a, b) -> do
            (tl, tr, br, bl) <- boxNatural w h -< (a, b)

            -- White background
            bg <- fillRectangle whiteColor -< (tl, tr, br, bl)

            ---- Center: intersection of diagonals
            center <- intersectLL -< ((tl, br), (tr, bl))

            edge <- rationalMult n d -< (center, tl)

            disc <- fillCircle redColor -< (center, edge)

            returnA -< bg <> disc
