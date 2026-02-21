{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.JPN
    ( japan
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Data.Ratio ((%))
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag)

japan :: Sourced :> es => Flag es
japan = mkCountryFlag
  "JPN"
  "Japan"
  (reference "Description" flagLaw "A white rectangular flag with a crimson disc at the center.")
  design

  where
    constructedAt = "2026-02-13"
    gov = mkAgentOrg "jpn_gov" "Japanese Government"

    flagLaw = screenshot constructedAt "jpn/act.png" $ attributeTo gov $ mkEntity
                "Act on National Flag and Anthem, Act #127, 1999"
                "https://elaws.e-gov.go.jp/document?lawid=411AC0000000127"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        (h, w) <- reference "2:3 proportion" flagLaw (2, 3)
        discRatio <- reference "Disc height" flagLaw (3 % 5)
        whiteColor <- impliedReference "White" flagLaw (sRGB24 255 255 255)
        redColor   <- impliedReference "Crimson" flagLaw (sRGB24 188 0 45)
        pure $ proc origin -> do
            (tl, tr, br, bl) <- boxNatural w h -< origin

            center <- intersectLL -< ((tl, br), (tr, bl))
            top <- midpoint -< (tl, tr)
            edge <- rationalMult discRatio -< (center, top)

            bg <- fillRectangle whiteColor -< (tl, tr, br, bl)
            disc <- fillCircle redColor -< (center, edge)

            returnA -< bg <> disc
