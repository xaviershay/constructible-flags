{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.FRA
    ( france
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag)

france :: Sourced :> es => Flag es
france = mkCountryFlag
  "FRA"
  "France"
  (reference "Description" constitution "A tricolour flag, blue, white, red.")
  design

  where
    constructedAt = "2026-02-14"

    gov = mkAgentOrg "fra_gov" "French Government"

    designGuidelines = screenshot constructedAt "fra/design-guidelines.png" $ attributeTo gov $ mkEntity
       "Official French Government Color Guidelines"
       "https://www.info.gouv.fr/marque-de-letat/les-couleurs#les-couleurs-principales"

    constitution = translated constructedAt $ screenshot constructedAt "fra/constitution.png" $ attributeTo gov $ mkEntity
        "French Constitution, Article 2"
        "https://www.legifrance.gouv.fr/loda/article_lc/LEGIARTI000006527453"

    references =
        [ mkEntity
            "Flag of France (Wikipedia)"
            "https://en.wikipedia.org/wiki/Flag_of_France"
        , mkEntity
            "France (Flags of the World)"
            "https://www.crwflags.com/fotw/flags/fr.html"
        ]

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blueColor  <- reference "Blue"  designGuidelines (sRGB24 0 0 145)
        whiteColor <- reference "White" designGuidelines (sRGB24 255 255 255)
        redColor   <- reference "Red"   designGuidelines (sRGB24 255 0 14)
        _ <- editorial "2:3 proportion" references ()
        pure $ proc (a, b) -> do
            c <- naturalMult 2 -< (a, b)
            d <- naturalMult 2 -< (b, c)

            d1 <- fillBox blueColor  1 2 -< (a, b)
            d2 <- fillBox whiteColor 1 2 -< (b, c)
            d3 <- fillBox redColor   1 2 -< (c, d)

            returnA -< d1 <> d2 <> d3
