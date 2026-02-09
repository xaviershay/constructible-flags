{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.France
    ( france
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag(..))

france :: Sourced :> es => Flag es
france = CountryFlag
  { flagIsoCode = "FRA"
  , flagName = "France"
  , flagDescription = sourced "Description" constitution "A tricolour flag, blue, white, red."
  , flagDesign = design
  }

  where
    govWebsite :: Source
    govWebsite = SourceAuthoritativeWebsite
        "Official French Government Color Guidelines"
        "https://www.info.gouv.fr/marque-de-letat/les-couleurs#les-couleurs-principales"

    constitution :: Source
    constitution = SourceLaw
        "French Constitution, Article 2"
        "https://www.legifrance.gouv.fr/loda/article_lc/LEGIARTI000006527453"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blueColor  <- sourced "Blue"  govWebsite (sRGB24 0 0 145)
        whiteColor <- sourced "White" govWebsite (sRGB24 255 255 255)
        redColor   <- sourced "Red"   govWebsite (sRGB24 255 0 14)
        _ <- sourced "2:3 proportion" SourceHabitual ()
        pure $ proc (a, b) -> do
            c <- naturalMult 2 -< (a, b)
            d <- naturalMult 2 -< (b, c)

            d1 <- fillBox blueColor  1 2 -< (a, b)
            d2 <- fillBox whiteColor 1 2 -< (b, c)
            d3 <- fillBox redColor   1 2 -< (c, d)

            returnA -< d1 <> d2 <> d3
