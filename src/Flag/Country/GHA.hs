{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultilineStrings #-}

module Flag.Country.GHA
    ( ghana
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag, editorNote)
import Flag.Pantone (pantoneToRGB)

ghana :: Sourced :> es => Flag es
ghana = editorNote "I cannot find an official specification. Proportions seem uncontroversial, and for colors I arbitrarily chose a source I thought looked the best." $ mkCountryFlag
  "GHA"
  "Ghana"
  constructedAt
  (reference "Description" flagSpec
    """
    The national flag of Ghana [...] consists of the Pan-African colours of red, gold, and green, in horizontal stripes, with a black five-pointed star in the centre of the gold stripe.
    """
  )
  design

  where
    constructedAt = "2026-09-25"
    mission = mkAgentOrg "gha_mission" "The Permanent Mission of Ghana to the United Nations"

    flagSpec = attributeTo mission $ mkEntity "Ghana Flag" "https://www.ghanamissionun.org/ghana-flag/"
    fotw = screenshot constructedAt "gha/fotw.png" $ mkEntity "Flags of the World, Ghana" "https://www.crwflags.com/fotw/flags/gha.html"
    album = mkEntity "Album des Pavillons, 2023" ""

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        redP <- unsightedReference "Red" album [fotw] "485-C"
        redColor <- pantoneToRGB "Red" redP
        yellowP <- unsightedReference "Yellow" album [fotw] "116-C"
        yellowColor <- pantoneToRGB "Yellow" yellowP
        greenP <- unsightedReference "Green" album [fotw] "3425-C"
        greenColor <- pantoneToRGB "Green" greenP
        blackC <- editorial "Black" [] (sRGB24 0 0 0)
        proportions <- editorial "Proportions" [flagSpec] [2, 2, 2]
        let colors = [redColor, yellowColor, greenColor]
        pure $ proc (a, b) -> do
            bg <- horizontalStripes 9 (zip proportions colors) -< (a, b)
            (tl, tr, _, _) <- boxNatural 9 6 -< (a, b)
            (down, _) <- perpendicular -< (tl, b)

            topMid <- midpoint -< (tl, tr)
            yellowTop <- naturalMult 2 -< (tl, down)
            yellowBottom <- naturalMult 4 -< (tl, down)
            (_, apex) <- translate -< ((tl, yellowTop), topMid)
            (_, along) <- translate -< ((a, b), apex)

            star <- fillStar5BetweenLines blackC -< (apex, along, yellowBottom)

            returnA -< bg <> star
