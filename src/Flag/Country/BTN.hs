{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.BTN
    ( bhutan
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Pantone
import Flag.Definition (Flag, mkCountryFlag)

bhutan :: Sourced :> es => Flag es
bhutan = mkCountryFlag
  "BTN"
  "Bhutan"
  ( reference "Description" flagRules
      ( "TODO"
      )
  )
  design

  where
    constructedAt = "2026-02-21"
    gov = mkAgentOrg "bgd_gov" "Government of Bhutan"

    locEntity = mkAgentOrg "loc2012" "London Organising Committee of the Olympic Games and Paralympic Games Limited"

    loc = screenshot constructedAt "btn/loc.png" $ attributeTo locEntity $ mkEntity
      "Flags and Anthems Manual, London 2012"
      "https://library.olympics.com/Default/doc/SYRACUSE/34593/flags-and-anthems-manual-london-2012-spp-final-version-london-organising-committee-of-the-olympic-ga?_lg=en-GB"

    flagRules = screenshot constructedAt "btn/flag-rules.png" $ attributeTo gov $ mkEntity
        "The National Flag Rules of Bhutan, 1972"
        "https://oag.gov.bt/wp-content/uploads/2011/02/National-Flag-Rules-1972-English.pdf"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        (w, h) <- reference "Proportion" flagRules (3, 2)
        orangeC <- referencePantoneAsRGB loc ("Orange", "165-C")
        yellowC <- referencePantoneAsRGB loc ("Yellow", "116-C")


        pure $ proc origin -> do
            (tl, tr, br, bl) <- boxNatural w h -< origin

            leftMid  <- midpoint -< (tl, bl)
            rightMid <- midpoint -< (tr, br)

            bg1 <- fillTriangle yellowC -< (tl, tr, bl)
            bg2 <- fillTriangle orangeC -< (bl, tr, br)

            center <- intersectLL -< ((tl, br), (tr, bl))

            scale <- rationalMult 2 3 -< (center, tr)
            dragon <- overlaySVG "data/images/btn/dragon.svg" -< (center, scale)


            returnA -< bg1 <> bg2 <> dragon
