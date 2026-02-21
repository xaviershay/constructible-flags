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
import Data.Ratio ((%))
import Flag.Source
import Flag.Pantone
import Flag.Definition (Flag, mkCountryFlag, editorNote)

bhutan :: Sourced :> es => Flag es
bhutan = editorNote "The dragon design used here is not the most common on the internet, but better matches designs used in official capacities recently (e.g. the Olympics)."
  $ mkCountryFlag
  "BTN"
  "Bhutan"
  ( reference "Description" flagRules
      ( "The yellow half holds the base from the top. The orange half extends from bottom and forms the fluttering end. "
      ++ "The dragon equally spreads over the borderline. The colour of dragon is white. Whatever be the length of a "
      ++ "larger or small national flag, two third of it must be kept as the width."
      )
  )
  design

  where
    constructedAt = "2026-02-21"
    gov = mkAgentOrg "bgd_gov" "Government of Bhutan"

    locEntity = mkAgentOrg "loc2012" "London Organising Committee of the Olympic Games and Paralympic Games Limited"
    tocEntity = mkAgentOrg "toc2020" "Tokyo Organising Committee of the Olympic and Paralympic Games"

    toc = screenshot constructedAt "btn/toc.png" $ attributeTo tocEntity $ mkEntity
      "Flagns and Anthems Manual, Tokyo 2020"
      "https://library.olympics.com/Default/doc/SYRACUSE/1568069/flags-and-anthems-manual-tokyo-2020-the-tokyo-organising-committee-of-the-olympic-and-paralympic-gam?_lg=en-GB"

    loc = screenshot constructedAt "btn/loc.png" $ attributeTo locEntity $ mkEntity
      "Flags and Anthems Manual, London 2012"
      "https://library.olympics.com/Default/doc/SYRACUSE/34593/flags-and-anthems-manual-london-2012-spp-final-version-london-organising-committee-of-the-olympic-ga?_lg=en-GB"

    flagRules = screenshot constructedAt "btn/flag-rules.png" $ attributeTo gov $ mkEntity
        "The National Flag Rules of Bhutan, 1972"
        "https://oag.gov.bt/wp-content/uploads/2011/02/National-Flag-Rules-1972-English.pdf"

    commons = mkEntity "Flag of Bhutan alternate (Wikimedia Commons)" "https://commons.wikimedia.org/wiki/File:Flag_of_Bhutan_alternate.svg"
    fotw = mkEntity "Bhutan: Specifications Discussions (Flags of the World)" "https://www.fotw.info/flags/bt'.html#var"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        (w, h) <- reference "Proportion" flagRules (3, 2)
        orangeC <- referencePantoneAsRGB loc ("Orange", "165-C")
        yellowC <- referencePantoneAsRGB loc ("Yellow", "116-C")
        dragonDesign <- reference "Dragon Design" commons "data/images/btn/dragon.svg"
        dragonScale <- editorial "Dragon" [toc, commons, fotw] (3 % 4)

        pure $ proc origin -> do
            (tl, tr, br, bl) <- boxNatural w h -< origin

            leftMid  <- midpoint -< (tl, bl)
            rightMid <- midpoint -< (tr, br)

            bg1 <- fillTriangle yellowC -< (tl, tr, bl)
            bg2 <- fillTriangle orangeC -< (bl, tr, br)

            center <- intersectLL -< ((tl, br), (tr, bl))

            scale <- rationalMult dragonScale -< (center, tr)
            dragon <- overlaySVG dragonDesign -< (center, scale)


            returnA -< bg1 <> bg2 <> dragon
