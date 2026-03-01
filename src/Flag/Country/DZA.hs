{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.DZA
  ( algeria,
  )
where

import Control.Arrow (returnA)
import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone
import Flag.Source

algeria :: (Sourced :> es) => Flag es
algeria =
  editorNote
    """
    I could not find an original scan of the relevant law online, and so rely on
    an archived page from the Permanent Mission of Algeria to the UN, courtesy
    of Flags of the World.

    Despite pouring over Rood's work, I could not figure how to interpret
    the color instructions provided. The Pantone colors here are used
    across most supplementary sources. (Interestingly, the one source
    I could reference directly – the London Olympic manual – has placed the star
    in the wrong place!)
    """
    $ mkCountryFlag
      "DZA"
      "Algeria"
      constructedAt
      ( reference
          "Description"
          flagLaw
          """
          The flag of the Democratic and Popular Republic of Algeria is constituted by a green and white rectangle embossed by a red star and a red crescent.
          """
      )
      design
  where
    constructedAt = "2026-02-28"
    gov = mkAgentOrg "dza_gov" "Government of Algeria"

    -- 1963
    flagLaw =
      screenshot constructedAt "dza/law.png" $
        attributeTo gov $
          mkEntity
            "Appendix of the law #63-145"
            "https://web.archive.org/web/20120205045854/http://www.algeria-un.org/default.asp?doc=-flag"

    locEntity = mkAgentOrg "loc2012" "London Organising Committee of the Olympic Games and Paralympic Games Limited"

    -- 2012
    loc =
      screenshot constructedAt "dza/loc.png" $
        attributeTo locEntity $
          mkEntity
            "Flags and Anthems Manual, London 2012"
            "https://library.olympics.com/Default/doc/SYRACUSE/34593/flags-and-anthems-manual-london-2012-spp-final-version-london-organising-committee-of-the-olympic-ga?_lg=en-GB"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      -- TODO: source dimensions from flagSpec
      whiteC <- editorial "White" [] (sRGB24 255 255 255)
      greenC <- referencePantoneAsRGB loc ("Green", "356-C")
      redC <- referencePantoneAsRGB loc ("Red", "186-C")

      pure $ proc (o, unit) -> do
        (tl, tr, br, bl) <- boxNatural 3 2 -< (o, unit)

        outerCrescentRadius <- rationalMult (1 % 4) -< (tl, bl)
        innerCrescentRadius <- rationalMult (1 % 5) -< (tl, bl)

        -- Find a vector we can translate to center to find midpoint of star
        starRadiusDown <- rationalMult (1 % 8) -< (tl, bl)
        (_, starRadius) <- perpendicular -< (tl, starRadiusDown)

        (q, _) <- perpendicular -< (o, starRadius)

        mq <- midpoint -< (o, q)
        (_, n2) <- intersectLC -< ((o, q), (mq, starRadius))
        (f2, _) <- intersectLC -< ((o, starRadius), (o, n2))
        g2 <- midpoint -< (o, f2)

        topMid <- midpoint -< (tl, tr)
        bottomMid <- midpoint -< (bl, br)
        center <- midpoint -< (topMid, bottomMid)

        (_, starCenter) <- translate -< ((g2, o), center)
        (_, starEdge) <- translate -< ((o, starRadius), starCenter)
        (_, outerCrescentEdge) <- translate -< ((tl, outerCrescentRadius), center)

        let outerCrescentCenter = center

        (_, p) <- intersectCC -< ((outerCrescentCenter, outerCrescentEdge), (outerCrescentEdge, outerCrescentCenter))
        (_, q) <- intersectCC -< ((outerCrescentCenter, p), (p, outerCrescentCenter))

        (_, pEdge) <- translate -< ((o, innerCrescentRadius), p)
        (_, qEdge) <- translate -< ((o, innerCrescentRadius), q)

        (_, innerCrescentCenter) <- intersectCC -< ((p, pEdge), (q, qEdge))
        (_, innerCrescentEdge) <- translate -< ((tl, innerCrescentRadius), innerCrescentCenter)

        bgL <- fillRectangle greenC -< (tl, topMid, bottomMid, bl)
        bgR <- fillRectangle whiteC -< (topMid, bottomMid, br, tr)
        star <- fillStar5 redC -< (starCenter, starEdge)
        -- outerCrescent <- fillCircle redC -< (outerCrescentCenter, outerCrescentEdge)
        -- innerCrescent <- fillCircle whiteC -< (innerCrescentCenter, innerCrescentEdge)
        crescent <- fillCrescent redC -< ((outerCrescentCenter, outerCrescentEdge), (innerCrescentCenter, innerCrescentEdge))
        returnA -< bgL <> bgR <> star <> crescent
