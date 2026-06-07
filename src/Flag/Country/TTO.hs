{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.TTO
  ( trinidadAndTobago,
  )
where

import Control.Arrow (arr, returnA, (>>>))
import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Source

trinidadAndTobago :: (Sourced :> es) => Flag es
trinidadAndTobago =
  editorNote
    """
    While the design is clear from abundant government usage, the description appears to underspecify it: why would the midpoint of the bend not pass through the corners of the rectangle?

    Also the framing of "width [...] joined side-by-side at upper dexter corner" is confusing - the side-by-side width is the same no matter where you measure! Unless this was intended to anchor the bottom of the bend to the corner ... but in that case it could also anchor the top (for a more horizontal stripe), no?
    """
    $ mkCountryFlag
      "TTO"
      "Trinidad and Tobago"
      constructedAt
      ( reference
          "Description"
          flagSpec
          "On a Red Field, a Bend Dexter Sable bordered Silver, that is to say, there is on the Red Field a diagonal from left to right in Black bordered with White. The width of the Black and White bands joined side by side at the upper dexter corner of the Flag is one-fifth of the full length of the Flag, and the width of each White band is one-sixth of the width of the White and Black bands together."
      )
      design
  where
    constructedAt = "2026-06-07"
    gov = mkAgentOrg "tto_gov" "Government of Trinidad and Tobago"

    identitySpec =
      screenshot constructedAt "tto/tto-identity.png" $
        screenshot constructedAt "tto/tto-identity-2.png" $
          attributeTo gov $
            mkEntity
              "The National Identity Guidelines of Trinidad and Tobago"
              "https://natt.gov.tt/sites/default/files/pdfs/National%20Identity%20Guidelines_FINALReduced%20Size.pdf"

    flagSpec =
      screenshot constructedAt "tto/tto-spec.png" $
        attributeTo gov $
          mkEntity
            "The National Emblems of Trinidad and Tobago (Regulation) Act, Chapter 19:04 (1967)"
            "https://rgd.legalaffairs.gov.tt/laws2/alphabetical_list/lawspdfs/19.04.pdf"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      whiteC <- reference "White" identitySpec (sRGB24 255 255 255)
      redC <- reference "Red" identitySpec (sRGB24 217 30 54)
      blackC <- reference "Black" identitySpec (sRGB24 0 0 0)
      (w, h, barWidthRatio, stripeWidth) <- reference "Dimensions" identitySpec (5, 3, 1 % 5, 1 % 6)

      pure $ proc unit -> do
        (tl, tr, br, bl) <- boxNatural w h -< unit

        m <- midpoint -< (tl, br)

        r <- rationalMult ((w % 1) * barWidthRatio) -< unit

        p <- intersectCC >>> arr snd -< ((tl, r), (m, tl))

        t4 <- intersectLL >>> label "T4" -< ((tl, tr), (p, br))

        u <- translate >>> arr snd -< (unit, t4)

        (_, t4p) <- perpendicular -< (t4, br)

        e1 <- intersectLC >>> arr fst >>> label "E1" -< ((t4, t4p), (t4, u))

        b1 <- intersectLL >>> label "B1" -< ((tl, e1), (bl, br))

        e3 <- rationalMult stripeWidth >>> label "E3" -< (t4, e1)
        e2 <- rationalMult (1 - stripeWidth) >>> label "E2" -< (t4, e1)

        e3p <- perpendicular >>> arr fst -< (e3, t4)
        e2p <- perpendicular >>> arr fst -< (e2, t4)

        t3 <- intersectLL >>> label "T3" -< ((e3, e3p), (tl, tr))
        b3 <- intersectLL >>> label "B3" -< ((e3, e3p), (bl, br))

        t2 <- intersectLL >>> label "T2" -< ((e2, e2p), (tl, tr))
        b2 <- intersectLL >>> label "B2" -< ((e2, e2p), (bl, br))

        bg1 <- fillTriangle redC -< (tl, bl, b1)
        bg2 <- fillTriangle redC -< (t4, tr, br)

        stripeW1 <- fillRectangle whiteC -< (tl, b1, b2, t2)
        stripeW2 <- fillRectangle whiteC -< (t4, br, b3, t3)

        stripeB <- fillRectangle blackC -< (t2, t3, b3, b2)

        returnA -< bg1 <> bg2 <> stripeW1 <> stripeW2 <> stripeB
