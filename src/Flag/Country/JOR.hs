{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.JOR
  ( jordan,
  )
where

import Control.Arrow (returnA)
import Data.Colour.SRGB (sRGB24)
import Data.Ratio ((%))
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone
import Flag.Source

jordan :: (Sourced :> es) => Flag es
jordan =
  editorNote
    """
    The star dimensions are not specified. Other constructions are varied in their
    approach. Using an inner circle half the diameter of the outer one feels
    reasonable. A 7x3 pentagram feels too skinny; a 7x4 too fat.

    Many other constructions use a lighter red. The darker hue used here is the
    latest I can find specified by a government source ... and I also happen to
    like it better. Interestingly, using approximations for the Pantone
    specifications better match the colors in the document than converting from
    the given CMYK.
    """
    $ mkCountryFlag
      "JOR"
      "Jordan"
      constructedAt
      ( reference
          "Description"
          constitution
          ( "Its length shall be twice its width. "
              ++ "It shall be divided horizontally into three parallel equal stripes, "
              ++ "the uppermost of which shall be black; the center, white; and the "
              ++ "lowest, green. "
              ++ "At the end of the flag-staff, it shall have a red triangle, the base "
              ++ "of which shall be equal to its width, and its height shall be equal "
              ++ "to half of its length. "
              ++ "In this triangle there shall be a white seven-pointed star of such "
              ++ "an area that may absorbed in a circle the diameter of which shall "
              ++ "be one-fourteenth of its length; and shall be so placed that its "
              ++ "middle shall be at the intersection of the lines bisecting the "
              ++ "angles of the triangle, and the axis running through one of its "
              ++ "points shall be parallel to the base of the triangle."
          )
      )
      design
  where
    constructedAt = "2026-02-18"
    gov = mkAgentOrg "bgd_gov" "King of Jordan"

    flagElements =
      screenshot "2026-02-19" "jor/flag-elements.png" $
        attributeTo gov $
          mkEntity
            "The Jordanian Flag Basic Elements"
            ""
    -- Issued 2007

    constitution =
      screenshot constructedAt "jor/constitution.png" $
        attributeTo gov $
          mkEntity
            "Jordan Constitution, Article 4"
            "https://www.constituteproject.org/constitution/Jordan_2016#s10"
    -- 2016

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      greenC <- referencePantoneAsRGB flagElements ("Green", "356-C")
      redC <- referencePantoneAsRGB flagElements ("Red", "200-C")
      blackC <- reference "Black" flagElements (cmyk 0 0 0 100)
      whiteC <- reference "White" flagElements (sRGB24 255 255 255)

      proportions <- reference "Stripe Heights" constitution [1, 1, 1]
      _ <- reference "Triangle Dimenstions" constitution ()
      starRatio <- reference "Star Specification" constitution (1 % 14)

      let colors = [blackC, whiteC, greenC]
      pure $ proc (a, b) -> do
        bg <- horizontalStripes 6 (zip proportions colors) -< (a, b)
        (tl, tr, br, bl) <- boxNatural 6 3 -< (a, b)

        center <- intersectLL -< ((tl, br), (tr, bl))

        t <- fillTriangle redC -< (tl, center, bl)

        b1 <- bisectAngle -< (tl, (bl, center))
        b2 <- bisectAngle -< (center, (tl, bl))
        triangleCenter <- intersectLL -< (b1, b2)

        r <- rationalMult (starRatio / 2) -< (tl, tr)
        (_, r') <- perpendicular -< (tl, r)
        starEdge <- translate -< ((tl, r'), triangleCenter)

        star <- fillStar7Inner (1 % 2) whiteC -< starEdge

        returnA -< bg <> t <> star
