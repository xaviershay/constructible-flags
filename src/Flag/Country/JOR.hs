{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.JOR
    ( jordan
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag(..))
import Flag.Pantone

jordan :: Sourced :> es => Flag es
jordan = CountryFlag
  { flagIsoCode = "JOR"
  , flagName = "Jordan"
  , flagDescription = reference "Description" constitution
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
  , flagDesign = design
  }

  where
    constructedAt = "2026-02-18"
    gov = mkAgentOrg "bgd_gov" "King of Jordan"

    flagElements = attributeTo gov $ mkEntity
      "The Jordanian Flag Basic Elements"
      ""
      -- Issued 2007

    references =
      [ screenshot constructedAt "jor/fotw.png" $ mkEntity
          "Jordan (Flags of the World)"
          "https://www.crwflags.com/fotw/flags/jo.html"
      ]

    constitution = screenshot constructedAt "jor/constitution.png" $ attributeTo gov $ mkEntity
      "Jordan Constitution, Article 4"
      "https://www.constituteproject.org/constitution/Jordan_2016#s10"
      -- 2016

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        (greenPms, redPms) <- unsightedReference "Pantone Colors" flagElements references ("356-C", "200-C")
        blackC <- reference "Black" constitution (sRGB24 0 0 0)
        whiteC <- reference "White" constitution (sRGB24 255 255 255)
        greenC <- pantoneToRGB greenPms
        redC <- pantoneToRGB redPms
        proportions <- reference "Stripe Heights" constitution [1, 1, 1]
        _ <- reference "Triangle Dimenstions" constitution ()
        (sn, sd) <- reference "Star Specification" constitution (1, 14)

        let colors = [blackC, whiteC, greenC]
        pure $ proc (a, b) -> do
            bg <- horizontalStripes 6 (zip proportions colors) -< (a, b)
            (tl, tr, br, bl) <- boxNatural 6 3 -< (a, b)

            center <- intersectLL -< ((tl, br), (tr, bl))

            t <- fillTriangle redC -< (tl, center, bl)

            b1 <- bisectAngle -< (tl, (bl, center))
            b2 <- bisectAngle -< (center, (tl, bl))
            triangleCenter <- intersectLL -< (b1, b2)

            r <- rationalMult sn (sd * 2) -< (tl, tr)
            (_, r') <- perpendicular -< (tl, r)
            starEdge <- translate -< ((tl, r'), triangleCenter)

            star <- fillStar7x3 whiteC -< starEdge

            returnA -< bg <> t <> star
