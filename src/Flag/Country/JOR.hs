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
import Flag.Definition (Flag, mkCountryFlag, editorNote)
import Flag.Pantone

jordan :: Sourced :> es => Flag es
jordan = editorNote (
    "Many other constructions use a lighter red. The darker hue used here is" ++
    " the latest I can find specified by a government source ... and I also happen to like it better."
  )
  $ mkCountryFlag
  "JOR"
  "Jordan"
  ( reference "Description" constitution
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

    flagElements = screenshot "2026-02-19" "jor/flag-elements.png" $ attributeTo gov $ mkEntity
      "The Jordanian Flag Basic Elements"
      ""
      -- Issued 2007

    constitution = screenshot constructedAt "jor/constitution.png" $ attributeTo gov $ mkEntity
      "Jordan Constitution, Article 4"
      "https://www.constituteproject.org/constitution/Jordan_2016#s10"
      -- 2016

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blackC <- reference "Black" flagElements (cmyk 0 0 0 100)
        whiteC <- reference "White" flagElements (sRGB24 255 255 255)
        greenC <- reference "Green" flagElements (cmyk 100 0 91 27.5)
        redC <- reference "Red" flagElements (cmyk 0 100 65 15)

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
