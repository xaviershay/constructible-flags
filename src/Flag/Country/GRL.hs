{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.GRL
  ( greenland,
  )
where

import Control.Arrow (arr, returnA, (>>>))
import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone (referencePantoneAsRGB)
import Flag.Source
import Foreign (addForeignPtrFinalizerEnv)

greenland :: (Sourced :> es) => Flag es
greenland =
  editorNote
    """
    Greenland 1985 law says the flag is determined by (Danish) Royal Resolution, however I cannot locate any such resolution.

    Organization Erfalasorput, a national organisation dedicated to the flag, provides a confusing an inconsistent specification (e.g. two different red color values). The construction sheet however is clear, if with aethestically displeasing rations.

    Flags of the World rely on an unsighted 1995 brochure containing a description from the flag's designer. I have opted to use the latter as it is more readily traced back to the original designer and maintains a more standard aspect ratio.
    """
    $ mkCountryFlag
      "GRL"
      "Greenland"
      constructedAt
      ( reference
          "Description"
          fotw
          """
          The flag is 12 parts by 18, the white and red stripe are both 6 parts. The centre of the circle is set 7 parts from the hoist along the dividing line between the white and red, the radius being 4 parts. The upper part of the circle is red, the lower white.
          """
      )
      design
  where
    constructedAt = "2026-03-14"

    fotw = screenshot constructedAt "grl/fotw.png" $ mkEntity "Flags of the World, Greenland" "https://www.crwflags.com/fotw/flags/gl.html"
    stampBrochure = mkEntity "Greenland Post Office Brochure, 1995" ""
    erfalasorput = screenshot constructedAt "grl/erfalasorput.png" $ mkEntity "Erfalasorput: Appearance and Size" "https://erfalasorput.gl/da/isikkua-angissusaalu/"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      whiteC <- editorial "White" [] (sRGB24 255 255 255)
      redC <- referencePantoneAsRGB erfalasorput ("Red", "186-C")
      (pWidth, pHeight, pDiscCenter, pDiscRadius) <- unsightedReference "Proportions" stampBrochure [fotw] (18, 12, 7 % 18, 4 % 18)

      pure $ proc origin -> do
        (tl, tr, br, bl) <- boxNatural pWidth pHeight -< origin
        leftHalf <- midpoint -< (tl, bl)
        rightHalf <- midpoint -< (tr, br)
        discCenter <- rationalMult pDiscCenter -< (leftHalf, rightHalf)
        discRadius <- rationalMult pDiscRadius -< (leftHalf, rightHalf)
        (_, discEdge) <- translate -< ((leftHalf, discRadius), discCenter)

        topR <- fillRectangle whiteC -< (tl, tr, rightHalf, leftHalf)
        bottomR <- fillRectangle redC -< (bl, br, rightHalf, leftHalf)

        topCircle <- fillCircle redC -< (discCenter, discEdge)
        bottomCircle <- fillCircle whiteC -< (discCenter, discEdge)
        topHalf <- clipDrawing -< (topCircle, topR)
        bottomHalf <- clipDrawing -< (bottomCircle, bottomR)
        returnA -< topR <> bottomR <> topHalf <> bottomHalf
