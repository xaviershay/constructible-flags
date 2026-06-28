{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.MHL
  ( marshallIslands,
  )
where

import Control.Arrow (arr, returnA, (>>>))
import Data.Colour.SRGB (Colour, sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone
import Flag.Source

marshallIslands :: (Sourced :> es) => Flag es
marshallIslands =
  editorNote
    """
    The specified .392 dimension for right bar height does not match any actual
    reproduction of the flag – the bars would take up almost 80% of the edge!
    Željko Heimer (2004) proposes these were accidentally doubled i.e. the width for
    both bars was set of that of one. This seems plausible to me and I have used
    half the value (.196) in this construction, which also pleasingly matches the inner diameter
    of the star.

    No specification is given for placement of the bars relative to top and
    bottom edges. The width of the bar at the left side felt an aesthetically
    rational choice.

    While an Executive Order of the Cabinet is referenced for colour
    specifciations, I have not been able to locate one.
    """
    $ mkCountryFlag
      "MHL"
      "Marshall Islands"
      constructedAt
      ( reference
          "Description"
          flagSpec
          """
          The official flag of the Republic of the Marshall Islands shall be blue with two (2) contiguous trapezoidal bars extending from the lower left of the flag, diagonally across to the upper right of the flag. The top bar shall be orange, and the bottom bar shall be white. The union of the flag, in the upper left of the flag, shall be a white star of twentyfour (24) points; the two (2) points which are parallel to the fly shall be longer than the remaining twenty (20) points. Points are fifteen (15) degrees apart. The union is located equidistant from the left edge, the top edge and the top of the orange bar.

          The colors used in the design of the official flag of the Republic of the Marshall Islands shall be of a hue authorized by Executive Order of the Cabinet, based upon the color identification system of the United States Bureau of Standards.
          """
      )
      design
  where
    constructedAt = "2026-03-14"
    gov = mkAgentOrg "mhl_gov" "Government of Marshall Islands"

    fotw = mkEntity "Flags of the World" "https://www.crwflags.com/fotw/flags/mh.html"

    flagSpec =
      screenshot constructedAt "mhl/flag-act.png" $
        attributeTo gov $
          mkEntity
            "Official Flag of the Marshall Islands Act 1979"
            "https://rmiparliament.org/cms/images/LEGISLATION/PRINCIPAL/1979/1979-0001/1979-0001_1.pdf"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      blueC <- referencePantoneAsRGB flagSpec ("Blue", "287-C")
      orangeC <- referencePantoneAsRGB flagSpec ("Orange", "152-C")
      whiteC <- editorial "White" [] (sRGB24 255 255 255)
      _ <- reference "Hoist" flagSpec (1 :: Int)
      pFly <- reference "Fly" flagSpec (19 % 10)
      pOuterUnionDiameter <- reference "Outer Union Diameter" flagSpec (620 % 1000)
      pInnerUnionDiameter <- reference "Inner Union Diameter" flagSpec (444 % 1000)
      pInnerCircleDiameter <- reference "Inner Union Circle Diameter" flagSpec (196 % 1000)
      pLeftBarWidth <- reference "Bar Width, Left Edge" flagSpec (16 % 1000)
      pRightBarWidth <- editorial "Bar Width, Right Edge" [fotw] (196 % 1000)
      pure $ proc origin -> do
        let tl = fst origin
        tr <- rationalMult pFly >>> label "TR" -< origin
        (bl, _) <- perpendicular >>> labelFirst "BL" -< origin

        br <- quad >>> label "BR" -< (tl, tr, bl)

        leftC <- rationalMult pLeftBarWidth >>> label "LC" -< (bl, tl)
        (_, leftB) <- intersectLC >>> labelSecond "LB" -< ((bl, leftC), (leftC, bl))
        (_, leftA) <- intersectLC >>> labelSecond "LA" -< ((leftC, leftB), (leftB, leftC))

        rightA <- rationalMult pLeftBarWidth >>> label "RA" -< (tr, br)
        rightBarHeight <- rationalMult pRightBarWidth -< (tr, br)
        (_, rightB) <- translate -< ((tr, rightBarHeight), rightA) -- intersectLC >>> labelSecond "RB" -< ((tr, rightA), (rightA, tr))
        (_, rightC) <- intersectLC >>> labelSecond "RC" -< ((rightA, rightB), (rightB, rightA))

        bisect1 <- bisectAngle -< (tl, (tr, bl))
        a <- intersectLL -< ((tl, tr), (leftA, rightA))
        bisect2 <- bisectAngle -< (leftA, (tl, a))

        unionCenter <- intersectLL >>> label "UC" -< (bisect1, bisect2)

        unionUnit <- translate -< (origin, unionCenter)
        unionInner <- rationalMult (pInnerCircleDiameter / 2) >>> label "UI" -< unionUnit
        unionMid <- rationalMult (pInnerUnionDiameter / 2) >>> label "UM" -< unionUnit
        unionOuter <- rationalMult (pOuterUnionDiameter / 2) >>> label "UO" -< unionUnit

        bg <- fillRectangle blueC -< (tl, tr, br, bl)
        orangeStripe <- fillRectangle orangeC -< (leftA, rightA, rightB, leftB)
        whiteStripe <- fillRectangle whiteC -< (leftB, rightB, rightC, leftC)
        star <- fillStar24InnerC whiteC -< (unionCenter, unionInner, unionMid, unionOuter)
        returnA -< bg <> orangeStripe <> whiteStripe <> star
