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
        star <- fillStar whiteC -< (unionCenter, unionInner, unionMid, unionOuter)
        returnA -< bg <> orangeStripe <> whiteStripe <> star

fillStar :: Colour Double -> FlagA (Point, Point, Point, Point) Drawing
fillStar col = group "Fill Star 24" $ proc (o, inner, mid, outer) -> do
  (o12, o0) <- intersectLC -< ((o, outer), (o, outer))
  (o6, o18) <- perpendicular -< (o, o0)

  ( m0,
    m1,
    m2,
    m3,
    m4,
    m5,
    _m6,
    m7,
    m8,
    m9,
    m10,
    m11,
    _m12,
    m13,
    m14,
    m15,
    m16,
    m17,
    _m18,
    m19,
    m20,
    m21,
    m22,
    m23
    ) <-
    fan -< (o, mid)
  innerAngle <- bisectAngle -< (o, (m0, m1))
  innerEdge <- intersectLC >>> arr snd -< (innerAngle, (o, inner))
  ( i0,
    i1,
    i2,
    i3,
    i4,
    i5,
    i6,
    i7,
    i8,
    i9,
    i10,
    i11,
    i12,
    i13,
    i14,
    i15,
    i16,
    i17,
    i18,
    i19,
    i20,
    i21,
    i22,
    i23
    ) <-
    fan -< (o, innerEdge)
  s1 <- fillTriangle col -< (i0, m1, i1)
  s2 <- fillTriangle col -< (i1, m2, i2)
  s3 <- fillTriangle col -< (i2, m3, i3)
  s4 <- fillTriangle col -< (i3, m4, i4)
  s5 <- fillTriangle col -< (i4, m5, i5)
  s6 <- fillTriangle col -< (i5, o6, i6)
  s7 <- fillTriangle col -< (i6, m7, i7)
  s8 <- fillTriangle col -< (i7, m8, i8)
  s9 <- fillTriangle col -< (i8, m9, i9)
  s10 <- fillTriangle col -< (i9, m10, i10)
  s11 <- fillTriangle col -< (i10, m11, i11)
  s12 <- fillTriangle col -< (i11, o12, i12)
  s13 <- fillTriangle col -< (i12, m13, i13)
  s14 <- fillTriangle col -< (i13, m14, i14)
  s15 <- fillTriangle col -< (i14, m15, i15)
  s16 <- fillTriangle col -< (i15, m16, i16)
  s17 <- fillTriangle col -< (i16, m17, i17)
  s18 <- fillTriangle col -< (i17, o18, i18)
  s19 <- fillTriangle col -< (i18, m19, i19)
  s20 <- fillTriangle col -< (i19, m20, i20)
  s21 <- fillTriangle col -< (i20, m21, i21)
  s22 <- fillTriangle col -< (i21, m22, i22)
  s23 <- fillTriangle col -< (i22, m23, i23)
  s24 <- fillTriangle col -< (i23, o0, i0)
  let s = s1 <> s2 <> s3 <> s4 <> s5 <> s6 <> s7 <> s8 <> s9 <> s10 <> s11 <> s12 <> s13 <> s14 <> s15 <> s16 <> s17 <> s18 <> s19 <> s20 <> s21 <> s22 <> s23 <> s24

  f1 <- fillTriangle col -< (o, i0, i1)
  f2 <- fillTriangle col -< (o, i1, i2)
  f3 <- fillTriangle col -< (o, i2, i3)
  f4 <- fillTriangle col -< (o, i3, i4)
  f5 <- fillTriangle col -< (o, i4, i5)
  f6 <- fillTriangle col -< (o, i5, i6)
  f7 <- fillTriangle col -< (o, i6, i7)
  f8 <- fillTriangle col -< (o, i7, i8)
  f9 <- fillTriangle col -< (o, i8, i9)
  f10 <- fillTriangle col -< (o, i9, i10)
  f11 <- fillTriangle col -< (o, i10, i11)
  f12 <- fillTriangle col -< (o, i11, i12)
  f13 <- fillTriangle col -< (o, i12, i13)
  f14 <- fillTriangle col -< (o, i13, i14)
  f15 <- fillTriangle col -< (o, i14, i15)
  f16 <- fillTriangle col -< (o, i15, i16)
  f17 <- fillTriangle col -< (o, i16, i17)
  f18 <- fillTriangle col -< (o, i17, i18)
  f19 <- fillTriangle col -< (o, i18, i19)
  f20 <- fillTriangle col -< (o, i19, i20)
  f21 <- fillTriangle col -< (o, i20, i21)
  f22 <- fillTriangle col -< (o, i21, i22)
  f23 <- fillTriangle col -< (o, i22, i23)
  f24 <- fillTriangle col -< (o, i23, i0)
  let f = f1 <> f2 <> f3 <> f4 <> f5 <> f6 <> f7 <> f8 <> f9 <> f10 <> f11 <> f12 <> f13 <> f14 <> f15 <> f16 <> f17 <> f18 <> f19 <> f20 <> f21 <> f22 <> f23 <> f24

  returnA -< s <> f
  where
    fan = proc (o, ov0) -> do
      (ov4, ov20) <- intersectCC >>> labelPair "4" "20" -< ((o, ov0), (ov0, o))

      (ov14, ov2) <- bisectArc >>> labelPair "14" "2" -< (o, ov0, ov4)
      (ov10, ov22) <- bisectArc >>> labelPair "10" "22" -< (o, ov0, ov20)
      (ov11, ov23) <- bisectArc >>> labelPair "11" "23" -< (o, ov0, ov22)
      (ov9, ov21) <- bisectArc >>> labelPair "9" "21" -< (o, ov20, ov22)
      (ov13, ov1) <- bisectArc >>> labelPair "13" "1" -< (o, ov0, ov2)
      (ov15, ov3) <- bisectArc >>> labelPair "15" "3" -< (o, ov2, ov4)
      (ov18, ov6) <- bisectArc >>> labelPair "18" "6" -< (o, ov3, ov9)
      (ov17, ov5) <- bisectArc >>> labelPair "17" "5" -< (o, ov4, ov6)
      (ov7, ov19) <- bisectArc >>> labelPair "7" "19" -< (o, ov18, ov20)

      ov8 <- intersectLC >>> arr fst >>> label "8" -< ((o, ov20), (o, ov20))
      ov12 <- intersectLC >>> arr fst >>> label "12" -< ((o, ov0), (o, ov0))
      ov16 <- intersectLC >>> arr fst >>> label "16" -< ((o, ov4), (o, ov4))

      returnA
        -<
          ( ov0,
            ov1,
            ov2,
            ov3,
            ov4,
            ov5,
            ov6,
            ov7,
            ov8,
            ov9,
            ov10,
            ov11,
            ov12,
            ov13,
            ov14,
            ov15,
            ov16,
            ov17,
            ov18,
            ov19,
            ov20,
            ov21,
            ov22,
            ov23
          )
