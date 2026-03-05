{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Country.NPL
  ( nepal,
  )
where

import Control.Arrow (returnA, second, (>>>))
import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Effectful
import Flag.Construction.Types (Drawing, FlagA, Point)
import Flag.Constructions
import Flag.Definition (Flag, editorNote, mkCountryFlag)
import Flag.Pantone (referencePantoneAsRGB)
import Flag.SharedSources
import Flag.Source

nepal :: (Sourced :> es) => Flag es
nepal =
  editorNote
    """
    The constitution is exact on construction, except for some slight ambiguity on
    the moon star: eight rays are fully visible, but what of the partial ones?
    The construction schedule also does not specify a sixteen-pointed star, but this is
    made clear in the summary description. The version here matches the example
    graphic in the constitution schedule.

    I could not find an authority on colors, so defaulted to the Olympic flags manual.
    """
    $ mkCountryFlag
      "NPL"
      "Nepal"
      constructedAt
      ( reference
          "Description"
          flagSpec
          """
          The national flag of Nepal consists of two juxtaposed
          triangular figures with a crimson-coloured base and deep blue borders, there
          being a white emblem of the crescent moon with eight rays visible out of
          sixteen in the upper part and a white emblem of a twelve rayed sun in the lower
          part.
          """
      )
      design
  where
    constructedAt = "2026-03-04"
    gov = mkAgentOrg "npl_gov" "Government of Nepal"

    flagSpec =
      screenshot constructedAt "npl/constitution-1.png" $
        screenshot constructedAt "npl/constitution-2.png" $
          screenshot constructedAt "npl/constitution-3.png" $
            attributeTo gov $
              mkEntity
                "The Constitution of Nepal"
                "https://ag.gov.np/files/Constitution-of-Nepal_2072_Eng_www.moljpa.gov_.npDate-72_11_16.pdf"

    locWithScreenshot = screenshot constructedAt "npl/loc.png" londonOlympicsFlagsManual

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      _ <- reference "Construction" flagSpec ()
      whiteC <- editorial "White" [] (sRGB24 255 255 255)
      redC <- referencePantoneAsRGB locWithScreenshot ("Red", "186-C")
      blueC <- referencePantoneAsRGB locWithScreenshot ("Blue", "287-C")
      pure $ proc (a, b) -> do
        _ <- label "A" -< a
        _ <- label "B" -< b
        (_, d) <- perpendicular >>> second (label "D") -< (a, b)
        c <- rationalMult (4 % 3) >>> label "C" -< (a, d)
        (_, e) <- intersectLC >>> second (label "E") -< ((b, d), (b, a))
        fg' <- parallel -< ((a, b), e)

        f <- intersectLL >>> label "F" -< (fg', (a, d))
        g <- quad >>> label "G" -< (a, b, f)
        h <- rationalMult (1 % 4) >>> label "H" -< (a, b)
        (i', _) <- perpendicular -< (h, a)
        i <- intersectLL >>> label "I" -< ((c, g), (h, i'))
        j <- rationalMult (1 % 2) >>> label "J" -< (c, f)
        (_, k') <- perpendicular -< (j, c)
        k <- intersectLL >>> label "K" -< ((j, k'), (c, g))
        l <- intersectLL >>> label "L" -< ((j, k), (i, h))
        m <- intersectLL >>> label "M" -< ((j, g), (i, h))
        (n', _) <- perpendicular -< (d, b)
        (_, n'') <- translate -< ((d, n'), m)
        n''' <- intersectLL -< ((m, n''), (d, b))
        (_, n) <- intersectLC >>> labelSecond "N" -< ((m, h), (m, n'''))
        (o, _) <- perpendicular >>> labelFirst "O" -< (m, n)
        (p, q) <- intersectLC >>> labelPair "P" "Q" -< ((m, o), (l, n))
        (r, s) <- intersectCC >>> labelPair "R" "S" -< ((l, n), (n, m))
        t <- intersectLL >>> label "T" -< ((r, s), (m, n))

        bg1 <- fillRectangle redC -< (a, b, e, f)
        bg2 <- fillTriangle redC -< (f, c, e)
        bg3 <- fillTriangle redC -< (c, e, g)
        let bg = bg1 <> bg2 <> bg3

        topCrescent <- fillCrescent whiteC -< ((m, p), (l, p))
        topCircle <- fillCircle whiteC -< (t, m)

        moonStarEdge <- halvePerpendicularN 3 -< (t, s)

        topStar <- fillStar16InnerC whiteC -< (t, m, moonStarEdge)
        clipCircle <- fillCircle whiteC -< (m, q)
        topStarClipped <- clipDrawing -< (topStar, clipCircle)
        let moon = topCrescent <> topCircle <> topStarClipped

        u <- rationalMult (1 % 2) >>> label "U" -< (a, f)
        (v', _) <- perpendicular -< (u, a)
        v <- intersectLL >>> label "V" -< ((u, v'), (b, d))
        w <- intersectLL >>> label "W" -< ((u, v), (h, m))
        (_, innerEdge) <- translate -< ((m, n), w)
        (_, outerEdge) <- translate -< ((l, n), w)

        sun <- fillStar12InnerC whiteC -< (w, innerEdge, outerEdge)

        (_, borderBottom) <- translate -< ((t, n), a)
        (_, borderMid1) <- translate -< ((t, n), f)
        (_, borderMid2) <- parallel -< ((f, e), borderMid1)
        (borderLeft1, _) <- perpendicular -< (a, borderBottom)
        (_, borderLeft2) <- parallel -< ((a, u), borderLeft1)

        (_, tPerp) <- perpendicular -< (t, n)
        topDiagPoint <- intersectLL -< ((c, g), (t, tPerp))
        (_, topDiagEdge) <- translate -< ((t, tPerp), topDiagPoint)
        (topDiagEdge', _) <- perpendicular -< (topDiagPoint, g)

        (topDiag1, _) <- intersectLC >>> labelFirst "TD" -< ((topDiagPoint, topDiagEdge'), (topDiagPoint, topDiagEdge))
        (_, topDiag2) <- parallel >>> labelPair "1" "2" -< ((k, g), topDiag1)

        (_, bd') <- translate >>> labelSecond "BD" -< ((n, t), b)
        (bdp, _) <- perpendicular >>> labelSecond "BDP" -< (b, v)
        (_, bottomDiag1) <- intersectLC >>> labelSecond "BDE" -< ((b, bdp), (b, bd'))
        (_, bottomDiag2) <- parallel -< ((b, v), bottomDiag1)

        aa <- quad >>> label "AA" -< (a, borderBottom, borderLeft1)
        gg <- intersectLL >>> label "GG" -< ((topDiag1, topDiag2), (borderMid1, borderMid2))
        cc <- intersectLL >>> label "CC" -< ((topDiag1, topDiag2), (borderLeft1, borderLeft2))
        bb <- intersectLL >>> label "BB" -< ((bottomDiag1, bottomDiag2), (aa, borderBottom))
        ee <- intersectLL >>> label "EE" -< ((bottomDiag1, bottomDiag2), (borderMid1, borderMid2))

        b1 <- fillRectangle blueC -< (aa, a, c, cc)
        b2 <- fillRectangle blueC -< (gg, g, c, cc)
        b3 <- fillRectangle blueC -< (gg, g, e, ee)
        b4 <- fillRectangle blueC -< (bb, b, e, ee)
        b5 <- fillRectangle blueC -< (bb, b, a, aa)
        let border = b1 <> b2 <> b3 <> b4 <> b5

        returnA -< border <> bg <> moon <> sun

    -- | Halve the perpendicular distance n times.
    -- Given (center, edgePoint), find a perpendicular point, then halve the
    -- distance n times by repeatedly taking midpoint and projecting back onto
    -- the circle via intersectLC.
    halvePerpendicularN :: Int -> FlagA (Point, Point) Point
    halvePerpendicularN n = group ("Halve perpendicular " ++ show n ++ " times") $ proc (center, edge) -> do
      (perpPoint, _) <- perpendicular -< (center, edge)
      halveN n -< (center, edge, perpPoint)
      where
        halveN :: Int -> FlagA (Point, Point, Point) Point
        halveN 0 = proc (_, _, p) -> returnA -< p
        halveN k = proc (center, edge, perpPoint) -> do
          midPt <- midpoint -< (edge, perpPoint)
          (_, newPoint) <- intersectLC -< ((center, midPt), (center, edge))
          halveN (k - 1) -< (center, edge, newPoint)
