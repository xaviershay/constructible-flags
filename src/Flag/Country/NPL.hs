{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Flag.Definition (Flag, mkCountryFlag)
import Flag.Pantone (referencePantoneAsRGB)
import Flag.Source

nepal :: (Sourced :> es) => Flag es
nepal =
  mkCountryFlag
    "NPL"
    "Nepal"
    constructedAt
    (reference "Description" flagSpec "TODO: add official flag description")
    design
  where
    constructedAt = "2026-03-02"
    gov = mkAgentOrg "npl_gov" "Government of Nepal"

    flagSpec =
      attributeTo gov $
        mkEntity
          "TODO: add official flag specification title"
          "TODO: add URL"

    design :: (Sourced :> es) => Eff es (FlagA (Point, Point) Drawing)
    design = do
      -- TODO: source dimensions from flagSpec
      whiteC <- impliedReference "White" flagSpec (sRGB24 255 255 255)
      redC <- referencePantoneAsRGB flagSpec ("Red", "186-C")
      blueC <- referencePantoneAsRGB flagSpec ("Blue", "287-C")
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

        -- TODO: Add the moon triangles
        topCrescent <- fillCrescent whiteC -< ((m, p), (l, p))
        topCircle <- fillCircle whiteC -< (t, m)
        (_, v2) <- intersectCC -< ((t, s), (s, t))
        v3 <- midpoint -< (s, v2)
        v4 <- midpoint -< (s, v3)
        (_, v6) <- intersectLC -< ((t, v4), (t, s))
        topStar <- fillStar12InnerC whiteC -< (t, m, v6)
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
