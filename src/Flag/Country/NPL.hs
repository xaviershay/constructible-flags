{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.NPL
    ( nepal
    ) where

import Data.Colour.SRGB (sRGB24)
import Data.Ratio
import Control.Arrow (returnA, (>>>), second)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag)

nepal :: Sourced :> es => Flag es
nepal = mkCountryFlag
  "NPL"
  "Nepal"
  constructedAt
  (reference "Description" flagSpec "TODO: add official flag description")
  design

  where
    constructedAt = "2026-03-02"
    gov = mkAgentOrg "npl_gov" "Government of Nepal"

    flagSpec = attributeTo gov $ mkEntity
        "TODO: add official flag specification title"
        "TODO: add URL"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- TODO: source dimensions from flagSpec
        whiteC <- impliedReference "White" flagSpec (sRGB24 255 0 0 )
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

            topCrescent <- fillCrescent whiteC -< ((m, p), (l, p))

            blah <- fillTriangle whiteC -< (a, b, d)
            returnA -< blah <> topCrescent
