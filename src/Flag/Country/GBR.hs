{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.GBR
    ( unitedKingdom
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA, arr)
import Debug.Trace (trace)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Construction.Radical (toKaTeX)
import Flag.Definition (Flag, mkCountryFlag)

unitedKingdom :: Sourced :> es => Flag es
unitedKingdom = mkCountryFlag
  "GBR"
  "United Kingdom"
  (reference "Description" flagSpec "TODO: add official flag description")
  design

  where
    constructedAt = "2026-02-21"
    gov = mkAgentOrg "gbr_gov" "Government of United Kingdom"

    flagSpec = attributeTo gov $ mkEntity
        "TODO: add official flag specification title"
        "TODO: add URL"

    tracePoints :: [Point] -> [Point]
    tracePoints pts = trace (unwords (map showPt pts)) pts
      where showPt (x, y) = "(" ++ toKaTeX x ++ ", " ++ toKaTeX y ++ ")"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- TODO: source dimensions from flagSpec
        blueC <- impliedReference "White" flagSpec (sRGB24 0 0 255)
        redC <- impliedReference "White" flagSpec (sRGB24 255 0 0)
        pure $ proc (origin, unit) -> do
            -- TODO: implement actual flag design
            (tl, tr, br, bl) <- boxNatural 50 30 -< (origin, unit)
            center <- intersectLL -< ((tl, br), (tr, bl))
            (_, unitDown) <- perpendicular -< (origin, unit)

            topMid <- midpoint -< (tl, tr)
            leftMid <- midpoint -< (tl, bl)

            three <- naturalMult 3 -< (origin, unit)

            (_, a) <- intersectLC -< ((tl, center), (origin, three))
            (b, _) <- perpendicular -< (a, origin)
            --(c, _) <- intersectLC -< ((a, b), (a, origin))
            c <- parallel -< ((origin, a), b)

            d <- intersectLL -< ((tl, tr), c)

            --11'oclock blue triangle
            v4' <- naturalMult 5 -< (origin, unit)
            (_, v4) <- translate -< ((origin, v4'), topMid)
            (_, v4Down) <- parallel -< ((tl, bl), v4)

            (v1, _) <- intersectLC -< ((origin, topMid), (topMid, v4))
            (_, v1Down) <- parallel -< ((tl, bl), v1)

            t1 <- intersectLL -< ((v1, v1Down), c)

           -- Middle vertical stripe
            v3' <- naturalMult 3 -< (origin, unit)
            (_, v3) <- translate -< ((origin, v3'), topMid)
            (_, v3Down) <- parallel -< ((tl, bl), v3)

            (v2, _) <- intersectLC -< ((origin, topMid), (topMid, v3))
            (_, v2Down) <- parallel -< ((tl, bl), v2)

           -- Middle horizontal stripe
            h3' <- naturalMult 3 -< (origin, unitDown)
            (_, h3) <- translate -< ((origin, h3'), leftMid)
            (_, h3Down) <- parallel -< ((tl, tr), h3)

            (_, h2) <- intersectLC -< ((origin, leftMid), (leftMid, h3))
            (_, h2Down) <- parallel -< ((tl, tr), h2)

            h1' <- naturalMult 5 -< (origin, unitDown)
            (_, h1) <- translate -< ((origin, h1'), leftMid)
            (_, h1Down) <- parallel -< ((tl, tr), h1)

            (_, h4) <- intersectLC -< ((origin, leftMid), (leftMid, h1))
            (_, h4Down) <- parallel -< ((tl, tr), h4)
            _ <- arr tracePoints -< [h1, h2, h3, h4]

            --4'oclock blue triangle
            t4 <- intersectLL -< ((h4, h4Down), c)
            t4' <- intersectLL -< ((tr, br), c)
            b4 <- fillTriangle blueC -< (t4, h4Down, t4')

            -- Triangles
            rVert <- fillRectangle redC -< (v2, v3, v3Down, v2Down)
            b11 <- fillTriangle blueC -< (t1, v1, d)
            rHorz <- fillRectangle redC -< (h2, h3, h3Down, h2Down)
            --s1 <- fillTriangle redC -< (tl, d, a)
            returnA -< b4 <> b11 <> rVert <> rHorz
