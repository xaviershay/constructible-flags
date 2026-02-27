{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Other.Aboriginal
    ( aboriginal
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Data.Ratio ((%))
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, FlagCategory(..), mkOtherFlag)

aboriginal :: Sourced :> es => Flag es
aboriginal = mkOtherFlag
  Cultural
  "ABORIGINAL"
  "Australian Aboriginal Flag"
  constructedAt
  (editorial "Description" []
      ( "A horizontal bicolour of black (top) and red (bottom), with a yellow "
     ++ "circle centred on the boundary between the two halves." ))
  design

  where
    constructedAt = "2026-02-27"

    -- Designed by Harold Thomas in 1971.
    flagSpec = mkEntity
        "Australian Aboriginal Flag (Wikipedia)"
        "https://en.wikipedia.org/wiki/Australian_Aboriginal_Flag"

    flagAct = mkEntity
        "Flags Act 1953 (Australian Aboriginal Flag Proclamation)"
        "https://www.legislation.gov.au/Details/F2008L02462"

    references = [flagSpec, flagAct]

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blackC  <- editorial "Black"  references (sRGB24 0x00 0x00 0x00)
        redC    <- editorial "Red"    references (sRGB24 0xCC 0x00 0x00)
        yellowC <- editorial "Yellow" references (sRGB24 0xFF 0xCD 0x00)

        -- Aspect ratio 1:2 (height:width).
        _ <- editorial "1:2 proportion" references ()

        -- The yellow circle has a diameter of approximately 13/30 of the flag
        -- width, per the Commonwealth Gazette proclamation. Here we use 1/3 of
        -- the flag width (= 2/3 of the flag height) as an editorial
        -- approximation pending a more precise source.
        circleRadiusRatio <- editorial "Circle radius (approx 1/6 of width)" references (1 % 6)

        pure $ proc (origin, unitRef) -> do
            (tl, tr, br, bl) <- boxNatural 2 1 -< (origin, unitRef)

            -- Horizontal midpoints (boundary between black and red halves)
            leftMid  <- midpoint -< (tl, bl)
            rightMid <- midpoint -< (tr, br)

            -- Centre of flag: intersection of diagonals
            flagCenter <- intersectLL -< ((tl, br), (tr, bl))

            -- Radius point: 1/6 of flag width to the right of centre
            -- (width = 2 units, so 1/6 of 2 = 1/3 unit)
            radiusEdge <- rationalMult circleRadiusRatio -< (tl, tr)
            (_, circleEdge) <- translate -< ((tl, radiusEdge), flagCenter)

            topHalf    <- fillRectangle blackC  -< (tl, tr, rightMid, leftMid)
            bottomHalf <- fillRectangle redC    -< (leftMid, rightMid, br, bl)
            circle     <- fillCircle    yellowC -< (flagCenter, circleEdge)

            returnA -< topHalf <> bottomHalf <> circle
