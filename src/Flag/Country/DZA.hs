{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Flag.Country.DZA
    ( algeria
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag)

algeria :: Sourced :> es => Flag es
algeria = mkCountryFlag
  "DZA"
  "Algeria"
  constructedAt
  (reference "Description" flagSpec "TODO: add official flag description")
  design

  where
    constructedAt = "2026-02-28"
    gov = mkAgentOrg "dza_gov" "Government of Algeria"

    flagSpec = attributeTo gov $ mkEntity
        "TODO: add official flag specification title"
        "TODO: add URL"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        -- TODO: source dimensions from flagSpec
        whiteColor <- impliedReference "White" flagSpec (sRGB24 255 255 255)
        pure $ proc origin -> do
            -- TODO: implement actual flag design
            (tl, tr, br, bl) <- boxNatural 3 2 -< origin
            bg <- fillRectangle whiteColor -< (tl, tr, br, bl)
            returnA -< bg
