{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Pantone
    ( PantoneId(..)
    , pmsToRGB
    ) where

import Data.Colour
import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Source

-- | Pantone color identifier
data PantoneId =
    PMSRed032C
  | PMSReflexBlueC
  | PMS154225TCX
  | PMS342C
  | PMS485C
  deriving (Show, Eq)

pantone :: Source
pantone = SourceAuthoritativeWebsite "Pantone" "https://www.pantone.com/"

-- | Convert a Pantone identifier to RGB color (sourced from Pantone website)
pmsToRGB :: Sourced :> es => PantoneId -> Eff es (Colour Double)
pmsToRGB PMSRed032C = sourced "RGB Conversion" pantone (sRGB24 230 49 62)
pmsToRGB PMSReflexBlueC = sourced "RGB Conversion" pantone (sRGB24 16 11 136)
pmsToRGB PMS154225TCX = sourced "RGB Conversion" pantone (sRGB24 117 168 210)
pmsToRGB PMS342C = sourced "RGB Conversion" pantone (sRGB24 24 104 72)
pmsToRGB PMS485C = sourced "RGB Conversion" pantone (sRGB24 210 40 24)
