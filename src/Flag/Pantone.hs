{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Pantone
    ( PantoneId(..)
    , pmsToRGB
    , pantoneToRGB
    ) where

import Data.Colour
import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Source
import Flag.GeneratedPantone (generatedPantoneRGB)

-- | Pantone color identifier
data PantoneId =
    PMSRed032C
  | PMSReflexBlueC
  | PMS154225TCX
  | PMS342C
  | PMS485C
  deriving (Show, Eq)

pantone :: Entity
pantone = mkEntity "Pantone" "https://www.pantone.com/"

-- | Lookup by string key (compiled from data/pantone.json)
pantoneToRGB :: Sourced :> es => String -> Eff es (Colour Double)
pantoneToRGB key =
  case generatedPantoneRGB key of
    Just (r,g,b) -> reference "RGB Conversion" pantone (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))
    Nothing -> error $ "pantoneToRGB: unknown Pantone key: " ++ key

-- | Backwards-compatible shim for existing PantoneId sum type
pmsToRGB :: Sourced :> es => PantoneId -> Eff es (Colour Double)
pmsToRGB PMSRed032C     = pantoneToRGB "PMSRed032C"
pmsToRGB PMSReflexBlueC = pantoneToRGB "PMSReflexBlueC"
pmsToRGB PMS154225TCX   = pantoneToRGB "PMS154225TCX"
pmsToRGB PMS342C        = pantoneToRGB "PMS342C"
pmsToRGB PMS485C        = pantoneToRGB "PMS485C"
