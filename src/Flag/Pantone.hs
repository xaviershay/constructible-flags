{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Pantone
  ( pantoneToRGB
  , pantoneAgent
  , cmyk
  ) where

import Flag.Source

import Data.Colour
import Data.Colour.SRGB (sRGB24, sRGB)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Effectful

import Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneSourceUrl, generatedPantoneList)

-- | Agent representing the Pantone organisation
pantoneAgent :: Agent
pantoneAgent = mkAgentOrg "pantone" "Pantone"

-- | Generic Pantone entity (kept for backwards compatibility in URLs)
pantone :: Entity
pantone = mkEntity "Pantone" "https://www.pantone.com/"

-- | Lookup by string key (compiled from data/pantone.json). When available
-- the generated module provides a sourceUrl for the chip; in that case we
-- attribute a chip-specific entity to the Pantone agent so provenance can
-- show the chip image and a later `color-sample` activity can be emitted.
pantoneToRGB :: Sourced :> es => String -> Eff es (Colour Double)
pantoneToRGB key =
  case generatedPantoneRGB key of
    Just (r,g,b) ->
      case generatedPantoneSourceUrl key of
        Just url ->
          let baseEntity = attributeTo pantoneAgent (mkEntity key url)
              chipEntity = case lookup key generatedPantoneList of
                Just (_, _, _, path, _) -> screenshot "" (fromMaybe path (stripPrefix "images/" path)) baseEntity
                Nothing                -> baseEntity
          in reference "RGB Conversion" chipEntity (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))
        Nothing -> reference "RGB Conversion" pantone (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))
    Nothing -> error $ "pantoneToRGB: unknown Pantone key: " ++ key


-- | Convert CMYK percentages (0-100) to an sRGB `Colour Double`.
-- Uses the standard subtractive-to-additive conversion: r = (1-C)*(1-K),
-- g = (1-M)*(1-K), b = (1-Y)*(1-K).
cmyk :: Double -> Double -> Double -> Double -> Colour Double
cmyk c m y k =
  let cf = c / 100
      mf = m / 100
      yf = y / 100
      kf = k / 100
      r = (1 - cf) * (1 - kf)
      g = (1 - mf) * (1 - kf)
      b = (1 - yf) * (1 - kf)
  in sRGB r g b
