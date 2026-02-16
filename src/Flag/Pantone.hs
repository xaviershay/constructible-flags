{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Pantone
    ( pantoneToRGB
    , pantoneAgent
    ) where

import Flag.Source

import Data.Colour
import Data.Colour.SRGB (sRGB24)
import Effectful

import Flag.Source (Agent, mkAgentOrg, mkEntity, attributeTo)
import Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneSourceUrl)

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
          let chipEntity = attributeTo pantoneAgent (mkEntity key url)
          in reference "RGB Conversion" chipEntity (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))
        Nothing -> reference "RGB Conversion" pantone (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))
    Nothing -> error $ "pantoneToRGB: unknown Pantone key: " ++ key
