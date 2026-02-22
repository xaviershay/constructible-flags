{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Pantone
  ( pantoneToRGB
  , referencePantoneAsRGB
  , approximationPantoneAsRGB
  , pantoneAgent
  , cmyk
  ) where

import Flag.Source
import Flag.Design.UnionJack (unionJackFlagSpec)

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

-- | Convert a Pantone key to an sRGB colour, recording the derivation in
-- provenance. @fromLabel@ must match the attribute name used when the
-- Pantone code was sourced (e.g. @"Green Pantone"@), so the PROV graph can
-- emit a @wasDerivedFrom@ link from the RGB attribute back to that attribute.
pantoneToRGB :: Sourced :> es => String -> String -> Eff es (Colour Double)
pantoneToRGB fromLabel key =
  case generatedPantoneRGB key of
    Just (r,g,b) ->
      case generatedPantoneSourceUrl key of
        Just url ->
          let baseEntity = attributeTo pantoneAgent (mkEntity key url)
              chipEntity = case lookup key generatedPantoneList of
                Just (_, _, _, path, _) -> screenshot "" (fromMaybe path (stripPrefix "images/" path)) baseEntity
                Nothing                -> baseEntity
          in derivedFrom (fromLabel ++ " (RGB)") fromLabel chipEntity (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))
        Nothing -> derivedFrom (fromLabel ++ " (RGB)") fromLabel pantone (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))

-- | Source a Pantone colour code from an entity and immediately convert it to
-- an sRGB colour. Combines 'reference' and 'pantoneToRGB' for the common case
-- where both steps share the same label.
referencePantoneAsRGB :: Sourced :> es => Entity -> (String, String) -> Eff es (Colour Double)
referencePantoneAsRGB entity (name, key) = do
  _ <- reference name entity key
  pantoneToRGB name key

-- | Choose a Pantone code as an editorial approximation of a previously-sourced
-- attribute (e.g. a dye name in a spec), and convert it to RGB.
-- @approxOf@ must match the attribute name used when the approximated value
-- was sourced, so the PROV graph emits a @wasInfluencedBy@ link between them.
approximationPantoneAsRGB :: Sourced :> es => String -> [Entity] -> (String, String) -> Eff es (Colour Double)
approximationPantoneAsRGB approxOf refs (name, key) = do
  _ <- approximation name approxOf refs key
  pantoneToRGB name key

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
