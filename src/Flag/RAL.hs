{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Flag.RAL
  ( ralToRGB
  , referenceRALAsRGB
  , ralAgent
  ) where

import Data.Colour
import Data.Colour.SRGB (sRGB24)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Effectful
import Flag.GeneratedRAL (generatedRALList, generatedRALRGB, generatedRALSourceUrl)
import Flag.Source

-- | Agent representing qconv, who provides the RAL→RGB calculation.
ralAgent :: Agent
ralAgent = mkAgentOrg "https://www.qconv.com/" "QConv"

-- | Convert a RAL code to sRGB. Records a Convert action by qconv derived
-- directly from the sourced RAL attribute.
ralToRGB :: Sourced :> es => String -> Int -> Eff es (Colour Double)
ralToRGB fromLabel code =
  let key = show code
      baseEntity = case generatedRALSourceUrl key of
                     Just url -> attributeTo ralAgent (mkEntity ("RAL " ++ key) url)
                     Nothing  -> attributeTo ralAgent (mkEntity ("RAL " ++ key) "https://qconv.com/")
      qconvEntity = case lookup key generatedRALList of
                      Just (_, _, _, path, _) -> screenshot "" (fromMaybe path (stripPrefix "images/" path)) baseEntity
                      Nothing                 -> baseEntity
  in case generatedRALRGB key of
    Nothing -> error $ "Unknown RAL key: " ++ key
    Just (r, g, b) ->
      convertedFrom (fromLabel ++ " (RGB)") fromLabel qconvEntity
        (sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b))

-- | Reference a RAL code from an entity and convert it to RGB.
referenceRALAsRGB :: Sourced :> es => Entity -> (String, Int) -> Eff es (Colour Double)
referenceRALAsRGB entity (name, code) = do
  _ <- reference name entity (show code)
  ralToRGB name code
