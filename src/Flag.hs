{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Top-level convenience re-export for the @constructible-flags@ library.
--
-- Import this module to get access to the full construction vocabulary,
-- flag definitions, rendering, and source-attribution machinery.
module Flag
  ( -- * Re-exports
  )
where

import Flag.Construction.FieldNumber
import Flag.Construction.Geometry
import Flag.Construction.Interpreter
import Flag.Construction.Layers
import Flag.Construction.Optimize
import Flag.Construction.Tree
import Flag.Construction.Types
import Flag.Constructions
import Flag.Definition
import Flag.Pantone
import Flag.Registry
import Flag.Source
