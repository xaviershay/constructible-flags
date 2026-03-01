{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Top-level convenience re-export for the @constructible-flags@ library.
--
-- Import this module to get access to the full construction vocabulary,
-- flag definitions, rendering, and source-attribution machinery.
module Flag
    ( -- * Re-exports
      module Flag.Construction.FieldNumber
    , module Flag.Construction.Types
    , module Flag.Construction.Geometry
    , module Flag.Construction.Interpreter
    , module Flag.Construction.Layers
    , module Flag.Construction.Tree
    , module Flag.Construction.Optimize
    , module Flag.Constructions
    , module Flag.Source
    , module Flag.Pantone
    , module Flag.Definition
    , module Flag.Registry
    ) where

import Flag.Construction.FieldNumber
import Flag.Construction.Types
import Flag.Construction.Geometry
import Flag.Construction.Interpreter
import Flag.Construction.Layers
import Flag.Construction.Tree
import Flag.Construction.Optimize
import Flag.Constructions
import Flag.Source
import Flag.Pantone
import Flag.Definition
import Flag.Registry
