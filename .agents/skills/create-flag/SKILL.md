---
name: create-flag
description: Create a new constructible flag module for a country in the constructible-flags Haskell project. Use this when the user asks to add or create a flag for a country.
---

# Create a New Country Flag Module

When asked to create a flag for a country, follow these steps:

First, look up the official ISO 3166-1 alpha-3 code for that country. Use your knowledge of ISO codes — do not guess or make one up. If you are uncertain, use the WebSearch tool to confirm it.

Then follow these steps:

1. **Create the module file** at `src/Flag/Country/<ISO>.hs` using the template below (substitute the 3-letter ISO code and country name appropriately).

Use the simplest flag that already exists (Japan, `src/Flag/Country/JPN.hs`) as a model. The placeholder design should use `boxNatural 3 2` (a standard 3:2 flag) with a white background, leaving a TODO comment for the actual design.

The module template is:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultilineStrings #-}

module Flag.Country.<ISO>
    ( <name>
    ) where

import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)
import Effectful

import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Constructions
import Flag.Source
import Flag.Definition (Flag, mkCountryFlag)

<name> :: Sourced :> es => Flag es
<name> = mkCountryFlag
  "<ISO>"
  "<Country Name>"
  constructedAt
  (reference "Description" flagSpec "TODO: add official flag description")
  design

  where
    constructedAt = "<today's date>"
    gov = mkAgentOrg "<iso_lower>_gov" "Government of <Country Name>"

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
```

Where:
- `<ISO>` = the 3-letter ISO code in uppercase (e.g. `JPN`)
- `<name>` = the lowercase country name as a Haskell identifier (e.g. `japan`)
- `<Country Name>` = the full country name (e.g. `Japan`)
- `<iso_lower>` = lowercase ISO code (e.g. `jpn`)
- `<today's date>` = today's date in YYYY-MM-DD format

After creating the files, run `stack build` to verify the code compiles.

2. **Update `test/FlagsUnderConstruction.hs`** — replace whatever ISO codes are currently in the `underConstruction` list with just the newly created flag's ISO code (uppercase). For example, if you just created `NZL`:

```haskell
underConstruction :: [String]
underConstruction =
  [ "NZL"
  ]
```

3. **Create image directory** with lower case ISO code:

   mkdir data/images/nzl

4. Done

The flag does NOT need to be added to Registry.hs - that happens as part of the build step.
