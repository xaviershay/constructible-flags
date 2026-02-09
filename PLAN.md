# Module Refactoring Plan

## Current State

Everything lives in three files:

| File | Lines | Responsibilities |
|---|---|---|
| `src/FlagConstruction.hs` | 610 | Core types (`Point`, `Drawing`, `FlagA` GADT), arrow instances, smart constructors, geometric primitives (eval functions), step/layer/tree interpreters, drawing optimization, composite constructions (`perpendicular`, `midpoint`, `rationalMult`, etc.), `fillRectangle`/`fillBox` drawing helpers |
| `src/Lib.hs` | 160 | `Sourced` effect + interpreters, `Source` data type, `PantoneId` + conversion, `Flag` record type, individual flag definitions (`france`, `japan`), `allCountryFlags` |
| `app/Main.hs` | 450 | `Drawing` → `Diagram B` rendering, construction-layer → `Diagram B` debug rendering (lines, circles, dots), numbered-tree logic, debug HTML generation, index HTML generation, `buildDebug` and `buildHtml` entry points, HTML escaping |

### Problems

1. **`FlagConstruction.hs` is a grab-bag.** It mixes the free-arrow GADT, its raw geometric evaluators, three different interpreter strategies (eval, evalLayers, evalTree), drawing optimization, and high-level compass constructions (perpendicular, midpoint, rationalMult). These are independent concerns.

2. **`Lib.hs` conflates sourcing infrastructure with flag data.** The `Sourced` effect, its interpreters, and Pantone conversion are general infrastructure; the individual flag definitions are domain data. They should be separate so adding a new flag doesn't touch the effect machinery.

3. **`Main.hs` is doing too much.** It contains rendering logic (Drawing → Diagram, construction geometry → Diagram), HTML generation (two separate page types), numbered-tree traversal, and the two build entry points. These are distinct concerns that can't be reused independently.

4. **No clear dependency direction.** Flag definitions depend on constructions; rendering depends on the layer types; HTML generation depends on rendering output — but all of this is flattened.

---

## Proposed Module Structure

```
src/
  Flag/
    Construction/
      Types.hs          -- Point, Drawing, FlagA GADT, Arrow/Category instances
      Geometry.hs       -- Raw evaluators: evalIntersectLL', evalIntersectLC', evalIntersectCC', dist
      Interpreter.hs    -- eval, Step, steps
      Layers.hs         -- ConstructionLayer, evalLayers, layerInputPoints, layerOutputPoints, pointDist
      Tree.hs           -- ConstructionTree, evalTree, flattenTree
      Optimize.hs       -- optimize (merge same-colour triangles into paths)
    Constructions.hs    -- Smart constructors and composite constructions:
                        --   intersectLL/LC/CC, fillTriangle, fillCircle, group,
                        --   perpendicular, parallel, midpoint, naturalMult,
                        --   rationalMult, quad, boxNatural, fillRectangle, fillBox
    Source.hs           -- Source data type, Sourced effect, interpreters
                        --   (runSourcedPure, runSourcedTrace, runSourcedCollect)
    Pantone.hs          -- PantoneId, pmsToRGB
    Definition.hs       -- Flag record type, allCountryFlags
    Country/
      France.hs         -- france
      Japan.hs          -- japan
    Render/
      Diagram.hs        -- Drawing → Diagram B, construction geometry → Diagram B,
                        --   renderDots, renderLine, renderCircle, renderFill
      Html.hs           -- HTML generation (index page, debug page), escapeHtml
      Debug.hs          -- NumberedEntry, numberTree, numberedLeaves, printNumberedTree,
                        --   buildDebug logic
  Flag.hs               -- Re-export module (convenient single import)
app/
  Main.hs               -- Thin entry point: parse mode, call buildDebug or buildHtml
```

### Dependency Graph

```
                    Flag.Construction.Types
                     /          |         \
          Geometry  /           |          \
              \    /            |           \
          Interpreter      Layers          Optimize
                              |
                            Tree

        Flag.Constructions  (imports Types, re-exports smart constructors)

        Flag.Source          (standalone, depends on effectful)
        Flag.Pantone         (imports Source)

        Flag.Flags.France  \
        Flag.Flags.Japan    }-- import Constructions, Source, Pantone
        ...                /

        Flag.Definition      (imports Flag type, collects allCountryFlags from Flags.*)

        Flag.Render.Diagram  (imports Types, Layers)
        Flag.Render.Html     (imports Source, Definition)
        Flag.Render.Debug    (imports Tree, Layers, Diagram)

        Main                 (imports Definition, Render.Debug, Render.Html)
```

---

## Refactoring Steps

Execute in order. Each step should compile before moving to the next.

### Step 1: Create `Flag.Construction.Types`

Extract from `FlagConstruction.hs`:
- `Point` type alias
- `Drawing` data type + `Semigroup`/`Monoid` instances
- `FlagA` GADT + `Category`/`Arrow`/`Show` instances + `showFlagA`

No other module changes yet — `FlagConstruction` re-exports everything from `Types`.

### Step 2: Create `Flag.Construction.Geometry`

Extract from `FlagConstruction.hs`:
- `evalIntersectLL'`, `evalIntersectLC'`, `evalIntersectCC'`
- `dist` (internal helper)

These are pure math with no dependency beyond `Types.Point`.

### Step 3: Create `Flag.Construction.Interpreter`

Extract from `FlagConstruction.hs`:
- `Step` data type
- `steps` function
- `eval` function

Depends on `Types` and `Geometry`.

### Step 4: Create `Flag.Construction.Layers`

Extract from `FlagConstruction.hs`:
- `ConstructionLayer` data type
- `layerInputPoints`, `layerOutputPoints`
- `pointDist` (re-export of `dist`)
- `evalLayers`

Depends on `Types` and `Geometry`.

### Step 5: Create `Flag.Construction.Tree`

Extract from `FlagConstruction.hs`:
- `ConstructionTree` data type
- `evalTree`
- `flattenTree`

Depends on `Types`, `Geometry`, and `Layers`.

### Step 6: Create `Flag.Construction.Optimize`

Extract from `FlagConstruction.hs`:
- `optimize` function and all its local helpers

Depends on `Types` only.

### Step 7: Create `Flag.Constructions`

Extract from `FlagConstruction.hs`:
- Smart constructors: `intersectLL`, `intersectLC`, `intersectCC`, `fillTriangle`, `fillCircle`, `group`
- Composite constructions: `perpendicular`, `parallel`, `midpoint`, `naturalMult`, `rationalMult`, `quad`, `boxNatural`, `fillRectangle`, `fillBox`
- Local colour helpers (`red`, `blue`, `white`) — evaluate whether these belong here or should be removed; they appear unused outside the module.

Depends on `Types` only (uses `FlagA` constructors).

### Step 8: Replace `FlagConstruction.hs` with a re-export shim

`FlagConstruction` becomes:

```haskell
module FlagConstruction (module X) where
import Flag.Construction.Types as X
import Flag.Construction.Geometry as X
import Flag.Construction.Interpreter as X
import Flag.Construction.Layers as X
import Flag.Construction.Tree as X
import Flag.Construction.Optimize as X
import Flag.Constructions as X
```

This keeps all downstream code compiling during the transition. It can be removed once all imports are updated.

### Step 9: Create `Flag.Source`

Extract from `Lib.hs`:
- `Source` data type
- `Sourced` effect + `DispatchOf` instance
- `sourced`, `sourcedM`
- `SourcedElement` type alias
- `runSourcedPure`, `runSourcedTrace`, `runSourcedCollect`
- `sourceDescription` (if retained)

### Step 10: Create `Flag.Pantone`

Extract from `Lib.hs`:
- `PantoneId` data type
- `pmsToRGB`
- The `pantone` source constant

Depends on `Flag.Source`.

### Step 11: Create `Flag.Flags.France` and `Flag.Flags.Japan`

Extract each flag definition into its own module. Each imports `Flag.Constructions`, `Flag.Source`, and `Flag.Pantone` as needed.

### Step 12: Create `Flag.Definition`

Extract from `Lib.hs`:
- `Flag` record type
- `allCountryFlags` (imports individual flag modules)

### Step 13: Replace `Lib.hs` with a re-export shim

Same pattern as step 8. Can be removed once `Main.hs` and any other consumers are updated.

### Step 14: Create `Flag.Render.Diagram`

Extract from `Main.hs`:
- `drawingToDiagram`
- `renderConstructionGeom`, `renderFill`, `renderLayer`
- `renderLine`, `renderCircle`, `renderDots`

### Step 15: Create `Flag.Render.Html`

Extract from `Main.hs`:
- `generateIndex`, `flagRow`, `formatSteps`, `formatSources`
- `escapeHtml`
- `generateDebugIndex`, `renderEntries`, `renderEntry`

### Step 16: Create `Flag.Render.Debug`

Extract from `Main.hs`:
- `NumberedEntry`, `numberTree`, `numberedLeaves`
- `layerLabel`, `padNum`
- `printNumberedTree`
- The body of `buildDebug`

### Step 17: Slim down `Main.hs`

`Main.hs` becomes a thin entry point that selects between `buildDebug` and `buildHtml`, importing from `Flag.Render.*` and `Flag.Definition`.

### Step 18: Create `Flag.hs` convenience re-export

A single import for downstream users:

```haskell
module Flag (module X) where
import Flag.Construction.Types as X (Point, Drawing(..), FlagA(..))
import Flag.Constructions as X
import Flag.Source as X
import Flag.Definition as X
```

### Step 19: Remove shims

Delete `FlagConstruction.hs` and `Lib.hs`, update all imports to use the new module paths directly.

### Step 20: Update `package.yaml`

Ensure `source-dirs` and `exposed-modules` (if using explicit module lists) reflect the new structure. No changes needed if using `source-dirs: src` with auto-discovery.

---

## Notes

- **Unused code.** The local `red`, `blue`, `white` colour constants in `FlagConstruction.hs` appear unused outside the module. Consider removing them during step 7. Remove them.
- **`sources/` directory** is currently empty. It can remain for future use or be removed. Remove it.
- **`pointDist` vs `dist`.** Currently `dist` is internal and `pointDist` re-exports it. After the split, `Geometry` can export `dist` directly and `pointDist` becomes a re-export alias in `Layers` for backward compat, or just use `dist` everywhere. dist everywhere.
- **The `londonOlympics2012` and `habitual` source constants** in `Lib.hs` are currently unused outside their module. They may belong in individual flag modules or in `Flag.Pantone` depending on future usage. Let's delete them.
