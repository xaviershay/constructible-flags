I've made changes to the desired interface for sourcing attributes, you can see in flag files (e.g. FRA.hs). It doesn't compile.

1. Change Source effect implementation and data model to support the given interface. (If anything in the interface seems weird or inconsistent, make a best effort to resolve - Iv'e probably made a typo)
2. Come up with a way to transform a collected source document into a W3C PROV XML document. Be explicit about generatedby/derivefrom/influencedby. For editorial decisions, attribute to an "editor" agent which is hard coded to me (Xavier Shay).
3. Emit XML document when generating site, and link to XML for each flag from "Source" cell in table.

---

## Implementation Plan

### Step 1: Rewrite Source data model and effect (Flag/Source.hs)

**New types:**

```haskell
data Agent = AgentOrg
  { agentId   :: String  -- e.g. "fra_gov"
  , agentName :: String  -- e.g. "French Government"
  }

data Entity = Entity
  { entityTitle      :: String
  , entityUrl        :: String
  , entityAgent      :: Maybe Agent      -- attributeTo
  , entityScreenshot :: Maybe (String, String)  -- (date, path)
  , entityTranslated :: Maybe String     -- date of translation access
  }
```

**Constructor functions (pure, no effect needed):**

- `mkAgentOrg :: String -> String -> Agent`
- `mkEntity :: String -> String -> Entity`
- `attributeTo :: Agent -> Entity -> Entity`
- `screenshot :: String -> String -> Entity -> Entity`
- `translated :: String -> Entity -> Entity`

**New Source sum type (replaces old Source):**

```haskell
data Source
  = SourceReference Entity           -- explicit value from document
  | SourceImpliedReference Entity    -- value implied by document
  | SourceUnsightedReference Entity [Entity]  -- value from unsighted spec + corroborating refs
  | SourceEditorial [Entity]         -- editorial decision, with references
  | SourceAttributeFromEntity Entity -- value extracted directly from entity
  | SourcePantoneConversion Entity   -- for Pantone RGB conversions
  deriving (Show, Eq)
```

**New sourcing functions:**

- `reference :: Sourced :> es => String -> Entity -> a -> Eff es a`
- `impliedReference :: Sourced :> es => String -> Entity -> a -> Eff es a`
- `unsightedReference :: Sourced :> es => String -> Entity -> [Entity] -> a -> Eff es a`
- `editorial :: Sourced :> es => String -> [Entity] -> a -> Eff es a`
- `attributeFromEntity :: Sourced :> es => String -> Entity -> a -> Eff es a`

Keep `sourced` as a low-level function for backward compat (used by Pantone.hs internally).

**Inconsistencies to fix in flag files:**

- BWA.hs line 37: `standards :: Source` should be `standards :: Entity` (type annotation typo)
- BWA.hs line 23: `sourced "Description" law` should be `attributeFromEntity "Description" law` (law is Entity now)

**Update Pantone.hs:** Change `pantone` from `Source` to `Entity`, use `reference` instead of `sourced`.

### Step 2: Update interpreters

- `runSourcedPure` — unchanged (ignores source metadata)
- `runSourcedTrace` — update string formatting for new Source variants
- `runSourcedCollect` — collects `(String, Source)` pairs as before, new type just has richer data

**Collected data type change:** `SourcedElement = (String, Source)` still works, but Source now carries Entity/Agent data.

### Step 3: Update Html.hs source rendering

Update `formatSources` / `formatSourceWithElements` to handle new Source variants. Group by entity rather than source type. Show entity title as link, with source relationship type in parentheses.

### Step 4: Create PROV XML generation (new module Flag/Render/Prov.hs)

**PROV mapping:**

| Source variant | PROV relationship | Agent |
|---|---|---|
| `SourceReference entity` | value `prov:wasDerivedFrom` entity | entity's agent |
| `SourceImpliedReference entity` | value `prov:wasInfluencedBy` entity | entity's agent |
| `SourceUnsightedReference entity refs` | value `prov:wasDerivedFrom` entity, value `prov:wasInfluencedBy` each ref | entity's agent |
| `SourceEditorial refs` | value `prov:wasAttributedTo` editor agent, value `prov:wasInfluencedBy` each ref | "editor" (Xavier Shay) |
| `SourceAttributeFromEntity entity` | value `prov:wasDerivedFrom` entity | entity's agent |
| `SourcePantoneConversion entity` | value `prov:wasDerivedFrom` entity | entity's agent |

**The "flag construction" is modeled as a `prov:Activity`** that `prov:used` each source entity and `prov:wasAssociatedWith` the editor agent. The final flag entity `prov:wasGeneratedBy` this activity.

**XML format:** Standard W3C PROV-XML namespace (`http://www.w3.org/ns/prov#`).

Function: `generateProvXml :: String -> String -> [SourcedElement] -> String`
Takes ISO code, flag name, collected sources, returns XML string.

### Step 5: Wire up in Main.hs

- Import `Flag.Render.Prov`
- After processing each flag, write `out/{isocode}-prov.xml`
- Pass PROV XML filename to `generateIndex` so it can link from Sources column

### Step 6: Add PROV link in HTML table

Add a small link like `[PROV]` in the Sources cell pointing to `{isocode}-prov.xml`.

---

## Progress

- [x] Step 1: Rewrite Source data model and effect
- [x] Step 1b: Fix flag file inconsistencies (BWA.hs: `sourced` -> `attributeFromEntity`, `:: Source` -> `:: Entity`)
- [x] Step 1c: Update Pantone.hs (`pantone :: Entity`, use `reference` instead of `sourced`)
- [x] Step 1d: Fix FRA.hs/JPN.hs (`sourced` -> `attributeFromEntity` for descriptions)
- [x] Step 2: Update interpreters (done as part of Source.hs rewrite)
- [x] Step 3: Update Html.hs source rendering
- [x] Step 4: Create PROV XML generation module (Flag/Render/Prov.hs)
- [x] Step 5: Wire up in Main.hs
- [x] Step 6: Add PROV link in HTML table
- [x] Verify: Project compiles, all 121 tests pass, builds and generates output successfully
