# Pantone → RGB: implementation plan

Goal
- Provide a simple CLI + JSON-backed mapping so new Pantone chips can be added reliably and produce provenance.
- Replace the current Pantone sum-type with a string-indexed lookup; keep `data/pantone.json` as the editable source-of-truth and generate a Haskell module from it during pre-build (`Setup.hs`) so the lookup is compiled into the binary.
- When a chip is added the system must: store the chip image in `data/images/pantone/`, extract the top-left RGB, write/update `data/pantone.json`, and record provenance: chip = entity (agent Pantone) + attribute produced by a `color-sample` activity.

Success criteria ✅
- `pantone-sample <KEY>` downloads chip (if missing), extracts RGB and updates `data/pantone.json`.
- Compiled lookup: `pmsToRGB` (or `pantoneToRGB`) is implemented via a generated Haskell module produced from `data/pantone.json` at build-time (via `Setup.hs`). Updating `data/pantone.json` requires rebuilding to include new mappings in the compiled binary.
- All existing flags that used the hard-coded Pantone constructors still render correctly after migration.
- New entries appear in provenance (entity + color-sample activity) so UI shows the chip/source.

Design decisions (summary) 💡
- Key format: use uppercase constructor-like string keys that match Pantone (e.g. RED-032-C`), this will mean changing the existing API.
- Store mapping in `data/pantone.json` (single source-of-truth). During build `Setup.hs` will read this JSON and generate a Haskell module (e.g. `src/Flag/GeneratedPantone.hs`) so lookups are compiled in. Example entry provided below.
- CLI: new small Haskell executable `pantone-sample` (preferred) that: downloads chip, extracts top-left pixel, writes mapping. CLI will prefer an in-process image decode but fall back to `dwebp`/`convert` if needed.
- Migration: skip straight to final API, don't worry about breaking compatibility

Files to add / edit (high level) 🔧
- Add: `data/pantone.json` (initial mapping for existing colors)
- Add: `data/images/pantone/` (image files created by CLI)
- Add executable: `app/PantoneCli.hs` + package.yaml entry / `bin/pantone-sample` wrapper
- Edit: `Setup.hs` — read `data/pantone.json` during pre-build and generate `src/Flag/GeneratedPantone.hs` (compiled lookup)
- Edit: `src/Flag/Pantone.hs` — add JSON loader, new lookup API, deprecation shim, update provenance wiring
- Edit: call sites that use Pantone constructors: `src/Flag/Country/*.hs` (BGD.hs, BWA.hs, …)
- Edit tests: update or add `test/PantoneSpec.hs` and modify any tests referencing constructors
- Edit (optional): `src/Flag/Render/Prov.hs` to include pantone-derived provenance when building PROV XML

Data format (example) — `data/pantone.json`

{
  "RED-032-C": {
    "rgb": [230, 49, 62],
    "chip": "images/pantone/red-032-c.webp",
    "sourceUrl": "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-red-032-c.webp",
    "sampledAt": "2024-02-13 14:15"
  }
}

CLI spec — `pantone-sample` (UX)
- Usage: `pantone-sample <KEY> <optional URL>`
- Options: none
- Behavior:
  1. Validate KEY grammar (allow A–Z, digits, hyphen/underscore).
  2. Check for `data/images/pantone/<KEY>.webp`. If missing download templated URL (user-supplied or inferred from KEY).
  3. Extract top-left pixel RGB using JuicyPixels with webp package https://hackage.haskell.org/package/webp
  4. Add/update entry in `data/pantone.json` (reserialise file, pretty-print).
  6. Exit with status 0 and write a short summary to stdout.

Implementation notes / dependencies
- Haskell: add `http-conduit` or `wreq` for download; `aeson` for JSON; `JuicyPixels` for PNG decode (already exists in code base), `webp` for JuicyPixels webp support; 

Migration plan (safe, incremental)
1) Phase A — **Add JSON + Lookup + CLI**
   - Implement `data/pantone.json` with current hard-coded entries.
   - Update `Setup.hs` to read `data/pantone.json` at pre-build and generate `src/Flag/GeneratedPantone.hs` (a Haskell Map/Text -> RGB).
   - Add `pantoneToRGB :: Sourced :> es => Text -> Eff es (Colour Double)` that uses the generated module for fast/compiled lookup and returns `reference "RGB Conversion" pantone (sRGB24 r g b)`.
   - Add CLI (tests for CLI recommended).
   - Replace call-sites as a separate migration step (Phase B) and keep a temporary shim for the old sum-type while migrating.
   - Update tests to use string keys (and add tests for generated-module behavior).

3) Phase B — **Cleanup**
   - Remove old constructors and unused imports.

Files to update (code locations) — quick checklist
- src/Flag/Pantone.hs (core change)
- src/Flag/Country/BGD.hs, BWA.hs, FRA.hs, JPN.hs (replace constructors)
- test/* (update tests referencing constructors)
- Setup.hs (generate Haskell module from `data/pantone.json` during pre-build)
- app/Main.hs or package.yaml (add CLI executable)
- src/Flag/Render/Prov.hs (include pantone mapping provenance)

Testing & QA (what to add) 🧪
- Unit: `PantoneSpec` — JSON load, lookup success, lookup missing => error.
- Integration: CLI tests (download mocked or use recorded fixture) verifying `data/pantone.json` update and image file present.
- Regression: update existing flag render/golden tests that expect the same RGB values.
- Manual: run `./bin/serve`, open a flag that uses a Pantone color and inspect PROV UI to confirm chip + color-sample activity appear.

Example commands & dev notes
- Add a chip (download + sample):
  - stack run pantone-sample add RED-032-C
- Print mapping for a key: `pantone-sample show PMSRed032C`


Next steps (concrete tickets) 📝
1. Add `data/pantone.json` with current hard-coded colors (starter entries).
2. Update `Setup.hs` to generate `src/Flag/GeneratedPantone.hs` from `data/pantone.json` at pre-build so lookups are compiled in.
3. Implement `app/PantoneCli.hs` and wire into `package.yaml`.
4. Implement JSON loader + `pantoneToRGB` in `src/Flag/Pantone.hs` (keep shim that uses the generated module).
5. Add tests and CI checks.
6. Migrate call sites and remove sum-type (Phase B).