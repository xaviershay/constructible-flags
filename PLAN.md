# Debug V2 — Interactive Construction Viewer

## Overview

An interactive, single-page debug viewer that replaces the current multi-SVG
debug output. The user can scrub through construction layers with a range
slider, inspect point coordinates on hover, and navigate the construction
tree via a collapsible sidebar.

## Architecture

```
┌──────────────┐        ┌────────────────┐        ┌──────────────────┐
│  Haskell     │──JSON──▶  debug-v2.html │──loads──▶  debug-v2.js    │
│  buildDebugV2│        │  (shell page)  │        │  (React app)     │
└──────────────┘        └────────────────┘        └──────────────────┘
```

### Data flow: Haskell → React

The Haskell build step (`buildDebugV2`) will:

1. Evaluate the construction tree via `evalTree`.
2. Walk the `[NumberedEntry]` tree and, for each leaf, emit the SVG
   fragments for that layer (construction geometry, fill, dots) as **raw
   SVG strings** — one `<g>` per leaf.
3. Also emit a bounding-box / viewBox computed from the union of all
   geometry (so the single SVG container has stable coordinates).
4. Serialize everything into a single **JSON blob** that is inlined into the
   HTML page as:
   ```html
   <script>window.__DEBUG_DATA__ = { ... };</script>
   ```
   This avoids any need for a dev server, fetch calls, or CORS — the page
   is fully self-contained and works from `file://` or a simple static
   server.

### JSON schema

```jsonc
{
  "viewBox": "minX minY width height",  // stable SVG viewBox
  "initialPoints": [                     // the two input points
    { "x": 0, "y": 0, "label": "A" },
    { "x": 1, "y": 0, "label": "B" }
  ],
  "tree": [                              // mirrors NumberedEntry hierarchy
    {
      "type": "group",
      "label": "Fill rectangle",
      "children": [
        {
          "type": "leaf",
          "index": 1,                    // global sequential step number
          "label": "Fill triangle",
          "svg": "<g>...</g>",           // raw SVG for construction geom
          "fillSvg": "<g>...</g>",       // raw SVG for persistent fill
          "points": [                    // points produced by this step
            { "x": 0.5, "y": 0.3, "label": "P1" }
          ],
          "inputPoints": [               // points consumed by this step
            { "x": 0, "y": 0 },
            { "x": 1, "y": 0 }
          ]
        },
        ...
      ]
    },
    {
      "type": "leaf",
      "index": 5,
      ...
    }
  ]
}
```

### Why inline JSON + raw SVG strings?

- **Self-contained**: one HTML file, one JS file, one JSON blob — works
  offline, from `file://`, no build step on the JS side.
- **No diagrams dependency in JS**: Haskell already knows how to render
  construction geometry to SVG via `diagrams-svg`. We render each layer to
  an SVG string fragment in Haskell, so the JS side just toggles `<g>`
  visibility — it never needs to understand the geometry.
- **Consistent rendering**: the exact same rendering code that produces the
  existing debug SVGs is reused; we just capture the SVG XML instead of
  writing separate files.

## Haskell changes

### New function in `Flag.Render.Debug`

```haskell
buildDebugV2 :: Flag (Sourced : '[]) -> IO ()
```

This function will:

1. Evaluate the tree (`evalTree`).
2. Number it (`numberTree`).
3. For each leaf, render its construction-geometry diagram and its
   persistent-fill diagram to SVG string fragments using
   `Diagrams.Backend.SVG.renderDia` with `SVGOptions` configured to
   produce a raw `<g>` (no outer `<svg>` wrapper).
4. Compute a single bounding box (the union of all leaf diagrams) for the
   shared `viewBox`.
5. Build the JSON data structure (using plain string concatenation or
   `aeson` — TBD based on whether `aeson` is already a dependency).
6. Write `out/debug-v2/index.html` (thin shell that loads the JS and
   inlines the JSON).
7. Copy or write `out/debug-v2/debug-v2.js` (the React app, served from a
   static file in `sources/`).

### Rendering individual layers to SVG strings

`diagrams-svg` renders to `Element` (from `svg-builder` / `lucid`). We can
render each layer's `Diagram B` through `renderDia SVG options diagram` and
then serialize the resulting `Element` to a `Text`/`String`. We'll strip
the outer `<svg>` wrapper and keep only the inner `<g>` content.

Alternatively we could render raw SVG path strings directly from the
geometry data (lines, circles, polygons) without going through diagrams.
This is more work but gives us full control and avoids coordinate-system
surprises. **Decision: start with diagrams-based rendering; switch to
manual SVG only if coordinate issues arise.**

### Coordinate system

The construction works in "math" coordinates (y-up). SVG uses y-down. The
existing diagrams rendering handles this via transforms. For the V2 viewer
we need all layers in a single `<svg>` with a shared coordinate system.

Approach: render all layers in a single diagrams composition (like the
existing `buildDebug` does for the `frameDia`), extract the overall
transform/viewBox, and then extract each layer's `<g>` content within that
coordinate frame.

Actually, the simplest approach is: **render each layer to SVG manually**
(bypassing diagrams entirely for the V2 output), since the geometry types
are simple (lines, circles, triangles/polygons, dots). This gives us
precise control over coordinates and avoids the diagrams y-flip. The
Haskell side emits SVG path data directly from `ConstructionLayer` values.

**Decision: manual SVG generation from `ConstructionLayer` data, no
diagrams dependency for V2.**

## React application (`debug-v2.js`)

Loaded from a CDN (React + ReactDOM via a `<script>` tag) — no npm/webpack
build step needed. The JS file is a plain script using `React.createElement`
or a JSX-like tagged template library (htm).

### Using htm + preact (lightweight alternative)

To avoid a build step while still writing JSX-like syntax, use
[htm](https://github.com/developit/htm) with preact (3KB). Loaded from CDN:

```html
<script type="module">
  import { h, render } from 'https://esm.sh/preact';
  import htm from 'https://esm.sh/htm';
  const html = htm.bind(h);
  // ...
</script>
```

**Decision: use htm + preact via ESM CDN imports. The JS lives in
`sources/debug-v2.js` and is copied to `out/debug-v2/debug-v2.js` at
build time.**

## UI layout

```
┌─────────────────────────────────────────────────────────┐
│  Construction Debug Viewer                              │
├──────────────────────────────┬──────────────────────────┤
│                              │  Steps                   │
│                              │  ┌─ Fill rectangle       │
│       SVG viewport           │  │  ├ 1. Fill triangle   │
│    (single <svg>, each       │  │  └ 2. Fill triangle   │
│     layer in a <g>)          │  ┌─ Disc (collapsed)     │
│                              │  3. Intersect L–L        │
│                              │  ...                     │
│                              │                          │
├──────────────────────────────┴──────────────────────────┤
│  ◄──────────[====●====]──────────►                      │
│  Range slider (lo ● hi)                                 │
└─────────────────────────────────────────────────────────┘
```

### Components

1. **`App`** — top-level state: `loLayer`, `hiLayer`, `expandedGroups`.
2. **`SvgViewer`** — renders a single `<svg>` with one `<g>` per leaf.
   Each `<g>` has `visibility: hidden | visible` based on whether its
   index is in `[loLayer, hiLayer]`. Points have `<title>` or a hover
   handler showing coordinates.
3. **`RangeSlider`** — a custom dual-thumb + midpoint slider.
   - Two endpoints (lo, hi) that can be dragged independently.
   - A midpoint thumb that drags both lo and hi together, maintaining
     the current window width.
   - The range is `[0, totalLeaves]` where 0 = initial points only.
4. **`StepTree`** — recursive tree of construction steps.
   - Groups are collapsible (click to expand/collapse).
   - The "active" range `[loLayer, hiLayer]` highlights which steps are
     currently visible.
   - The midpoint layer is scrolled into view and specially highlighted.
   - When a group is **collapsed**, all its children count as a single
     "layer" for slider purposes. When **expanded**, each child is an
     independent layer.

### Slider ↔ tree interaction (collapsed groups)

This is the trickiest part. The slider operates over a **virtual layer
count** that depends on which groups are expanded:

- A collapsed group with 5 leaves counts as **1 virtual layer**.
- An expanded group with 5 leaves counts as **5 virtual layers**.

We maintain a mapping `virtualIndex → Set<leafIndex>` that is recomputed
whenever the expanded-groups set changes. The slider range adjusts
dynamically.

```
function buildVirtualMapping(tree, expandedGroups):
  virtualLayers = []
  for node in tree:
    if node.type == "leaf":
      virtualLayers.push([node.index])
    else if node.label in expandedGroups:
      virtualLayers.push(...recurse(node.children))
    else:
      // collapsed group: all leaves as one virtual layer
      virtualLayers.push(allLeafIndices(node))
  return virtualLayers
```

When the slider is at virtual position `v`, the visible leaf indices are
the union of `virtualLayers[lo..hi]`.

### Point coordinate hover

Each point in the SVG data includes `x, y` coordinates. In the SVG, points
are rendered as small circles. On hover, a tooltip (or a fixed-position
readout) displays the coordinates.

Implementation: each dot `<circle>` gets an `onMouseEnter` handler that
sets a state variable `hoveredPoint = {x, y, label}`, rendered as a
floating div or an SVG `<text>` element near the cursor.

### Coordinate display

Points will be shown as exact decimal values. If we want to show "nice"
forms (like fractions), that's a future enhancement — for now, display
raw doubles rounded to 4 decimal places.

## File inventory

| File | Language | Purpose |
|------|----------|---------|
| `src/Flag/Render/Debug.hs` | Haskell | Add `buildDebugV2` function |
| `sources/debug-v2.js` | JavaScript | React (preact+htm) application |
| `out/debug-v2/index.html` | HTML | Generated shell page (output) |
| `out/debug-v2/debug-v2.js` | JS | Copied from sources/ (output) |

## Implementation plan

### Phase 1: Haskell JSON export

1. Add a function `layerToSvgFragments :: ConstructionLayer -> (String, String)`
   that converts a layer to raw SVG strings (construction geom + fill).
   - Lines → `<line>` elements with dashed stroke
   - Circles → `<circle>` elements with dashed stroke
   - Triangles → `<polygon>` elements with fill
   - Dots → `<circle>` elements (small, filled)
2. Add `entryToJson :: NumberedEntry -> String` to serialize the tree.
3. Add `buildDebugV2` that evaluates the tree, serializes to JSON, and
   writes the output files.
4. Wire into `Main.hs`.

### Phase 2: React viewer (basic)

1. Create `sources/debug-v2.js` with the preact+htm app.
2. Parse `window.__DEBUG_DATA__`.
3. Render the SVG with all layers, all visible.
4. Add the range slider (simple HTML `<input type="range">` first, dual-
   thumb later).
5. Toggle `<g>` visibility based on slider position.

### Phase 3: Tree sidebar

1. Render the step tree with collapsible groups.
2. Highlight the active range.
3. Sync the midpoint to the tree scroll position.

### Phase 4: Collapsed-group virtual mapping

1. Implement the virtual layer mapping.
2. Re-map slider positions when groups are expanded/collapsed.
3. Adjust slider range dynamically.

### Phase 5: Point hover & polish

1. Add hover handlers to dot elements.
2. Display coordinate tooltip.
3. CSS polish, responsive layout.

## Open questions

- **Should the SVG use y-up (math) or y-down (SVG native)?** Using y-down
  with a `transform="scale(1,-1)"` on the root `<g>` is simplest. Hover
  coordinates should display the math-convention values (y-up). Decision:
  use SVG-native y-down (negate y values in Haskell when emitting SVG),
  display original math coordinates in tooltips.

- **Should `aeson` be added as a dependency?** The JSON is simple enough to
  emit via string concatenation. Avoid the dependency for now.

- **CDN vs vendored JS?** Start with CDN (esm.sh) for preact+htm. If
  offline use is important, vendor them later.
