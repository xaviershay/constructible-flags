# Plan: Replace Diagrams with svg-builder

## Overview

The project currently uses `diagrams-lib` and `diagrams-svg` solely to produce SVG
output.  The Diagrams library is large and the way it is used here — filling
polygons, drawing circles, dashing lines — maps directly to SVG primitives.
Removing it eliminates a heavy dependency and gives us direct control over the
output.

The replacement is `svg-builder` (Hackage), which provides a lightweight DSL for
constructing SVG elements as a `Monoid`-based `Element` type.

**Recommended approach: direct replacement, no abstraction layer.**  A swappable
backend typeclass is achievable (see end of document) but is not recommended —
this project has one output format and no identified need for multiple backends.
Introducing an abstraction for hypothetical future use adds complexity without
present benefit.

---

## Diagrams surface in the codebase

All Diagrams usage is confined to three rendering modules:

| File | Diagrams symbols used |
|---|---|
| `src/Flag/Render/Diagram.hs` | `Diagrams.Prelude` (almost everything), `Diagrams.Backend.SVG` (`B`) |
| `src/Flag/Render/SVGOverlay.hs` | `renderSVG`, `mkWidth`, `width`, `boundingBox`, `getCorners`, `unp2` |
| `src/Flag/Render/Debug.hs` | `Diagram`, `pad`, `phantom`, `(#)`, `mkWidth`, `B`, `renderSVG` |

No country flag module or construction module imports Diagrams.

---

## Dependency changes

In `package.yaml` (and the generated `.cabal`):

- **Remove**: `diagrams-lib`, `diagrams-svg`
- **Add**: `svg-builder`

---

## Coordinate system

Diagrams uses a y-up (mathematical) coordinate system; SVG uses y-down.
`diagrams-svg` handles this by emitting a wrapping `<g transform="translate(0,H)
scale(1,-1)">` around all shapes.  `SVGOverlay.injectOverlays` already compensates
for this flip manually when positioning overlay elements.

**Decision**: continue the same approach — wrap all rendered shapes in a
`<g transform="translate(0,H) scale(1,-1)">` — so that internal coordinates
remain y-up throughout and the overlay injection logic is unchanged.

The SVG root element will use `viewBox="minX minY_svg W H"` where `minY_svg = 0`
and the height is derived from the bounding box (see below).

---

## Bounding box

Diagrams computes bounding boxes automatically.  After removal we need to derive
them from the `Drawing` tree.  Add a new function (in
`Flag.Render.SVGOverlay`):

```haskell
drawingBounds :: Drawing -> Maybe (Double, Double, Double, Double)
-- returns Just (minX, minY, maxX, maxY) or Nothing for EmptyDrawing
```

Rules per constructor:

| Constructor | Bounds |
|---|---|
| `EmptyDrawing` | `Nothing` |
| `Overlay a b` | union of `drawingBounds a` and `drawingBounds b` |
| `DrawTriangle _ p1 p2 p3` | min/max of the three converted `(Double,Double)` pairs |
| `DrawPath _ pts` | min/max over all points |
| `DrawCircle _ (cx,cy) r` | `(cx-r, cy-r, cx+r, cy+r)` |
| `DrawCrescent _ oc or' _ _` | outer circle's bounding box `(ocx-or', ocy-or', ocx+or', ocy+or')` |
| `DrawSVGOverlay _ center edge` | circle bounding box with radius = `dist(center, edge)` |

This replaces the `width diagram`, `boundingBox diagram`, `getCorners`, and
`unp2` calls in `SVGOverlay.renderOptimizedDrawingToSVG`.

---

## SVG file writing

Replace `renderSVG outPath (mkWidth svgOutputWidth) diagram` with a small helper:

```haskell
writeSVG
    :: FilePath
    -> Double            -- output width in pixels
    -> Double            -- output height in pixels (derived from aspect ratio)
    -> Double            -- viewBox minX
    -> Double            -- viewBox minY  (in y-up coords = -(maxY))
    -> Double            -- viewBox width
    -> Double            -- viewBox height
    -> Element           -- svg-builder element
    -> IO ()
```

The function produces:

```xml
<svg xmlns="http://www.w3.org/2000/svg"
     width="W" height="H"
     viewBox="minX minY vbW vbH">
  <g transform="translate(0,vbH) scale(1,-1)">
    ELEMENTS
  </g>
</svg>
```

The `width` and `height` pixel dimensions preserve the aspect ratio of the
viewBox scaled to `svgOutputWidth`.

---

## Rewrite `Flag.Render.Diagram`

The module changes from returning `Diagram B` to returning `Element`
(from `Graphics.Svg`).  The public API remains the same; only the return types
and internal implementation change.

### Shape mapping

**Polygon (triangle / path)**

```xml
<polygon points="x1,y1 x2,y2 ..." fill="COLOR" fill-opacity="1" stroke="COLOR" stroke-width="0.02"/>
```

Use `polygon_` with a `points_` attribute built from the coordinate list.

**Circle**

```xml
<circle cx="cx" cy="cy" r="r" fill="COLOR" fill-opacity="1" stroke="none"/>
```

Use `circle_` with `cx_`, `cy_`, `r_`, `fill_`, `fillOpacity_`, `strokeWidth_
"0"`.

**Crescent (EvenOdd two-circle)**

Diagrams uses two `Path` objects composed with `EvenOdd` fill.  In SVG this is a
`<path>` whose `d` attribute traces both circles as arcs, with
`fill-rule="evenodd"`:

```
M (ocx - or') ocy
a or' or' 0 1 0 (2*or') 0
a or' or' 0 1 0 (-2*or') 0
M (icx - ir) icy
a ir ir 0 1 0 (2*ir) 0
a ir ir 0 1 0 (-2*ir) 0
Z
```

This traces each circle as two semicircular arcs, producing a single compound
path with `fill-rule="evenodd"` to cut out the inner circle.

**Dashed construction line**

```xml
<line x1=".." y1=".." x2=".." y2=".."
      stroke="grey" stroke-width="0.02" stroke-dasharray="0.05,0.05"/>
```

**Dashed construction circle**

```xml
<circle cx=".." cy=".." r=".."
        fill="none" stroke="grey" stroke-width="0.02"
        stroke-dasharray="0.08,0.05"/>
```

**Dot**

```xml
<circle cx=".." cy=".." r="0.04" fill="black" stroke="none"/>
```

**Overlay / EmptyDrawing**

`Overlay a b` → `drawingToElement b <> drawingToElement a` (svg-builder
`Element` is a `Monoid`; later elements render on top, matching Diagrams
semantics).

`EmptyDrawing` → `mempty`.

`DrawSVGOverlay` → `mempty` (overlay content is injected in post-processing as
before).

### Colour rendering

`Data.Colour.SRGB.toSRGB` gives `RGB Double` with components in `[0,1]`.
Format as `"rgb(r%,g%,b%)"` or as a hex string `#rrggbb`.

Fill opacity is applied via `fill-opacity` attribute (was `withOpacity` /
`fcA`).

---

## Rewrite `Flag.Render.SVGOverlay`

### `renderOptimizedDrawingToSVG`

Replace the Diagrams pipeline:

```
OLD:
  diagram   = drawingToDiagram drawing          -- Diagram B
  diagramW  = width diagram
  bb        = boundingBox diagram
  ...
  renderSVG outPath (mkWidth w) diagram

NEW:
  (minX, minY, maxX, maxY) = fromMaybe (0,0,1,1) (drawingBounds drawing)
  diagramW  = maxX - minX
  diagramH  = maxY - minY
  svgH      = svgOutputWidth * diagramH / diagramW
  element   = drawingToElement drawing
  writeSVG outPath svgOutputWidth svgH minX minY diagramW diagramH element
```

The `(bbMinX, bbMinY, bbMaxY)` values passed to `injectOverlays` are now taken
directly from `drawingBounds` — no change to the overlay injection logic itself.

### `injectOverlays`

No change needed.  The coordinate math already handles the y-flip manually and
is independent of Diagrams.

### Remove imports

Drop `Diagrams.Backend.SVG` and `Diagrams.Prelude` imports entirely.

---

## Rewrite `Flag.Render.Debug`

### Uniform bounding box (replacing `phantom`)

`Debug.buildDebug` uses `phantom combined` so that every step SVG shares the
same canvas.  Replace with:

```haskell
let allBounds = mapMaybe (layerBounds . snd) leaves
    unionBB   = foldl1 unionBounds allBounds  -- (minX,minY,maxX,maxY)
    padded    = applyPadding 1.3 unionBB
```

Write a `layerBounds :: ConstructionLayer -> Maybe (Double, Double, Double, Double)`
helper (analogous to `drawingBounds` above but for `ConstructionLayer`) and an
`applyPadding` that expands the box by a factor.  Pass `padded` as the viewBox to
every step's `writeSVG` call.

### `pad` / `#`

These are gone.  Padding is baked into the viewBox computed above.

### `mconcat` over `Diagram B`

Replace with `mconcat :: [Element] -> Element` — `Element` is already a
`Monoid`.

### `renderSVG`

Replace every call site with `writeSVG`.

---

## Swappable backend (not recommended)

If a swappable backend were ever needed, the approach would be:

1. **Factor `drawingBounds`/`layerBounds` into `Flag.Render.Bounds`** — they are
   already backend-independent (pure `Drawing`/`ConstructionLayer` traversals).
2. **Define a `RenderBackend` typeclass** in `Flag.Render.Backend` with an
   associated `Canvas b` type (constrained to `Monoid`) and five methods:
   `drawingToCanvas`, `layerGeomToCanvas`, `layerFillToCanvas`, `dotsToCanvas`,
   `writeCanvas`.  The pre-computed `BoundingBox` is passed to `writeCanvas` so
   backends only serialise, never measure.
3. **Parameterize `renderOptimizedDrawingToSVG` and `buildDebug`** on
   `RenderBackend b =>`.
4. **Rename `Flag.Render.Diagram`** to `Flag.Render.DiagramsBackend` and create
   `Flag.Render.SVGBuilderBackend` as the new instance.

The prerequisite is a working svg-builder implementation — the typeclass can be
layered on top once both backends exist.

---

## Checklist

- [ ] Add `svg-builder` to `package.yaml` dependencies
- [ ] Remove `diagrams-lib` and `diagrams-svg` from `package.yaml` dependencies
- [ ] Add `drawingBounds` (and `layerBounds`) helper
- [ ] Add `writeSVG` helper
- [ ] Rewrite `Flag.Render.Diagram`: `Drawing` / `ConstructionLayer` → `Element`
- [ ] Rewrite `Flag.Render.SVGOverlay`: replace all Diagrams calls
- [ ] Rewrite `Flag.Render.Debug`: replace `phantom`, `pad`, `renderSVG`
- [ ] Run `./bin/test` and fix golden image diffs (visual output will change
  slightly due to SVG renderer differences; update golden images once the output
  looks correct)
- [ ] Delete `constructible-flags.cabal` and regenerate with `stack build` (or
  update manually if preferred)
