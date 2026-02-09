# Exact Arithmetic — Radical Number Type

## Motivation

All points produced by straightedge-and-compass constructions are
**constructible numbers** — they lie in a tower of quadratic extensions
over the rationals.  Currently the project represents points as
`(Double, Double)`, which:

1. **Loses information** — we can't show the user that a coordinate is
   $\frac{3}{5}$ or $\frac{\sqrt{3}}{2}$; we can only show `0.866025…`.
2. **Accumulates error** — chaining many intersection steps compounds
   floating-point drift.
3. **Can't display exact values** — the debug viewer could show beautiful
   KaTeX-rendered expressions like $\frac{1 + \sqrt{5}}{2}$, but has
   nothing to render.

The data type we introduce supports **arbitrary nth roots** (not just
square roots), so it can later be extended to constructions involving
neusis, origami, or other tools that produce cube roots, fifth roots, etc.
For now, only square roots arise from compass-and-straightedge, but the
representation is ready for the future.

## Mathematical background

A constructible number can always be written as a finite expression
involving:

- Rational numbers ($\mathbb{Q}$)
- Addition, subtraction, multiplication, division
- Square roots ($\sqrt{\cdot}$)

This forms a **field extension tower**:
$$\mathbb{Q} = F_0 \subset F_1 \subset \cdots \subset F_n$$
where each $F_{i+1} = F_i(\sqrt{d_i})$ for some $d_i \in F_i$.

More generally, numbers expressible with **nth roots** (radicals) live
in towers of the form $F_{i+1} = F_i(\sqrt[n_i]{d_i})$.  Cube roots
arise from neusis and origami constructions; higher roots from other
tools.  Our data type will support the general $a + b\sqrt[n]{r}$ form
so that future construction methods can be added without changing the
numeric representation.

Currently, only square roots ($n = 2$) arise from compass-and-straightedge
constructions, so arithmetic is fully implemented for that case first.
Nth-root arithmetic for $n > 2$ will be added when the corresponding
construction primitives are introduced.

## Data type design

### `Radical` — the core type

```haskell
-- | Exact representation of a number built from rationals and nth roots.
--
-- A value is either a rational, or lives in a radical extension:
-- a + b · r^(1/n), where a, b, r are themselves Radical values and
-- n is the root index (2 for square root, 3 for cube root, etc.).
data Radical
  = Rational !Rational              -- ^ An exact rational value
  | Ext !Radical                    -- ^ a  (rational part)
        !Radical                    -- ^ b  (coefficient of the radical)
        !Radical                    -- ^ r  (the radicand, canonical form)
        !Int                        -- ^ n  (root index: 2 = √, 3 = ∛, etc.)
  deriving (Eq, Ord, Show)
```

The `Ext a b r n` constructor represents $a + b\sqrt[n]{r}$.  By convention:

- $n \geq 2$ always.
- $r$ is always in **canonical form** (see Normalisation below).
- When $b = 0$, we collapse back to `Rational` or a simpler `Ext`.
- `Rational 0` is the zero element; `Rational 1` is the unit.

> **Why not just square roots?**  Supporting general $n$ from the start
> means the data type, serialisation, KaTeX rendering, and `toDouble`
> conversion all work unchanged when cube-root constructions (neusis,
> origami) are added later.  Arithmetic for $n = 2$ is fully implemented
> first; higher-$n$ arithmetic is stubbed with `error` until needed.

### `ExactPoint` — replaces `Point`

```haskell
type ExactPoint = (Radical, Radical)
```

The current `type Point = (Double, Double)` becomes a type alias that can
be toggled:

```haskell
-- The canonical internal representation is now exact:
type Point = ExactPoint
```

All construction geometry (intersections, midpoints, etc.) will operate on
`Radical` values.  Conversion to `Double` happens **only** at the
rendering boundary (SVG generation).

## Normalisation

Every operation that produces a `Radical` must normalise its result
via `normalize :: Radical -> Radical`:

1. **Collapse zero extensions**: if $b = 0$ in $a + b\sqrt[n]{r}$, return $a$.
2. **Collapse perfect-power radicands**: if $r$ is a perfect $n$th power
   rational $r = \frac{p^n}{q^n}$, compute $a + b \cdot \frac{p}{q}$ as a
   rational (or simpler radical).
3. **Canonical radicand**: factor out perfect $n$th-power components from
   $r$.  E.g. $\sqrt{12} \to 2\sqrt{3}$ (for $n=2$: `Ext 0 1 12 2` →
   `Ext 0 2 3 2`).  For $n=3$: $\sqrt[3]{24} \to 2\sqrt[3]{3}$.
4. **Sign of radicand**: for even $n$, $r$ must be positive (complex roots
   are an error).  For odd $n$, negative $r$ is allowed
   ($\sqrt[3]{-8} = -2$).
5. **Recursive normalisation**: normalise $a$, $b$, and $r$ before
   applying the above rules.

### Perfect nth-power detection for rationals

```haskell
-- | Integer nth root, if n is a perfect nth power.
inroot :: Int -> Integer -> Maybe Integer
inroot 2 n = isqrt n
inroot k n | n < 0 && even k = Nothing
           | n < 0     = negate <$> inroot k (negate n)
           | n == 0    = Just 0
           | otherwise = let s = round ((fromIntegral n :: Double) ** (1 / fromIntegral k))
                         in if s^k == n then Just s else Nothing

isqrt :: Integer -> Maybe Integer  -- integer square root, if perfect square
isqrt n | n < 0     = Nothing
        | n == 0    = Just 0
        | otherwise = let s = floor (sqrt (fromIntegral n :: Double))
                      in if s*s == n then Just s else Nothing

-- | Factor n = d^k · m where m is k-th-power-free.
nthPowerFree :: Int -> Integer -> (Integer, Integer)  -- (d, m)
nthPowerFree 2 n = squareFree n
nthPowerFree k n = ...  -- generalised factoring

-- | Factor n = d² · m where m is square-free.
squareFree :: Integer -> (Integer, Integer)  -- (d, m)
```

## Arithmetic operations

### Field operations (implemented for $n = 2$)

All of these must normalise their result.  For $n = 2$ (square roots),
the extension $a + b\sqrt{r}$ forms a **field**, so all operations are
closed.

| Operation | Formula |
|-----------|---------|
| **Add** | $(a_1 + b_1\sqrt{r}) + (a_2 + b_2\sqrt{r}) = (a_1 + a_2) + (b_1 + b_2)\sqrt{r}$ |
| **Sub** | analogous |
| **Mul** | $(a_1 + b_1\sqrt{r})(a_2 + b_2\sqrt{r}) = (a_1 a_2 + b_1 b_2 r) + (a_1 b_2 + a_2 b_1)\sqrt{r}$ |
| **Div** | Rationalise denominator: multiply num & denom by conjugate $(a - b\sqrt{r})$ |
| **Sqrt** | See below |
| **Negate** | $-(a + b\sqrt{r}) = (-a) + (-b)\sqrt{r}$ |
| **Abs** | Compare to zero, negate if negative |
| **Signum** | $-1$, $0$, or $1$ |
| **fromRational** | `Rational r` |
| **fromInteger** | `Rational (fromInteger n)` |

**Mixed radicand arithmetic**: when combining $a_1 + b_1\sqrt{r_1}$ with
$a_2 + b_2\sqrt{r_2}$ where $r_1 \neq r_2$, we must introduce a new
extension level.  Practically, nest one inside the other:

$$(\text{result lives in } \mathbb{Q}(\sqrt{r_1}, \sqrt{r_2}))$$

Represent as `Ext (Ext a1 b2 r2 2) (Ext b1 0 r2_unused 2) r1 2` — i.e.
the coefficients $a$ and $b$ of the outer $\sqrt{r_1}$ are themselves
`Radical` values that may contain $\sqrt{r_2}$.

### Arithmetic for $n > 2$ (future)

For $n > 2$, the extension $a + b\sqrt[n]{r}$ does **not** form a field
(you need all $n$ powers of the radical: $1, \sqrt[n]{r}, \sqrt[n]{r}^2,
\ldots, \sqrt[n]{r}^{n-1}$).  Full arithmetic for higher roots will be
implemented when the corresponding construction primitives are added.  In
the meantime, operations on `Ext _ _ _ n` where $n > 2$ will raise an
error with a descriptive message.

### Square root

The `sqrt` of a radical is the key operation for compass constructions
(circle intersection involves $\sqrt{\text{discriminant}}$).

For a rational $\frac{p}{q}$:
- If $p \cdot q$ is a perfect square, return the exact rational root.
- Otherwise, return `Ext 0 1 (p/q) 2` after making the radicand
  square-free.

For a nested `Ext a b r 2`: use the identity (when it simplifies):
$$\sqrt{a + b\sqrt{r}} = \sqrt{\frac{a + d}{2}} + \sqrt{\frac{a - d}{2}}$$
where $d = \sqrt{a^2 - b^2 r}$, **if** $a^2 - b^2 r$ is a perfect square
in the current field.  Otherwise, introduce a new extension level.

### Nth root (future)

`nthRootC :: Int -> Radical -> Radical` will generalise `sqrtC` for
arbitrary $n$.  For now, only `sqrtC` (i.e. $n = 2$) is implemented;
`nthRootC` for $n > 2$ checks for perfect $n$th powers among rationals
and otherwise returns `Ext 0 1 r n`.

## Type class instances

```haskell
instance Num Radical where
  (+)         = addC
  (-)         = subC
  (*)         = mulC
  negate      = negateC
  abs         = absC
  signum      = signumC
  fromInteger = Rational . fromInteger

instance Fractional Radical where
  (/)          = divC
  fromRational = Rational

instance Floating Radical where
  sqrt = sqrtC        -- i.e. nthRootC 2
  pi   = error "pi is not algebraic"
  -- Other Floating methods not needed; error stubs.
```

Additionally, a standalone function for general nth roots:

```haskell
nthRootC :: Int -> Radical -> Radical
nthRootC 2 = sqrtC
nthRootC n = \case
  Rational r -> ...  -- check for perfect nth power, else Ext 0 1 r n
  _          -> error $ "nthRootC: n=" ++ show n ++ " not yet implemented for nested radicals"
```

## Conversion to Double

```haskell
toDouble :: Radical -> Double
toDouble (Rational r)    = fromRational r
toDouble (Ext a b r n)   = toDouble a + toDouble b * (toDouble r ** (1 / fromIntegral n))
```

This works for any $n$ — no special-casing needed.  Used **only** at the
SVG/diagrams rendering boundary.

## Conversion to KaTeX string

```haskell
toKaTeX :: Radical -> String
toKaTeX (Rational r)
  | denominator r == 1 = show (numerator r)
  | otherwise          = "\\frac{" ++ show (numerator r) ++ "}{"
                         ++ show (denominator r) ++ "}"
toKaTeX (Ext a b r n)
  -- a + b · root_n(r), with special cases for a=0, b=1, b=-1, etc.
  -- For n=2: \sqrt{r}
  -- For n>2: \sqrt[n]{r}
```

The radical symbol adapts to the root index:
- $n = 2$: `\sqrt{r}` → $\sqrt{r}$
- $n = 3$: `\sqrt[3]{r}` → $\sqrt[3]{r}$
- $n = k$: `\sqrt[k]{r}` → $\sqrt[k]{r}$

Examples:
| Value | KaTeX output |
|-------|-----------|
| `Rational (1 % 2)` | $\frac{1}{2}$ |
| `Ext 0 1 3 2` | $\sqrt{3}$ |
| `Ext (1/2) (1/2) 3 2` | $\frac{1}{2} + \frac{1}{2}\sqrt{3}$ |
| `Ext 0 (Rational (3%5)) 2 2` | $\frac{3}{5}\sqrt{2}$ |
| `Ext 0 1 2 3` | $\sqrt[3]{2}$ |
| `Ext 1 1 5 3` | $1 + \sqrt[3]{5}$ |

## Comparison / ordering

Comparing radical values for equality and ordering is needed for
normalisation and geometry (e.g. checking if a discriminant is zero).

- **Equality**: structural equality after normalisation.
- **Ordering**: convert to `Double` for approximate comparison, then verify
  algebraically if close to zero.  For $n = 2$, compute the sign of
  $a + b\sqrt{r}$ by case analysis on signs of $a$, $b$, and comparison
  of $a^2$ vs $b^2 r$.  For general $n$, fall back to `Double`
  comparison (sufficient for normalisation decisions).

## Geometry updates

### `Flag.Construction.Geometry`

Replace all `Double` arithmetic with `Radical` arithmetic:

```haskell
evalIntersectLL' :: ((Point, Point), (Point, Point)) -> Point
-- Same formulas, but all operations are on Radical values.
-- No sqrt needed — line-line intersection is rational in the inputs.

evalIntersectLC' :: ((Point, Point), (Point, Point)) -> (Point, Point)
-- Uses sqrtC for the discriminant — this is where Ext values are born
-- (always with n=2, since these are compass constructions).

evalIntersectCC' :: ((Point, Point), (Point, Point)) -> (Point, Point)
-- Also uses sqrtC (n=2).

dist :: Point -> Point -> Radical
-- Returns sqrt((x2-x1)² + (y2-y1)²) — exact, n=2.
```

### `Flag.Construction.Types`

```haskell
type Point = (Radical, Radical)
```

The `Drawing` type gains `Radical` coordinates:

```haskell
data Drawing
  = DrawTriangle (Colour Double) Point Point Point
  | DrawPath (Colour Double) [Point]
  | DrawCircle (Colour Double) Point Radical  -- radius is Radical
  | Overlay Drawing Drawing
  | EmptyDrawing
```

### `Flag.Construction.Layers`

`ConstructionLayer` stores `Point` (now exact).  `pointDist` returns
`Radical`.

### `Flag.Construction.Tree`

No structural changes — just uses the updated `Point` type.

### `Flag.Construction.Interpreter`

`eval` works with exact `Point`.  The `DrawCircle` case calls `dist`
which now returns `Radical`.

## Rendering updates

### `Flag.Render.Diagram`

Convert `Point` to `(Double, Double)` at the boundary:

```haskell
toDoublePoint :: Point -> (Double, Double)
toDoublePoint (x, y) = (toDouble x, toDouble y)
```

All `p2`, `r2`, etc. calls use `toDoublePoint`.  Circle radius uses
`toDouble`.  This works regardless of the root index $n$ inside the
`Radical` values, since `toDouble` handles all $n$.

### `Flag.Render.DebugV2`

**JSON output**: add exact coordinate data alongside numeric values.

```jsonc
{
  "points": [
    {
      "x": 0.5,
      "y": 0.866025,
      "label": "P1",
      "exactX": "\\frac{1}{2}",
      "exactY": "\\frac{\\sqrt{3}}{2}"
    }
  ]
}
```

**HTML shell**: include the KaTeX CSS and JS from CDN (already done in
`Html.hs` for the index page — reuse the same CDN links).

**JS viewer**: render exact coordinates using `katex.renderToString()`
in the point-hover tooltip and the sidebar point list.

### `Flag.Render.Html`

The index page already loads KaTeX.  No changes needed unless we want to
show exact coordinates in the summary table.

## Module plan

### New module: `Flag.Construction.Radical`

```
src/Flag/Construction/Radical.hs
```

Exports:

```haskell
module Flag.Construction.Radical
    ( Radical(..)
    , normalize
    , toDouble
    , toKaTeX
    , sqrtC
    , nthRootC
    , isZero
    , isRational
    , toRational'
    ) where
```

### New test module: `test/RadicalSpec.hs`

```
test/RadicalSpec.hs
```

## Test suite

The new test file `test/RadicalSpec.hs` should cover:

### 1. Normalisation

- `Ext a 0 r 2` collapses to `a`
- `Ext 0 1 4 2` collapses to `Rational 2`
- `Ext 0 1 12 2` normalises to `Ext 0 2 3 2`
- `Rational 0 + Rational 0` = `Rational 0`
- Deeply nested: `Ext (Ext 0 0 2 2) 1 3 2` collapses to `Ext 0 1 3 2`
- Cube root: `Ext 0 1 8 3` collapses to `Rational 2`
- Cube root canonical: `Ext 0 1 24 3` normalises to `Ext 0 2 3 3`

### 2. Arithmetic (n=2)

- Rational + Rational = Rational
- `(1 + √2) + (3 - √2)` = `Rational 4`
- `(1 + √2) * (1 - √2)` = `Rational (-1)` (conjugate product)
- `(√2)²` = `Rational 2`
- `1 / √2` = `Ext 0 (1/2) 2 2` (i.e. $\frac{\sqrt{2}}{2}$)
- Associativity: `(a + b) + c ≡ a + (b + c)` for sample values
- Distributivity: `a * (b + c) ≡ a*b + a*c`

### 3. Square root

- `sqrtC (Rational 4)` = `Rational 2`
- `sqrtC (Rational 2)` = `Ext 0 1 2 2`
- `sqrtC (Rational (9/4))` = `Rational (3/2)`
- `sqrtC (Rational (1/2))` normalised correctly

### 4. Nth root

- `nthRootC 3 (Rational 27)` = `Rational 3`
- `nthRootC 3 (Rational 2)` = `Ext 0 1 2 3`
- `nthRootC 4 (Rational 16)` = `Rational 2`
- `nthRootC 3 (Rational (-8))` = `Rational (-2)`

### 5. Conversion to Double

- `toDouble (Rational (1/3))` ≈ `0.33333…`
- `toDouble (Ext 0 1 2 2)` ≈ `1.41421…`
- `toDouble (Ext 1 1 2 2)` ≈ `2.41421…`
- `toDouble (Ext 0 1 2 3)` ≈ `1.25992…` (cube root of 2)

### 6. KaTeX rendering

- `toKaTeX (Rational 3)` = `"3"`
- `toKaTeX (Rational (1/2))` = `"\\frac{1}{2}"`
- `toKaTeX (Ext 0 1 3 2)` = `"\\sqrt{3}"`
- `toKaTeX (Ext (1/2) (1/2) 3 2)` = `"\\frac{1}{2} + \\frac{1}{2}\\sqrt{3}"`
- `toKaTeX (Ext 0 (-1) 5 2)` = `"-\\sqrt{5}"`
- `toKaTeX (Ext 0 1 2 3)` = `"\\sqrt[3]{2}"`
- `toKaTeX (Ext 1 1 5 3)` = `"1 + \\sqrt[3]{5}"`

### 7. Geometry integration

- Line-line intersection of axis-aligned lines produces rational points
- Circle-line intersection produces expected radical values
  (e.g. unit circle intersected with $x = \frac{1}{2}$ gives
  $y = \pm\frac{\sqrt{3}}{2}$)
- `dist (0,0) (1,1)` = `Ext 0 1 2 2` (i.e. $\sqrt{2}$)
- Full France flag construction produces all rational coordinates
- Full Japan flag construction produces expected exact disc radius

### 8. Comparison

- `Rational 0 == Ext 0 0 2 2` after normalisation
- Ordering: `Rational 1 < Ext 0 1 2 2` (i.e. $1 < \sqrt{2}$)

## Migration strategy

### Phase 1: New module + tests (no existing code changes)

1. Create `Flag.Construction.Radical` with the data type, normalisation,
   arithmetic instances (for $n = 2$), `nthRootC` (stubbed for $n > 2$),
   `toDouble`, `toKaTeX`.
2. Create `test/RadicalSpec.hs` with the test suite above.
3. Add both to the cabal file.
4. Ensure `./bin/test` passes.

### Phase 2: Geometry on Radical

1. Update `type Point` in `Types.hs` to `(Radical, Radical)`.
2. Update `Drawing` to use `Radical` radius.
3. Rewrite `Geometry.hs` arithmetic to use `Radical` operations.
4. Update `Layers.hs`, `Tree.hs`, `Interpreter.hs` — mostly type-driven,
   minimal logic changes.
5. Fix all type errors in `Constructions.hs` (construction definitions
   should be unchanged since they use arrow combinators, not raw arithmetic).
6. Ensure `./bin/test` passes (existing tests now run on exact arithmetic).

### Phase 3: Rendering boundary

1. Add `toDoublePoint` helper.
2. Update `Diagram.hs` to convert at the rendering boundary.
3. Update `Optimize.hs` — polygon merging works on `Point` which is now
   exact (edge sharing comparison becomes exact equality — an improvement!).
4. Update `DebugV2.hs` to emit `exactX` / `exactY` in JSON.
5. Update `debug-v2.js` to render KaTeX in tooltips.
6. Ensure all flags render identically (diff SVG output before/after).

### Phase 4: Cleanup

1. Remove any remaining `Double`-based point arithmetic.
2. Add KaTeX display to the index page for point coordinates.
3. Document the new type in `README.md`.

## Risks and mitigations

| Risk | Mitigation |
|------|-----------|
| Performance regression from exact arithmetic | Benchmark flag builds; the number of operations per flag is small (< 200 intersections) so this is unlikely to matter |
| Deeply nested `Ext` towers from many chained radicals | Normalisation collapses unnecessary nesting; in practice, flag constructions rarely exceed 2 levels of extension |
| Mixed-radicand arithmetic complexity | Start with same-radicand fast path; fall back to nesting for mixed radicands; add tests for each case |
| Breaking existing tests | Phase 1 is additive; Phase 2 type errors are caught at compile time; existing test assertions should hold since `toDouble` of exact values matches the old `Double` computations |
| SVG output drift | Diff SVG output before and after migration; exact arithmetic should produce *better* results (no floating-point accumulation) |
| Incomplete nth-root arithmetic ($n > 2$) | Explicit `error` stubs with descriptive messages; the type carries $n$ correctly so adding arithmetic later is backwards-compatible |

## Files changed (summary)

| File | Change |
|------|--------|
| `src/Flag/Construction/Radical.hs` | **NEW** — core data type, instances, normalisation, `toDouble`, `toKaTeX`, `nthRootC` |
| `test/RadicalSpec.hs` | **NEW** — test suite |
| `src/Flag/Construction/Types.hs` | Change `type Point`, update `Drawing` |
| `src/Flag/Construction/Geometry.hs` | Rewrite with `Radical` arithmetic |
| `src/Flag/Construction/Interpreter.hs` | Type-driven updates |
| `src/Flag/Construction/Layers.hs` | Type-driven updates, `pointDist` returns `Radical` |
| `src/Flag/Construction/Tree.hs` | Type-driven updates |
| `src/Flag/Construction/Optimize.hs` | Type-driven updates (exact equality is a win) |
| `src/Flag/Render/Diagram.hs` | Add `toDoublePoint`, convert at boundary |
| `src/Flag/Render/DebugV2.hs` | Emit `exactX`/`exactY` in JSON, add KaTeX CDN |
| `src/Flag/Render/Debug.hs` | Type-driven updates |
| `sources/debug-v2.js` | Render KaTeX in tooltips |
| `constructible-flags.cabal` | Add new modules |
| `package.yaml` | Add new modules |
