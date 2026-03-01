# NEW_NUMBERS.md

## Goal

A new numeric data type to support tracking of the field a number is in (Integer, Rational, Irrational, Cyclomatic, Real):

- Use Double for underlying value
- Arithmetic operations should inherit the most complicated field involved, i.e. a Rational + Irrational becomes irrational.
- Square root operation, when not applied to perfect square, should result in an Irrational number.
- Cyclomatic or Real numbers are introduced by methods exposed from this module, which are wrappers around e.g. cos and sin.

```
type Field = FInteger | FRational | FIrrational | FCyclomatic | FReal
type FieldNumber = FieldNumber Field Double
```

---

## Implementation Plan

### Rationale

`Radical` is a symbolic exact-arithmetic type: it maintains expression trees,
performs algebraic normalisation (denesting, consolidation, factoring), and
supports exact `Eq`. The complexity stems from this exactness — the current
module is ~650 lines with numerous normalisation passes that still have known
pathological cases (slow expression blowup, disabled canonical-ordering tests).

`FieldNumber` trades exactness for simplicity: all arithmetic is `Double`
(sufficient for SVG pixel output), and the type merely *tracks* which algebraic
field a number belongs to as metadata. The construction code remains unchanged
in structure; only the numeric implementation changes.

**Gains:**
- No expression trees, no normalisation passes
- No size/depth blowup for complex constructions
- Trig functions (`cos`, `sin`, `pi`) compose naturally via field promotion
- ~90 % reduction in the numeric-type implementation

**Losses:**
- No longer exact: `1/3 + 2/3` may differ from `1` by ~`1e-16`
- `Eq` becomes approximate
- `toRational'` can only return approximations

---

## Phase 1: `FieldNumber` Module

### 1a. File: `src/Flag/Construction/FieldNumber.hs`

#### Type

```haskell
module Flag.Construction.FieldNumber
    ( Field(..)
    , FieldNumber(..)
    -- smart constructors
    , fnInteger, fnRational
    -- trig wrappers (introduce FCyclomatic)
    , fnCos, fnSin
    -- transcendental wrappers (introduce FReal)
    -- None for now
    -- queries
    , fieldOf, getValue
    , isZero, isInteger, isNatural, isRational, isIrrational
    -- conversion
    , toDouble, toRational'
    -- display
    , toKaTeX, showFN
    ) where

-- | Hierarchy of algebraic fields, ordered by complexity.
-- FInteger < FRational < FIrrational < FCyclomatic < FReal
data Field
  = FInteger     -- ^ Whole number: 0, 1, -3
  | FRational    -- ^ Ratio of integers: 1/3, -7/5
  | FIrrational  -- ^ Algebraic irrational from sqrt: √2, (1+√5)/2
  | FCyclomatic  -- ^ Algebraic from trig of rational-π multiples: cos(2π/7)
  | FReal        -- ^ Transcendental: π, e
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A number with a tracked algebraic-field classification.
-- The Double gives the numeric value; the Field tag says what kind of
-- number it is. Once a value enters a higher field it cannot be demoted.
data FieldNumber = FieldNumber !Field !Double
  deriving (Show, Read)
```

#### Smart Constructors

Only two user-facing smart constructors for "input" values:

| Constructor | Signature | Produces |
|---|---|---|
| `fnInteger` | `Integer -> FieldNumber` | `FInteger` |
| `fnRational` | `Rational -> FieldNumber` | `FInteger` if denom=1, else `FRational` |

Cyclomatic and Real values arise exclusively from module-exposed wrappers
(see below), not from raw `Double` tagging.

#### Trig and Transcendental Wrappers

These wrappers replace the `Floating` instance methods and apply the correct
field promotion:

```haskell
-- Cyclomatic wrappers — input field promoted to FCyclomatic
fnCos  :: FieldNumber -> FieldNumber
fnSin  :: FieldNumber -> FieldNumber
```

The `Floating` partial instance delegates directly to these:
```haskell
instance Floating FieldNumber where
  cos = fnCos
  sin = fnSin
  ...
```

This means callers using `cos`, `sin` via the standard `Floating`
interface automatically get the correct field tags. The geometry code
(`evalNGonVertex`) uses `cos theta` and `sin theta`, so it requires no
change beyond the module import swap.

#### Field Promotion

```haskell
promoteField :: Field -> Field -> Field
promoteField = max   -- Field derives Ord; higher tag wins
```

All binary operations (`+`, `-`, `*`) use `promoteField` on input tags.

#### `sqrt` Rules

`sqrt` is the only operator that may *introduce* a new field. The rule for
`sqrt (FieldNumber f d)`:

1. Input field `<= FRational`:
   - Compute `s = sqrt d`.
   - If `abs (round(s)^2 - d) < 1e-9`: perfect integer square → `FieldNumber FInteger (fromIntegral (round s))`
   - Else: non-perfect square → `FieldNumber FIrrational s`
2. Input field `> FRational` (already irrational or higher): field unchanged, value is `sqrt d`.

Note: detecting "perfect rational square" for non-integer rationals (e.g.,
`sqrt(1/4) = 1/2`) requires checking both numerator and denominator. Since
`FRational` values carry only a `Double`, we use the heuristic: if `s` is
within `1e-9` of a low-denominator fraction (by rounding `s` and `1/s`),
classify as `FRational`. In practice, flag specs use only small rationals,
making this reliable.

#### Division Rules

Integer division may produce a rational: `1 / 3 = 0.333…`.

```haskell
divField :: Field -> Field -> Double -> Field
divField f1 f2 result
  | f1 <= FRational && f2 <= FRational =
      if abs (result - fromIntegral (round result :: Integer)) < 1e-9
        then FInteger    -- e.g. 6 / 3 = 2
        else FRational   -- e.g. 1 / 3 = 0.333...
  | otherwise = promoteField f1 f2
```

#### Instances

```haskell
instance Eq FieldNumber where
  FieldNumber _ d1 == FieldNumber _ d2 = abs (d1 - d2) < 1e-9

instance Ord FieldNumber where
  compare (FieldNumber _ d1) (FieldNumber _ d2)
    | abs (d1 - d2) < 1e-9 = EQ
    | d1 < d2               = LT
    | otherwise             = GT

instance Num FieldNumber where
  FieldNumber f1 d1 + FieldNumber f2 d2 =
    FieldNumber (promoteField f1 f2) (d1 + d2)
  -- (-), (*), negate, abs, signum similarly
  fromInteger n = FieldNumber FInteger (fromInteger n)

instance Fractional FieldNumber where
  fn1@(FieldNumber f1 d1) / fn2@(FieldNumber f2 d2) =
    FieldNumber (divField f1 f2 result) result
    where result = d1 / d2
  fromRational r
    | denominator r == 1 = FieldNumber FInteger (fromIntegral (numerator r))
    | otherwise          = FieldNumber FRational (fromRational r)

instance Floating FieldNumber where
  sqrt (FieldNumber f d) = case f of
    _ | f <= FRational -> detectSqrtField d
    _                  -> FieldNumber f (sqrt d)
  pi   = fnPi
  cos  = fnCos
  sin  = fnSin
  ...
```

#### Query Functions

```haskell
fieldOf   :: FieldNumber -> Field
getValue  :: FieldNumber -> Double
toDouble  :: FieldNumber -> Double   -- alias for getValue
isZero    :: FieldNumber -> Bool     -- abs d < 1e-15
isInteger :: FieldNumber -> Bool     -- fieldOf == FInteger
isNatural :: FieldNumber -> Bool     -- isInteger && d >= 0
isRational :: FieldNumber -> Bool    -- fieldOf <= FRational
isIrrational :: FieldNumber -> Bool  -- fieldOf == FIrrational

-- Returns Just for FInteger/FRational (reconstructed from Double),
-- Nothing for higher fields.
toRational' :: FieldNumber -> Maybe Rational
```

#### Display

Use derived Show instance

## Phase 2: Test Suite

### 2a. File: `test/FieldNumberSpec.hs`

Add to `test/Spec.hs` and `package.yaml` other-modules. The helper
`assertNear msg expected actual` asserts `abs (expected - actual) < 1e-9`.

#### Unit Tests — Construction

```haskell
testCase "fnInteger 0 has field FInteger"
testCase "fnInteger 42 has field FInteger"
testCase "fnInteger (-3) has field FInteger"
testCase "fnInteger 5 has value 5.0"

testCase "fnRational (1%2) has field FRational"
testCase "fnRational (3%1) has field FInteger (whole number)"
testCase "fnRational (1%3) has value ~0.333"

testCase "fnPi has field FReal"
testCase "fnPi value ≈ 3.14159265358979"

testCase "fromInteger produces FInteger"
testCase "fromRational (1%3) produces FRational"
testCase "fromRational (6%2) produces FInteger (simplifies)"
```

#### Unit Tests — Trig Wrappers

```haskell
testCase "fnCos result has field FCyclomatic"
testCase "fnSin result has field FCyclomatic"
testCase "fnTan result has field FCyclomatic"
testCase "fnAsin result has field FCyclomatic"
testCase "fnAcos result has field FCyclomatic"
testCase "fnAtan result has field FCyclomatic"
testCase "fnExp result has field FReal"
testCase "fnLog result has field FReal"
testCase "fnAtan2 result has field FCyclomatic"

-- via Floating instance (same wrappers, different syntax)
testCase "cos (via Floating) produces FCyclomatic"
testCase "sin (via Floating) produces FCyclomatic"
testCase "pi (via Floating) produces FReal"

-- value correctness
testCase "fnCos (fnInteger 0) ≈ 1.0"
testCase "fnSin (fnInteger 0) ≈ 0.0"
testCase "fnCos (fnPi) ≈ -1.0"
```

#### Unit Tests — Arithmetic Field Promotion

```haskell
-- Addition
testCase "FInteger + FInteger -> FInteger"
testCase "FInteger + FRational -> FRational"
testCase "FRational + FIrrational -> FIrrational"
testCase "FIrrational + FCyclomatic -> FCyclomatic"
testCase "FCyclomatic + FReal -> FReal"

-- Multiplication
testCase "FInteger * FInteger -> FInteger"
testCase "FRational * FIrrational -> FIrrational"
testCase "FIrrational * FReal -> FReal"

-- Subtraction
testCase "FInteger - FInteger -> FInteger"
testCase "FRational - FRational -> FRational"

-- Division
testCase "FInteger / FInteger -> FInteger (exact: 6/3=2)"
testCase "FInteger / FInteger -> FRational (inexact: 1/3)"
testCase "FRational / FRational -> FRational"
testCase "FIrrational / FIrrational -> FIrrational"
```

#### Unit Tests — sqrt Field Rules

```haskell
-- Perfect-square integers -> FInteger
testCase "sqrt(fnInteger 4) -> FInteger 2"
testCase "sqrt(fnInteger 9) -> FInteger 3"
testCase "sqrt(fnInteger 0) -> FInteger 0"
testCase "sqrt(fnInteger 1) -> FInteger 1"
testCase "sqrt(fnInteger 100) -> FInteger 10"

-- Non-perfect-square integers -> FIrrational
testCase "sqrt(fnInteger 2) -> FIrrational"
testCase "sqrt(fnInteger 3) -> FIrrational"
testCase "sqrt(fnInteger 5) -> FIrrational"
testCase "sqrt(fnInteger 7) -> FIrrational"

-- Perfect-square rationals -> FRational
testCase "sqrt(fnRational (1%4)) -> FRational, value 0.5"
testCase "sqrt(fnRational (9%4)) -> FRational, value 1.5"
testCase "sqrt(fnRational (1%9)) -> FRational, value 1/3"

-- Non-perfect-square rationals -> FIrrational
testCase "sqrt(fnRational (1%2)) -> FIrrational"
testCase "sqrt(fnRational (2%3)) -> FIrrational"

-- Higher fields: field unchanged, value updated
testCase "sqrt(sqrt(fnInteger 2)) stays FIrrational"
testCase "sqrt(fnCos (fnInteger 1)) stays FCyclomatic"
testCase "sqrt(fnPi) stays FReal"
```

#### Unit Tests — Predicates

```haskell
testCase "isZero: fnInteger 0 -> True"
testCase "isZero: FieldNumber FReal 1e-20 -> True (below threshold)"
testCase "isZero: fnInteger 1 -> False"

testCase "isInteger: FInteger -> True"
testCase "isInteger: FRational -> False"
testCase "isInteger: FIrrational -> False"

testCase "isNatural: fnInteger 0 -> True"
testCase "isNatural: fnInteger 7 -> True"
testCase "isNatural: fnInteger (-1) -> False"

testCase "isRational: FInteger satisfies isRational"
testCase "isRational: FRational satisfies isRational"
testCase "isRational: FIrrational does not"
testCase "isRational: FCyclomatic does not"

testCase "toRational': FInteger 5 -> Just (5%1)"
testCase "toRational': FRational (1%3) -> Just (approx 1%3)"
testCase "toRational': FIrrational -> Nothing"
testCase "toRational': FCyclomatic -> Nothing"
testCase "toRational': FReal -> Nothing"
```

#### Unit Tests — Eq and Ord

```haskell
testCase "Eq: same value, same field"
testCase "Eq: same value, different field (field tag ignored)"
testCase "Eq: sqrt(2)^2 ≈ 2"
testCase "Eq: clearly different values are not equal"
testCase "Ord: 1 < 2"
testCase "Ord: sqrt(2) < 2"
testCase "Ord: negative < positive"
testCase "Ord: compare (fnInteger 1) (fnInteger 1) == EQ"
```

#### Unit Tests — Numeric Correctness

```haskell
testCase "(1 + sqrt(2))^2 ≈ 3 + 2*sqrt(2)"
  -- verifies: FIrrational arithmetic propagates correctly

testCase "Quadratic x^2 - 5x + 6 = 0: roots are FInteger 2 and FInteger 3"
  -- disc = sqrt(25-24) = sqrt(1) = 1, FInteger
  -- x1 = (5-1)/2 = 2, x2 = (5+1)/2 = 3, both FInteger

testCase "Quadratic x^2 - 2 = 0: root is FIrrational sqrt(2)"
  -- disc = sqrt(8) = 2*sqrt(2), FIrrational

testCase "Golden ratio (1+sqrt(5))/2 is FIrrational, value ≈ 1.618"

testCase "NGon vertex angle: 2*pi/7 has field FReal (pi is FReal)"
  -- multiplication of FReal by FRational stays FReal

testCase "cos(2*pi/7) has field FCyclomatic"
  -- wrapping via fnCos or Floating cos
```

#### QuickCheck Properties

```haskell
-- Generators
genFieldNumber :: Gen FieldNumber   -- arbitrary Field tag + Double in [-100, 100]
genNonNegFN    :: Gen FieldNumber   -- abs applied

-- Properties

testProperty "field promotion: fieldOf (a+b) >= fieldOf a"
testProperty "field promotion: fieldOf (a+b) >= fieldOf b"
testProperty "field promotion: mul symmetric fieldOf (a*b) == fieldOf (b*a)"
testProperty "toDouble preserves addition: getValue (a+b) ≈ getValue a + getValue b"
testProperty "toDouble preserves multiplication: getValue (a*b) ≈ getValue a * getValue b"
testProperty "sqrt field never decreases: fieldOf (sqrt a) >= fieldOf a"
testProperty "sqrt roundtrip: sqrt(x)^2 ≈ x for x >= 0"
testProperty "negate is involution: negate (negate a) == a"
testProperty "negate preserves field"
testProperty "abs is non-negative: getValue (abs a) >= 0"
testProperty "isZero agrees with value: isZero a == (abs (getValue a) < 1e-15)"
testProperty "isInteger implies isRational"
testProperty "isNatural implies isInteger"
testProperty "Ord: compare consistent with EQ/LT/GT"
testProperty "field of a+a equals field of a (integers closed under addition)"
testProperty "division by non-zero: fieldOf (a/b) >= min (promoteField fa fb) FRational"
testProperty "fnCos result always FCyclomatic regardless of input field"
testProperty "fnExp result always FReal regardless of input field"
```

---

### 2b. File: `test/ArbitraryFieldNumber.hs`

```haskell
module ArbitraryFieldNumber () where

import Test.QuickCheck
import Flag.Construction.FieldNumber

instance Arbitrary FieldNumber where
  arbitrary = do
    f <- elements [FInteger, FRational, FIrrational, FCyclomatic, FReal]
    d <- choose (-10.0, 10.0)
    return (FieldNumber f d)

  shrink (FieldNumber f d)
    | d == 0    = []
    | otherwise = [ FieldNumber f 0
                  , FieldNumber f (d / 2)
                  ] ++ [ FieldNumber f' d | f' <- [minBound .. pred f] ]

instance Arbitrary Field where
  arbitrary = elements [minBound .. maxBound]
  shrink f  = [minBound .. pred f]
```

---

### 2c. Benchmarks: `test/FieldNumberBenchSpec.hs`

Use `tasty-bench` (add to `package.yaml` test dependencies). Unlike the
existing `BenchRadical.hs` standalone executable, these benchmarks live in
the test suite and run via `./bin/test --pattern '/Benchmarks/'`.

```haskell
module FieldNumberBenchSpec (fieldNumberBenchmarks) where

import Test.Tasty.Bench
import Data.Ratio ((%))
import Flag.Construction.FieldNumber

fieldNumberBenchmarks :: TestTree
fieldNumberBenchmarks = bgroup "Benchmarks/FieldNumber"
  [ bgroup "Addition"
    [ bench "FInteger + FInteger"       $ nf (\x -> x + x) (fnInteger 42)
    , bench "FRational + FRational"     $ nf (\x -> x + x) (fnRational (1%3))
    , bench "FIrrational + FIrrational" $ nf (\x -> x + x) (sqrt (fnInteger 2))
    , bench "FCyclomatic + FInteger"    $
        nf (\x -> x + fnInteger 1) (fnCos (fnInteger 1))
    , bench "FReal + FIrrational"       $
        nf (\x -> x + sqrt (fnInteger 2)) fnPi
    ]

  , bgroup "Multiplication"
    [ bench "FInteger * FInteger"              $ nf (\x -> x * x) (fnInteger 7)
    , bench "FIrrational * FIrrational (same)" $ nf (\x -> x * x) (sqrt (fnInteger 2))
    , bench "FIrrational * FIrrational (diff)" $
        let y = sqrt (fnInteger 3)
        in nf (\x -> x * y) (sqrt (fnInteger 2))
    , bench "FCyclomatic * FRational"          $
        nf (\x -> x * fnRational (1%2)) (fnCos (fnInteger 1))
    ]

  , bgroup "Division"
    [ bench "FInteger / FInteger -> FInteger"  $ nf (fnInteger 6 /) (fnInteger 3)
    , bench "FInteger / FInteger -> FRational" $ nf (fnInteger 1 /) (fnInteger 3)
    , bench "FIrrational / FIrrational"        $
        let y = sqrt (fnInteger 3)
        in nf (sqrt (fnInteger 2) /) y
    , bench "FReal / FReal"                    $ nf (fnPi /) fnPi
    ]

  , bgroup "sqrt"
    [ bench "sqrt FInteger (perfect square: 4)"   $ nf sqrt (fnInteger 4)
    , bench "sqrt FInteger (non-perfect: 2)"      $ nf sqrt (fnInteger 2)
    , bench "sqrt FInteger (non-perfect: 5)"      $ nf sqrt (fnInteger 5)
    , bench "sqrt FRational (perfect: 1%4)"       $ nf sqrt (fnRational (1%4))
    , bench "sqrt FRational (non-perfect: 1%2)"   $ nf sqrt (fnRational (1%2))
    , bench "sqrt FIrrational"                    $ nf sqrt (sqrt (fnInteger 2))
    , bench "sqrt FCyclomatic"                    $ nf sqrt (fnCos (fnInteger 1))
    ]

  , bgroup "Trig wrappers"
    [ bench "fnCos FInteger"   $ nf fnCos (fnInteger 1)
    , bench "fnSin FRational"  $ nf fnSin (fnRational (1%4))
    , bench "fnCos FReal (pi)" $ nf fnCos fnPi
    , bench "fnExp FInteger"   $ nf fnExp (fnInteger 1)
    , bench "fnLog FInteger"   $ nf fnLog (fnInteger 2)
    ]

  , bgroup "Comparison to Radical (reference)"
    -- These benchmark the same geometric computation performed by the old
    -- Radical type. The speedup should be substantial (no expression tree).
    [ bench "line-line intersection (FieldNumber)"  $ nf lineLineIntersect ()
    , bench "circle-circle intersection (FieldNumber)" $ nf circleCircleIntersect ()
    ]
  ]

-- Minimal inline versions of the geometry operations for benchmarking
-- without importing the full Geometry module (avoids circular deps in bench).
lineLineIntersect :: () -> FieldNumber
lineLineIntersect _ =
  let x1 = fnInteger 0; y1 = fnInteger 0
      x2 = fnInteger 1; y2 = fnInteger 1
      x3 = fnInteger 0; y3 = fnInteger 1
      x4 = fnInteger 1; y4 = fnInteger 0
      denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
      t = ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom
  in x1 + t*(x2-x1)

circleCircleIntersect :: () -> FieldNumber
circleCircleIntersect _ =
  let x1 = fnInteger 0; y1 = fnInteger 0   -- c1 center
      x2 = fnRational (6%5); y2 = fnInteger 0  -- c2 center (rational)
      dx = x2-x1; dy = y2-y1
      d2 = dx*dx + dy*dy
      r1sq = fnInteger 1
      r2sq = fnInteger 1
      ad = (r1sq - r2sq + d2) / (2*d2)
      hd = sqrt (r1sq/d2 - ad*ad)
  in x1 + ad*dx + hd*dy   -- x-coord of one intersection point
```

#### Package Changes for Benchmarks

In `package.yaml`, add `tasty-bench` to the test target dependencies:
```yaml
tests:
  constructible-flags-test:
    dependencies:
    - ...existing...
    - tasty-bench
```

Add `FieldNumberBenchSpec` to `other-modules`. In `test/Spec.hs`, wire in:
```haskell
import FieldNumberBenchSpec (fieldNumberBenchmarks)
-- in main testGroup:
, fieldNumberBenchmarks
```

---

## Phase 3: Conversion Plan

Once Phase 2 tests all pass, replace `Radical` throughout the project.

The public-facing name for the numeric type is `Number`, introduced as a
type alias in `Types.hs`. All modules that currently import `Radical` will
import `Number` instead. `FieldNumber` remains as the implementation name
in `Flag.Construction.FieldNumber` but is not used directly at call sites.

### 3a. `src/Flag/Construction/Types.hs`

**Change:**
```haskell
-- Before
import Flag.Construction.Radical (Radical, toDouble)
type Point = (Radical, Radical)
drawingRadicals :: Drawing -> [Radical]

-- After
import Flag.Construction.FieldNumber (FieldNumber, toDouble, fieldOf, isZero)
import Flag.Construction.FieldNumber (fnInteger, fnRational, fnCos, fnSin)

-- Public alias: the rest of the project uses 'Number', not 'FieldNumber'
type Number = FieldNumber

type Point = (Number, Number)
drawingNumbers :: Drawing -> [Number]
```

`Types.hs` re-exports `Number` and the smart constructors so that downstream
modules only need `import Flag.Construction.Types` — they never import
`Flag.Construction.FieldNumber` directly.

All `Drawing` constructors reference `Point` and use `Radical` for radii
(e.g., `DrawCircle` radius). Replace every occurrence of `Radical` with
`Number` in the type signatures.

### 3b. `src/Flag/Construction/Geometry.hs`

**Changes:**
1. `import Flag.Construction.Radical (Radical(..), isZero)` →
   `import Flag.Construction.Types (Number, isZero)` (picks up the alias)
2. `evalNGonVertex`: remove `Real (cos theta)` / `Real (sin theta)` —
   replace with `fnCos` / `fnSin` applied to a `Number` angle, or rely on
   the `Floating` instance (`cos`, `sin`) which delegates to those wrappers
   automatically. Since `theta` is computed as a `Double`, convert first:
   ```haskell
   -- Before
   cosT = Real (cos theta)
   sinT = Real (sin theta)
   -- After
   cosT = fnCos (fromDouble theta)   -- or: cos (fromDouble theta :: Number)
   sinT = fnSin (fromDouble theta)
   ```
   where `fromDouble :: Double -> Number` is added as a convenience (producing
   `FReal`, since a raw Double has no better provenance).
   **Alternative:** keep `theta` as a `Number` throughout by computing it via
   `Number` arithmetic: `theta = 2 * pi * fromInteger k / fromInteger n` — then
   `cos theta` and `sin theta` via the `Floating` instance give `FCyclomatic`
   automatically (since `cos` delegates to `fnCos`).
3. `isZero denom` in `evalIntersectLL'` — unchanged: `isZero :: Number -> Bool` preserved.
4. `evalIntersectLC'`, `evalIntersectCC'`, `dist` — no changes needed.

### 3c. `src/Flag/Construction/Interpreter.hs`

Replace `import Flag.Construction.Radical (Radical)` with
`import Flag.Construction.Types (Number)`.
`evalCollectRadicals` becomes:
```haskell
evalCollectNumbers :: FlagA a b -> a -> (b, [Number])
```

### 3d. `src/Flag/Construction/Layers.hs`, `Debug.hs`, `Render/*.hs`

Grep for `Radical` imports — replace with `Number` (imported from
`Flag.Construction.Types`). The render pipeline only calls `toDouble` on
coordinates; no structural inspection of expression trees. Mechanical
import and type substitution throughout.

### 3e. `src/Flag/Construction/Optimize.hs`

Check for any `Radical`-specific logic (e.g., `isRational`, `radicands`).
If `Optimize` uses `radicands` (extracts radicand list from expression tree),
that concept no longer exists. The equivalent in `Number` is `fieldOf`.
Rewrite any such logic in terms of field tags.

### 3f. Country flag files (`src/Flag/Country/*.hs`)

Country flags define constructions using the arrow DSL (`proc ... do`).
They use only:
- `Point` type (changes transparently via `Types.hs`)
- Numeric literals (`fromInteger`, `fromRational`) via instances
- `Ratio Int` for `rationalMult` — no change

**No country flag files should need changes** beyond recompiling against
the updated library. Verify by building after updating the library modules.

### 3g. Test Files

| Old file | Action |
|---|---|
| `test/RadicalSpec.hs` | Delete. Replaced by `FieldNumberSpec.hs`. |
| `test/ArbitraryRadical.hs` | Delete. Replaced by `ArbitraryFieldNumber.hs`. |
| `test/GeometrySpec.hs` | Update point construction: `Rational (n%d)` → `fnRational (n%d)`. Remove all `Ext (...)` literal constructions — delete the `l1Regression` / `l2Regression` values (no longer relevant). |
| `test/ConstructionSpec.hs` | Check for `Radical` constructor usage; likely minimal changes since it uses the arrow DSL. |
| `test/Spec.hs` | Add `FieldNumberSpec`, `FieldNumberBenchSpec`; remove `RadicalSpec`. |

The field-membership aspects of the regression points (intersection of
lines with irrational coordinates) can be verified with `fieldOf` if desired.

### 3h. `package.yaml`

1. Remove `RadicalSpec` from `other-modules` in test target.
2. Add `FieldNumberSpec`, `ArbitraryFieldNumber`, `FieldNumberBenchSpec`.
3. Add `tasty-bench` to test dependencies.
4. Remove `bench-radical` executable (or keep as historical reference — it
   will no longer compile against `Number` without updates).

### 3i. `src/Flag/Construction/Radical.hs`

**Delete.** No backwards compatibility required (per `CLAUDE.md`).

---

## Known Limitations and Trade-offs

| Concern | Impact |
|---|---|
| **Loss of exact arithmetic** | `1/3 + 2/3` may differ from `1.0` by ~`1e-16`. In practice, flag geometry never tests exact rational equality directly; only `toDouble` output matters for SVG rendering. |
| **Approximate `Eq`** | Two computed points that are "the same" geometrically but computed via different paths may compare unequal if the error exceeds `1e-9`. The epsilon matches existing `approxD` usage in tests. Where needed, use `isZero (a - b)` explicitly. |
| **Perfect-square detection** | `sqrt` of `FRational (n%d)` classifies as `FRational` only via a floating-point heuristic. For large `n`, `d`, floating-point imprecision may misclassify. Acceptable for flag specs, which use only small rationals. |
| **`toRational'` is approximate** | For `FRational` values, `toRational'` reconstructs a `Rational` from the Double. May not exactly equal the original input. Only used for diagnostics/display. |
| **`FCyclomatic` broadness** | `cos(π/3) = 1/2` is rational, not truly cyclomatic. Detecting this would require a lookup table of rational trig values — overkill for the current application. Accepting false FCyclomatic tags for such edge cases. |
| **`FReal` from multiplying by pi** | `2 * pi` has field `FReal` (pi promotes everything). This is technically correct (it's transcendental), even though `cos(2*pi)` would collapse back to `1`. Field tracking is conservative: once FReal, always FReal within a computation. |

---

## Implementation Order

1. **Write `src/Flag/Construction/FieldNumber.hs`** — type, instances, constructors, wrappers.
2. **Write `test/FieldNumberSpec.hs`** — all unit + property tests from Phase 2.
3. **Write `test/ArbitraryFieldNumber.hs`** — QuickCheck instance.
4. **Write `test/FieldNumberBenchSpec.hs`** — tasty-bench benchmarks.
5. **Update `package.yaml`** — add new modules and `tasty-bench` dep.
6. **Update `test/Spec.hs`** — wire in `fieldNumberTests` and `fieldNumberBenchmarks`.
7. **Run tests**: `./bin/test --pattern '/FieldNumber/'` — all should pass.
8. **Update `src/Flag/Construction/Types.hs`** — add `type Number = FieldNumber`,
   update `Point` and `Drawing` to use `Number`, re-export smart constructors.
9. **Update `src/Flag/Construction/Geometry.hs`** — import `Number` from `Types`,
   fix NGonVertex to use `Number` arithmetic for trig.
10. **Update `src/Flag/Construction/Interpreter.hs`** — swap `Radical` → `Number`.
11. **Update remaining `src/` files** — mechanical import swap (`Radical` → `Number`).
12. **Delete `src/Flag/Construction/Radical.hs`**.
13. **Update `test/GeometrySpec.hs`** and other test files — replace `Ext`/`Rational`
    constructors with `fnRational`/arithmetic; delete regression value literals.
14. **Delete `test/RadicalSpec.hs`** and `test/ArbitraryRadical.hs`**.
15. **Full test run**: `./bin/test` — all tests should pass.
16. **Build**: `stack build` — clean compile with no `Radical` references.
