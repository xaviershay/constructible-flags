# Normalization strategies

All three strategies have been implemented in `src/Flag/Construction/Radical.hs`.

## Implemented passes (in pipeline order)

1. **`normalizeNested`** — recursively normalize sub-expressions
2. **`composeRoots`** — flatten nested pure roots: `√(√r) = ∜r`, `√(∛r) = r^(1/6)`, etc.
3. **`factorRadical`** — strip perfect nth powers from rational radicands, rationalize denominators, and reduce root index via GCD (`∜4 = √2`, `8^(1/6) = √2`, etc.)
4. **`consolidateRadicands`** — merge duplicate radicands at any nesting depth into one layer
5. **`denestSqrt`** — denest `√(a ± b√r)` when discriminant is a perfect rational square
6. **`zeroElimination`** — collapse `Ext` when coefficient or radicand is zero
7. **`canonicalOrder`** — sort radicand layers so the largest radicand is outermost, making `Eq` commutative
