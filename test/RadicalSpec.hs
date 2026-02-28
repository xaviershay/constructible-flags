module RadicalSpec (radicalTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, forAll, elements, oneof, frequency, (==>))
import Test.QuickCheck (Gen)

import Data.Ratio ((%))

import Flag.Construction.Radical

radicalTests :: TestTree
radicalTests = testGroup "Radical"
  [ nthPowerFreeTests
  , normalisationTests
  , denestingTests
  , arithmeticTests
  , sqrtTests
  , nthRootTests
  , toDoubleTests
  , toKaTeXTests
  , comparisonTests
  , isNaturalTests
  , isIntegerTests
  , radicandsTests
  , realTests
  , divRMultiRadicandTests
  , consolidationTests
  , rootIndexReductionTests
  , nestedRootCompositionTests
  , canonicalOrderingTests
  ]

-- -----------------------------------------------------------------------
-- Helpers
-- -----------------------------------------------------------------------

-- | Shorthand for a rational Radical
rat :: Rational -> Radical
rat = Rational

-- | Shorthand for a square-root extension: a + b√r
ext2 :: Radical -> Radical -> Radical -> Radical
ext2 a b r = normalize (Ext a b r 2)

-- | Assert two Radicals are structurally equal (after normalisation)
radicalEq :: String -> Radical -> Radical -> Assertion
radicalEq msg expected actual =
  assertEqual msg expected actual

-- | Assert a Radical equals a rational value
isRat :: String -> Rational -> Radical -> Assertion
isRat msg expected actual =
  radicalEq msg (Rational expected) actual

-- | Assert two doubles are approximately equal
approxD :: String -> Double -> Double -> Assertion
approxD msg expected actual =
  assertBool (msg ++ ": expected " ++ show expected ++ " got " ++ show actual)
    (abs (expected - actual) < 1e-9)

-- | Depth of a Radical expression tree
radDepth :: Radical -> Int
radDepth (Rational _) = 0
radDepth (Real _) = 0
radDepth (Ext a b _ _) = 1 + max (radDepth a) (radDepth b)

-- | Size (node count) of a Radical expression tree
radSize :: Radical -> Int
radSize (Rational _) = 1
radSize (Real _) = 1
radSize (Ext a b r _) = 1 + radSize a + radSize b + radSize r

-- -----------------------------------------------------------------------
-- 0. nthPowerFree
-- -----------------------------------------------------------------------

nthPowerFreeTests :: TestTree
nthPowerFreeTests = testGroup "nthPowerFree"
  [ testCase "nthPowerFree 2 1 = (1, 1)" $
      nthPowerFree 2 1 @?= (1, 1)

  , testCase "nthPowerFree 2 4 = (2, 1) — 4 = 2²·1" $
      nthPowerFree 2 4 @?= (2, 1)

  , testCase "nthPowerFree 2 12 = (2, 3) — 12 = 2²·3" $
      nthPowerFree 2 12 @?= (2, 3)

  , testCase "nthPowerFree 2 18 = (3, 2) — 18 = 3²·2" $
      nthPowerFree 2 18 @?= (3, 2)

  , testCase "nthPowerFree 2 50 = (5, 2) — 50 = 5²·2" $
      nthPowerFree 2 50 @?= (5, 2)

  , testCase "nthPowerFree 2 7 = (1, 7) — 7 is square-free" $
      nthPowerFree 2 7 @?= (1, 7)

  , testCase "nthPowerFree 2 0 = (0, 0)" $
      nthPowerFree 2 0 @?= (0, 0)

  , testCase "nthPowerFree 3 8 = (2, 1) — 8 = 2³·1" $
      nthPowerFree 3 8 @?= (2, 1)

  , testCase "nthPowerFree 3 24 = (2, 3) — 24 = 2³·3" $
      nthPowerFree 3 24 @?= (2, 3)

  , testCase "nthPowerFree 3 27 = (3, 1) — 27 = 3³·1" $
      nthPowerFree 3 27 @?= (3, 1)

  , testCase "nthPowerFree 2 72 = (6, 2) — 72 = 6²·2" $
      nthPowerFree 2 72 @?= (6, 2)

  , testCase "nthPowerFree 2 100 = (10, 1) — 100 = 10²" $
      nthPowerFree 2 100 @?= (10, 1)
  ]

-- -----------------------------------------------------------------------
-- 1. Normalisation
-- -----------------------------------------------------------------------

normalisationTests :: TestTree
normalisationTests = testGroup "Normalisation"
  [ testCase "Eliminate radicals with 0 coefficient" $
      radicalEq "zero coeff" (rat 5) (normalize (Ext (rat 5) (rat 0) (rat 3) 2))

  -- PERFECT SQUARES
  , testCase "Eliminate perfect squares: √4 = 2" $
      isRat "√4" 2 (normalize (Ext (rat 0) (rat 1) (rat 4) 2))
  , testCase "Ext 0 1 (1/4) 2 collapses to Rational (1/2)" $
      isRat "√(1/4)" (1 % 2) (normalize (Ext (rat 0) (rat 1) (rat (1 % 4)) 2))
  , testCase "Ext 0 1 (9/4) 2 collapses to Rational (3/2)" $
      isRat "√(9/4)" (3 % 2) (normalize (Ext (rat 0) (rat 1) (rat (9 % 4)) 2))
  , testCase "Cube root: Ext 0 1 8 3 collapses to Rational 2" $
      isRat "∛8" 2 (normalize (Ext (rat 0) (rat 1) (rat 8) 3))

  -- EXTRACTING SQUARE COMPONENT FROM RADICAL
  , testCase "Ext 0 1 12 2 normalises to Ext 0 2 3 2 (√12 = 2√3)" $
      radicalEq "√12 = 2√3"
        (Ext (rat 0) (rat 2) (rat 3) 2)
        (normalize (Ext (rat 0) (rat 1) (rat 12) 2))

  , testCase "Rational 0 + Rational 0 = Rational 0" $
      isRat "0+0" 0 (rat 0 + rat 0)

  , testCase "Deeply nested: Ext (Ext 0 0 2 2) 1 3 2 collapses" $
      radicalEq "nested zero"
        (Ext (rat 0) (rat 1) (rat 3) 2)
        (normalize (Ext (Ext (rat 0) (rat 0) (rat 2) 2) (rat 1) (rat 3) 2))

  , testCase "Root of zero is 0" $
      isRat "0 radicand" 5 (normalize (Ext (rat 5) (rat 3) (rat 0) 2))

  -- Regression: normalization should be idempotent for tricky inputs that
  -- previously caused rewrite cycles in trace output.
  , testCase "normalize is idempotent (no-op repeat case)" $
      let x = Ext (rat 0) (rat (6 % 5)) (rat 5) 2
      in assertEqual "idempotent" (normalize x) (normalize (normalize x))

  , testCase "normalize is idempotent (81/5 -> 9/1 case)" $
      let x = Ext (rat 0) (rat 1) (rat (81 % 5)) 2
      in assertEqual "idempotent 81/5" (normalize x) (normalize (normalize x))

  , testCase "normalize is idempotent (denominator-doubling case)" $
      let x = Ext (rat 9) (rat 9) (rat (1 % 5)) 2
      in assertEqual "idempotent denom-doubling" (normalize x) (normalize (normalize x))

  -- Regression: without factorRadical, adding Ext values with different
  -- rational radicands causes unbounded nesting. This test checks that
  -- (a + b√(4/5))² normalises to a flat Ext (not a deeply nested tree).
  , testCase "squaring Ext with reducible radicand stays flat" $
      let x = Ext (rat (1 % 2)) (rat ((-5) % 3)) (rat (4 % 5)) 2
          x2 = x * x
      in assertBool "result is Rational or single Ext"
           (case x2 of
              Rational _ -> True
              Ext _ _ (Rational _) _ -> True
              _ -> False)

  -- Regression: intersectCC with Ext inputs hangs computing
  -- h = sqrt(r1^2 - a^2) where a = d^2/(2*d) = d/2 but the
  -- symbolic form of a after divR is deeply nested.
  -- Minimal: divR by sqrt of doubly-nested Ext
  , testCase "divR: 1 / sqrt(doubly-nested Ext) simple" $
      let inner = Ext (rat 1) (rat 1) (rat 2) 2       -- 1+√2
          outer = Ext inner (rat 1) (rat 3) 2          -- (1+√2)+√3
          s = sqrtC outer                              -- √((1+√2)+√3)
      in approxD "1/s" (1 / toDouble s) (toDouble (1 / s))
    , testCase "divR: x^2/(2x) where x = sqrt(doubly-nested) simple" $
      let inner = Ext (rat 1) (rat 1) (rat 2) 2
          outer = Ext inner (rat 1) (rat 3) 2
          s = sqrtC outer
      in approxD "s/2" (toDouble s / 2) (toDouble (s*s / (2*s)))
    , testCase "divR: 1 / sqrt(doubly-nested Ext) actual radicand" $
      let radicand = Ext (Ext (rat (182405 % 15876)) (rat (52 % 21)) (rat 5) 2)
                         (rat ((-10) % 27)) (rat (1 % 5)) 2
          s = sqrtC radicand
      in approxD "1/s" (1 / toDouble s) (toDouble (1 / s))
    , testCase "divR: x^2/(2x) actual radicand" $
      let radicand = Ext (Ext (rat (182405 % 15876)) (rat (52 % 21)) (rat 5) 2)
                         (rat ((-10) % 27)) (rat (1 % 5)) 2
          s = sqrtC radicand
      in approxD "s/2" (toDouble s / 2) (toDouble (s*s / (2*s)))
    , testCase "intersectCC sub-steps: aval = d^2/(2d) completes" $
      let radicand = Ext (Ext (rat (182405 % 15876)) (rat (52 % 21)) (rat 5) 2)
                         (rat ((-10) % 27)) (rat (1 % 5)) 2
          r1 = sqrtC radicand
          d  = r1
          aval = (r1*r1 - r1*r1 + d*d) / (2 * d)
      in approxD "a=d/2" (toDouble d / 2) (toDouble aval)
    , testCase "intersectCC sub-steps: aval*aval completes" $
      let radicand = Ext (Ext (rat (182405 % 15876)) (rat (52 % 21)) (rat 5) 2)
                         (rat ((-10) % 27)) (rat (1 % 5)) 2
          r1 = sqrtC radicand
          d  = r1
          aval = (r1*r1 - r1*r1 + d*d) / (2 * d)
      in approxD "a^2" (toDouble d ^ 2 / 4) (toDouble (aval * aval))
    , testCase "intersectCC sub-steps: r1^2 - a^2 completes" $
      let radicand = Ext (Ext (rat (182405 % 15876)) (rat (52 % 21)) (rat 5) 2)
                         (rat ((-10) % 27)) (rat (1 % 5)) 2
          r1 = sqrtC radicand
          d  = r1
          aval = (r1*r1 - r1*r1 + d*d) / (2 * d)
          inside = r1*r1 - aval*aval
      in approxD "r1^2-a^2" (toDouble r1 ^ 2 * 3 / 4) (toDouble inside)
  -- Normalization should merge √(1/5) and √5 into a single extension level
  -- since √(1/5) = √5/5. Without this, divR creates 24-deep nesting
  -- with alternating √(1/5) and √5 levels.
  , testCase "normalize merges √(1/r) and √r extensions" $
      let x = Ext (Ext (rat 1) (rat 1) (rat 5) 2)
                  (rat 1) (rat (1 % 5)) 2
          -- x = (1 + √5) + √(1/5) = (1 + √5) + √5/5
          -- should normalize to Ext _ _ 5 2 (single level)
      in case normalize x of
           Ext _ _ (Rational r) 2 | r == 5 ->
             approxD "value" (1 + sqrt 5 + sqrt 0.2) (toDouble x)
           other -> assertFailure $
             "Expected single Ext with radicand 5, got: " ++ take 200 (show other)
    , testCase "intersectCC sub-steps: sqrt(r1^2 - a^2) completes" $
      let radicand = Ext (Ext (rat (182405 % 15876)) (rat (52 % 21)) (rat 5) 2)
                         (rat ((-10) % 27)) (rat (1 % 5)) 2
          r1 = sqrtC radicand
          d  = r1
          aval = (r1*r1 - r1*r1 + d*d) / (2 * d)
          h  = sqrtC (r1*r1 - aval*aval)
      in approxD "h" (toDouble r1 * sqrt 3 / 2) (toDouble h)
  ]

-- -----------------------------------------------------------------------
-- 1b. Denesting sqrt(a ± b√n)
-- -----------------------------------------------------------------------

denestingTests :: TestTree
denestingTests = testGroup "Denesting sqrt(a +- b*sqrt(n))"
  -- The identity: sqrt(a + b√n) = sqrt((a+d)/2) + sign(b)*sqrt((a-d)/2)
  -- where d = sqrt(a^2 - b^2*n), applies when disc = a^2 - b^2*n is a
  -- perfect rational square.

  [ testCase "sqrt(3 + 2√2) = 1 + √2  [disc = 9-8 = 1]" $
      let inner = Ext (rat 3) (rat 2) (rat 2) 2
      in radicalEq "denest"
           (Ext (rat 1) (rat 1) (rat 2) 2)
           (normalize (Ext (rat 0) (rat 1) inner 2))

  , testCase "sqrt(7 + 4√3) = 2 + √3  [disc = 49-48 = 1]" $
      let inner = Ext (rat 7) (rat 4) (rat 3) 2
      in radicalEq "denest"
           (Ext (rat 2) (rat 1) (rat 3) 2)
           (normalize (Ext (rat 0) (rat 1) inner 2))

  , testCase "sqrt(6 + 2√5) = 1 + √5  [disc = 36-20 = 16]" $
      let inner = Ext (rat 6) (rat 2) (rat 5) 2
      in radicalEq "denest"
           (Ext (rat 1) (rat 1) (rat 5) 2)
           (normalize (Ext (rat 0) (rat 1) inner 2))

  , testCase "sqrt(5 - 2√6) = √3 - √2  [disc = 25-24 = 1]" $
      let inner = Ext (rat 5) (rat (-2)) (rat 6) 2
          result = normalize (Ext (rat 0) (rat 1) inner 2)
      in approxD "denest neg" (sqrt 3 - sqrt 2) (toDouble result)

  , testCase "2*sqrt(3 + 2√2) = 2 + 2√2  [outer coefficient]" $
      let inner = Ext (rat 3) (rat 2) (rat 2) 2
      in radicalEq "denest coeff"
           (Ext (rat 2) (rat 2) (rat 2) 2)
           (normalize (Ext (rat 0) (rat 2) inner 2))

  -- This case requires the fix: when a0 /= 0 the denesting must still fire
  -- and combine the outer addend with the denested result.
  , testCase "1 + sqrt(3 + 2√2) = 2 + √2  [non-zero outer addend]" $
      let inner = Ext (rat 3) (rat 2) (rat 2) 2
      in radicalEq "denest outer"
           (Ext (rat 2) (rat 1) (rat 2) 2)
           (normalize (Ext (rat 1) (rat 1) inner 2))

  , testCase "3 + 2*sqrt(7 + 4√3) = 7 + 2√3  [non-zero a0 with coefficient]" $
      let inner = Ext (rat 7) (rat 4) (rat 3) 2
      in radicalEq "denest outer coeff"
           (Ext (rat 7) (rat 2) (rat 3) 2)
           (normalize (Ext (rat 3) (rat 2) inner 2))

  , testCase "sqrt(2 + √2): disc = 2 (not perfect square), value preserved" $
      -- a=2, b=1, r=2 => disc = 4-2 = 2, not a perfect square => stays nested
      let inner = Ext (rat 2) (rat 1) (rat 2) 2
          result = normalize (Ext (rat 0) (rat 1) inner 2)
      in approxD "no denest" (sqrt (2 + sqrt 2)) (toDouble result)
  ]

-- -----------------------------------------------------------------------
-- 2. Arithmetic (n=2)
-- -----------------------------------------------------------------------

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic"
  [ testCase "Rational + Rational" $
      isRat "3+4" 7 (rat 3 + rat 4)

  , testCase "(1 + √2) + (3 - √2) = 4" $
      isRat "cancel"
        4
        (ext2 (rat 1) (rat 1) (rat 2) + ext2 (rat 3) (rat (-1)) (rat 2))

  , testCase "(1 + √2) * (1 - √2) = -1" $
      isRat "conjugate"
        (-1)
        (ext2 (rat 1) (rat 1) (rat 2) * ext2 (rat 1) (rat (-1)) (rat 2))

  , testCase "(√2)² = 2" $
      let sqrt2 = ext2 (rat 0) (rat 1) (rat 2)
      in  isRat "sq" 2 (sqrt2 * sqrt2)

  , testCase "1 / √2 = (1/2)√2" $
      let sqrt2 = ext2 (rat 0) (rat 1) (rat 2)
          result = rat 1 / sqrt2
      in  radicalEq "recip √2"
            (Ext (rat 0) (rat (1 % 2)) (rat 2) 2)
            result

  , testCase "Rational subtraction" $
      isRat "5-3" 2 (rat 5 - rat 3)

  , testCase "Negation of extension" $
      radicalEq "negate"
        (Ext (rat (-1)) (rat (-1)) (rat 2) 2)
        (negate (ext2 (rat 1) (rat 1) (rat 2)))

  , testCase "Distributivity: a*(b+c) = a*b + a*c" $
      let a = ext2 (rat 1) (rat 1) (rat 2)
          b = rat 3
          c = ext2 (rat 0) (rat 1) (rat 2)
          lhs = a * (b + c)
          rhs = a * b + a * c
      in  approxD "distributive" (toDouble lhs) (toDouble rhs)

  , testCase "Associativity: (a+b)+c = a+(b+c)" $
      let a = rat 1
          b = ext2 (rat 0) (rat 1) (rat 2)
          c = ext2 (rat 0) (rat 1) (rat 3)
          lhs = (a + b) + c
          rhs = a + (b + c)
      in  approxD "associative" (toDouble lhs) (toDouble rhs)

  , testCase "Multiplication by zero" $
      isRat "×0" 0 (ext2 (rat 1) (rat 1) (rat 2) * rat 0)

  , testCase "Division by rational" $
      isRat "6/3" 2 (rat 6 / rat 3)

  , testCase "fromInteger" $
      isRat "fromInteger" 42 (fromInteger 42 :: Radical)
  ]

-- -----------------------------------------------------------------------
-- 3. Square root
-- -----------------------------------------------------------------------

sqrtTests :: TestTree
sqrtTests = testGroup "Square root"
  [ testCase "sqrtC (Rational 4) = Rational 2" $
      isRat "√4" 2 (sqrtC (rat 4))

  , testCase "sqrtC (Rational 2) = Ext 0 1 2 2" $
      radicalEq "√2"
        (Ext (rat 0) (rat 1) (rat 2) 2)
        (sqrtC (rat 2))

  , testCase "sqrtC (Rational (9/4)) = Rational (3/2)" $
      isRat "√(9/4)" (3 % 2) (sqrtC (rat (9 % 4)))

  , testCase "sqrtC (Rational (1/2)) normalises" $ do
      let result = sqrtC (rat (1 % 2))
      approxD "√(1/2)" (sqrt 0.5) (toDouble result)

  , testCase "sqrtC (Rational 0) = 0" $
      isRat "√0" 0 (sqrtC (rat 0))

  , testCase "sqrtC (Rational 1) = 1" $
      isRat "√1" 1 (sqrtC (rat 1))

  , testCase "sqrtC (Rational 18) = 3√2" $
      radicalEq "√18"
        (Ext (rat 0) (rat 3) (rat 2) 2)
        (sqrtC (rat 18))

  , testCase "sqrtC (Rational 50) = 5√2" $
      radicalEq "√50"
        (Ext (rat 0) (rat 5) (rat 2) 2)
        (sqrtC (rat 50))
  ]

-- -----------------------------------------------------------------------
-- 4. Nth root
-- -----------------------------------------------------------------------

nthRootTests :: TestTree
nthRootTests = testGroup "Nth root"
  [ testCase "nthRootC 3 (Rational 27) = Rational 3" $
      isRat "∛27" 3 (nthRootC 3 (rat 27))

  , testCase "nthRootC 3 (Rational 2) = Ext 0 1 2 3" $
      radicalEq "∛2"
        (Ext (rat 0) (rat 1) (rat 2) 3)
        (nthRootC 3 (rat 2))

  , testCase "nthRootC 4 (Rational 16) = Rational 2" $
      isRat "∜16" 2 (nthRootC 4 (rat 16))

  , testCase "nthRootC 3 (Rational (-8)) = Rational (-2)" $
      isRat "∛(-8)" (-2) (nthRootC 3 (rat (-8)))

  , testCase "nthRootC 3 (Rational 8) = Rational 2" $
      isRat "∛8" 2 (nthRootC 3 (rat 8))

  , testCase "nthRootC 4 (Rational 81) = Rational 3" $
      isRat "∜81" 3 (nthRootC 4 (rat 81))

  , testCase "nthRootC 3 (Rational 0) = Rational 0" $
      isRat "∛0" 0 (nthRootC 3 (rat 0))
  ]

-- -----------------------------------------------------------------------
-- 5. Conversion to Double
-- -----------------------------------------------------------------------

toDoubleTests :: TestTree
toDoubleTests = testGroup "Conversion to Double"
  [ testCase "toDouble (Rational (1/3)) ≈ 0.33333" $
      approxD "1/3" (1/3) (toDouble (rat (1 % 3)))

  , testCase "toDouble (Ext 0 1 2 2) ≈ 1.41421" $
      approxD "√2" (sqrt 2) (toDouble (Ext (rat 0) (rat 1) (rat 2) 2))

  , testCase "toDouble (Ext 1 1 2 2) ≈ 2.41421" $
      approxD "1+√2" (1 + sqrt 2) (toDouble (Ext (rat 1) (rat 1) (rat 2) 2))

  , testCase "toDouble (Ext 0 1 2 3) ≈ 1.25992 (∛2)" $
      approxD "∛2" (2 ** (1/3)) (toDouble (Ext (rat 0) (rat 1) (rat 2) 3))

  , testCase "toDouble (Rational 0) = 0" $
      approxD "0" 0.0 (toDouble (rat 0))

  , testCase "toDouble preserves negatives" $
      approxD "neg" (-2.5) (toDouble (rat ((-5) % 2)))
  ]

-- -----------------------------------------------------------------------
-- 6. KaTeX rendering
-- -----------------------------------------------------------------------

toKaTeXTests :: TestTree
toKaTeXTests = testGroup "KaTeX rendering"
  [ testCase "Rational 3" $
      toKaTeX (rat 3) @?= "3"

  , testCase "Rational (1/2)" $
      toKaTeX (rat (1 % 2)) @?= "\\frac{1}{2}"

  , testCase "Ext 0 1 3 2 → √3" $
      toKaTeX (Ext (rat 0) (rat 1) (rat 3) 2) @?= "\\sqrt{3}"

  , testCase "Ext (1/2) (1/2) 3 2" $
      toKaTeX (Ext (rat (1 % 2)) (rat (1 % 2)) (rat 3) 2)
        @?= "\\frac{1}{2} + \\frac{1}{2}\\sqrt{3}"

  , testCase "Ext 0 (-1) 5 2 → -√5" $
      toKaTeX (Ext (rat 0) (rat (-1)) (rat 5) 2) @?= "-\\sqrt{5}"

  , testCase "Ext 0 1 2 3 → ∛2" $
      toKaTeX (Ext (rat 0) (rat 1) (rat 2) 3) @?= "\\sqrt[3]{2}"

  , testCase "Ext 1 1 5 3 → 1 + ∛5" $
      toKaTeX (Ext (rat 1) (rat 1) (rat 5) 3) @?= "1 + \\sqrt[3]{5}"

  , testCase "Rational (-3)" $
      toKaTeX (rat (-3)) @?= "-3"

  , testCase "Rational 0" $
      toKaTeX (rat 0) @?= "0"

  , testCase "Ext 0 2 3 2 → 2√3" $
      toKaTeX (Ext (rat 0) (rat 2) (rat 3) 2) @?= "2\\sqrt{3}"

  , testCase "Ext 1 (-2) 3 2 → 1 - 2√3" $
      toKaTeX (Ext (rat 1) (rat (-2)) (rat 3) 2) @?= "1 - 2\\sqrt{3}"

  , testCase "Ext 0 (3/5) 2 2 → (3/5)√2" $
      toKaTeX (Ext (rat 0) (rat (3 % 5)) (rat 2) 2)
        @?= "\\frac{3}{5}\\sqrt{2}"
  ]

-- -----------------------------------------------------------------------
-- 7. Comparison
-- -----------------------------------------------------------------------

comparisonTests :: TestTree
comparisonTests = testGroup "Comparison"
  [ testCase "Rational 0 == Ext 0 0 2 2 after normalisation" $
      assertEqual "zero eq" (rat 0) (normalize (Ext (rat 0) (rat 0) (rat 2) 2))

  , testCase "Rational 1 < Ext 0 1 2 2 (1 < √2)" $
      assertBool "1 < √2" (rat 1 < Ext (rat 0) (rat 1) (rat 2) 2)

  , testCase "Ext 0 1 2 2 < Rational 2 (√2 < 2)" $
      assertBool "√2 < 2" (Ext (rat 0) (rat 1) (rat 2) 2 < rat 2)

  , testCase "Equal values compare EQ" $
      assertEqual "eq" EQ (compare (rat 2) (rat 2))

  , testCase "Negative < Positive" $
      assertBool "neg < pos" (rat (-1) < rat 1)
  ]

-- -----------------------------------------------------------------------
-- 8. isNatural
-- -----------------------------------------------------------------------

isNaturalTests :: TestTree
isNaturalTests = testGroup "isNatural"
  [ testCase "0 is natural" $
      assertBool "0" (isNatural (rat 0))

  , testCase "positive integer is natural" $
      assertBool "42" (isNatural (rat 42))

  , testCase "negative integer is not natural" $
      assertBool "-1" (not (isNatural (rat (-1))))

  , testCase "rational is not natural" $
      assertBool "1/2" (not (isNatural (rat (1 % 2))))

  , testCase "extension is not natural" $
      assertBool "√2" (not (isNatural (ext2 (rat 0) (rat 1) (rat 2))))
  ]

-- -----------------------------------------------------------------------
-- 9. isInteger
-- -----------------------------------------------------------------------

isIntegerTests :: TestTree
isIntegerTests = testGroup "isInteger"
  [ testCase "0 is integer" $
      assertBool "0" (isInteger (rat 0))

  , testCase "positive integer is integer" $
      assertBool "42" (isInteger (rat 42))

  , testCase "negative integer is integer" $
      assertBool "-1" (isInteger (rat (-1)))

  , testCase "rational is not integer" $
      assertBool "1/2" (not (isInteger (rat (1 % 2))))

  , testCase "extension is not integer" $
      assertBool "√2" (not (isInteger (ext2 (rat 0) (rat 1) (rat 2))))
  ]

-- -----------------------------------------------------------------------
-- 10. radicands
-- -----------------------------------------------------------------------

radicandsTests :: TestTree
radicandsTests = testGroup "radicands"
  [ testCase "rational has no radicands" $
      radicands (rat 5) @?= []

  , testCase "√3 has radicand (3, 2)" $
      radicands (ext2 (rat 0) (rat 1) (rat 3)) @?= [(3, 2)]

  , testCase "1 + 2√3 has radicand (3, 2)" $
      radicands (ext2 (rat 1) (rat 2) (rat 3)) @?= [(3, 2)]

  , testCase "cube root: ∛5 has radicand (5, 3)" $
      radicands (Ext (rat 0) (rat 1) (rat 5) 3) @?= [(5, 3)]

  , testCase "nested: a-part has √2, b-part has √3" $
      let val = Ext (ext2 (rat 0) (rat 1) (rat 2)) (rat 1) (rat 3) 2
      in  radicands val @?= [(2, 2), (3, 2)]

  , testCase "duplicates are removed" $
      let val = Ext (ext2 (rat 1) (rat 1) (rat 2)) (rat 1) (rat 2) 2
      in  radicands val @?= [(2, 2)]
  ]


-- -----------------------------------------------------------------------
-- 11. Real constructor tests
-- -----------------------------------------------------------------------

realTests :: TestTree
realTests = testGroup "Real"
  [ testCase "Real + Real" $ do
      let a = Real 1.5
          b = Real 2.5
      approxD "add" 4.0 (toDouble (a + b))

  , testCase "Real + Rational collapses to Real" $ do
      let a = Real 1.5
          b = Rational 2
          r = a + b
      approxD "add" 3.5 (toDouble r)
      case r of
        Real _ -> return ()
        _      -> assertFailure "expected Real constructor"

  , testCase "Real * Ext collapses to Real" $ do
      let a = Real 2.0
          b = Ext (Rational 1) (Rational 1) (Rational 2) 2  -- 1 + sqrt(2)
          r = a * b
      approxD "mul" (2.0 * (1 + sqrt 2)) (toDouble r)
      case r of
        Real _ -> return ()
        _      -> assertFailure "expected Real constructor"

  , testCase "toDouble (Real d) == d" $ do
      toDouble (Real 3.14) @?= 3.14

  , testCase "toKaTeX (Real d)" $ do
      toKaTeX (Real 3.14) @?= "3.14"

  , testCase "isZero (Real 0.0)" $ do
      isZero (Real 0.0) @?= True

  , testCase "isZero (Real 1.0)" $ do
      isZero (Real 1.0) @?= False

  , testCase "Real / Real" $ do
      let r = Real 6.0 / Real 2.0
      approxD "div" 3.0 (toDouble r)

  , testCase "negate (Real d)" $ do
      approxD "neg" (-2.5) (toDouble (negate (Real 2.5)))
  ]

-- -----------------------------------------------------------------------
-- 12. divR with multi-radicand denominators
-- -----------------------------------------------------------------------

divRMultiRadicandTests :: TestTree
divRMultiRadicandTests = testGroup "divR multi-radicand"
  [ testCase "divR by denom with √3 at two nesting levels" $
      -- Ext(Ext(1, 1, 3, 2), 1, 5, 2) has √3 inside √5 layer.
      -- Wrapping in another √3 layer creates duplicate √3.
      let inner = Ext (Ext (rat 1) (rat 1) (rat 3) 2) (rat 1) (rat 5) 2
          denom = Ext inner (rat 1) (rat 3) 2  -- √3 at two levels
          result = rat 1 / denom
      in approxD "1/denom" (1 / toDouble denom) (toDouble result)

  , testCase "divR by denom with √5 at two nesting levels" $
      let inner = Ext (rat 2) (rat 1) (rat 5) 2       -- 2 + √5
          mid   = Ext inner (rat 1) (rat 3) 2          -- (2+√5) + √3
          denom = Ext mid (rat 1) (rat 5) 2            -- ... + √5 (duplicate)
          result = rat 1 / denom
      in approxD "1/denom" (1 / toDouble denom) (toDouble result)

  , testCase "divR by depth-5 nested Ext with 3 radicands" $
      -- Simulates the structure from the intersectLL hang:
      -- depth 5 with radicands √3, √5, √15
      let d1 = Ext (rat 0) (rat 2) (rat 15) 2           -- 2√15
          d2 = Ext d1 (rat 4) (rat 5) 2                  -- 2√15 + 4√5
          d3 = Ext d2 (rat 1) (rat 3) 2                  -- ... + √3
          d4 = Ext d3 (rat (-2)) (rat 15) 2              -- ... - 2√15
          denom = Ext d4 (rat (-9)) (rat 3) 2             -- ... - 9√3
          result = rat 1 / denom
      in approxD "1/deep-denom" (1 / toDouble denom) (toDouble result)

  , testCase "divR preserves value (Ext numerator / multi-rad denom)" $
      let num = Ext (rat 3) (rat 1) (rat 5) 2  -- 3 + √5
          inner = Ext (rat 1) (rat 1) (rat 3) 2
          denom = Ext inner (rat 2) (rat 5) 2   -- (1+√3) + 2√5
          result = num / denom
      in approxD "num/denom" (toDouble num / toDouble denom) (toDouble result)

  , testCase "divR: consolidation eliminates spurious radicand" $
      -- Ext(Ext(0, 1, 3, 2), 1, 3, 2) = √3 + √3 = 2√3
      -- After consolidation b' = 2, and divR should produce Rational
      let denom = Ext (Ext (rat 0) (rat 1) (rat 3) 2) (rat 1) (rat 3) 2
          result = rat 1 / denom
      in approxD "1/(2√3)" (1 / toDouble denom) (toDouble result)

  --, testCase "divR: conjugate denominator zero (perfect square radicand, r2=1)" $
  --    -- Ext 1 1 1 2 = 1 + √1 = 2 (un-normalised), so 1/denom should be 1/2.
  --    -- After consolidation: a2'=1, b2'=1, r2=1.
  --    -- Conjugate denominator: a2'² - b2'²·r2 = 1 - 1·1 = 0, hitting the error branch.
  --    let denom = Ext (rat 1) (rat 1) (rat 1) 2
  --    in approxD "1/2" 0.5 (toDouble (rat 1 / denom))
  ]

-- -----------------------------------------------------------------------
-- 13. Consolidation regression tests (expression blowup prevention)
-- -----------------------------------------------------------------------

consolidationTests :: TestTree
consolidationTests = testGroup "consolidateRadicands"
  [ testCase "normalize consolidates √3 at two levels into one" $
      -- Ext(Ext(0, 1, 3, 2), 1, 3, 2) = √3 + √3 should become 2√3
      let expr = normalize (Ext (Ext (rat 0) (rat 1) (rat 3) 2) (rat 1) (rat 3) 2)
      in do radDepth expr @?= 1
            approxD "value" (2 * sqrt 3) (toDouble expr)

  , testCase "normalize consolidates √5 nested under √3" $
      -- (√5) + (√5)·√3 → with √5 at two levels
      let expr = normalize (Ext (Ext (rat 0) (rat 1) (rat 5) 2)
                                (Ext (rat 0) (rat 1) (rat 5) 2)
                                (rat 3) 2)
      in do assertBool "depth should be <= 2" (radDepth expr <= 2)
            approxD "value" (sqrt 5 + sqrt 5 * sqrt 3) (toDouble expr)

  , testCase "addR with different radicands doesn't blow up" $
      -- (1+2√3)+3√5 added to itself should stay small
      let e = Ext (Ext (rat 1) (rat 2) (rat 3) 2) (rat 3) (rat 5) 2
          r = e + e
      in do assertBool ("depth <= 2, got " ++ show (radDepth r)) (radDepth r <= 2)
            assertBool ("size <= 10, got " ++ show (radSize r)) (radSize r <= 10)
            approxD "value" (2 * toDouble e) (toDouble r)

  , testCase "mulR with different radicands doesn't blow up" $
      -- (√3+√5)² should stay small
      let e = Ext (Ext (rat 0) (rat 1) (rat 3) 2) (rat 1) (rat 5) 2
          r = e * e
      in do assertBool ("depth <= 2, got " ++ show (radDepth r)) (radDepth r <= 2)
            assertBool ("size <= 15, got " ++ show (radSize r)) (radSize r <= 15)
            approxD "value" ((sqrt 3 + sqrt 5) ** 2) (toDouble r)

  , testCase "repeated squaring doesn't blow up" $
      -- ((1+2√3)+3√5)^8 via repeated squaring should stay bounded
      let e = Ext (Ext (rat 1) (rat 2) (rat 3) 2) (rat 3) (rat 5) 2
          e2 = e * e
          e4 = e2 * e2
          e8 = e4 * e4
      in do assertBool ("depth <= 3, got " ++ show (radDepth e8)) (radDepth e8 <= 3)
            assertBool ("size <= 20, got " ++ show (radSize e8)) (radSize e8 <= 20)

  , testCase "sqrt of multi-radicand expression stays small" $
      let e = Ext (Ext (rat 1) (rat 2) (rat 3) 2) (rat 3) (rat 5) 2
          r = sqrt e
      in do assertBool ("size <= 15, got " ++ show (radSize r)) (radSize r <= 15)
            approxD "value" (sqrt (toDouble e)) (toDouble r)

  , testCase "chained mul/div with multi-radicand stays small" $
      let e1 = Ext (Ext (rat 1) (rat 2) (rat 3) 2) (rat 3) (rat 5) 2
          e2 = Ext (Ext (rat 0) (rat 1) (rat 3) 2) (rat 1) (rat 5) 2
          r = (e1 * e1) / e2
      in do assertBool ("size <= 20, got " ++ show (radSize r)) (radSize r <= 20)
            approxD "value" (toDouble e1 ** 2 / toDouble e2) (toDouble r)

  , testCase "three-level consolidation: √3+√3+√3 = 3√3" $
      -- Ext(Ext(Ext(0,1,3,2), 1, 3, 2), 1, 3, 2) — three nested √3 layers
      let expr = normalize $
            Ext (Ext (Ext (rat 0) (rat 1) (rat 3) 2) (rat 1) (rat 3) 2)
                (rat 1) (rat 3) 2
      in do radDepth expr @?= 1
            approxD "value" (3 * sqrt 3) (toDouble expr)

  , testCase "different radicands don't merge: √2+√3 stays depth 2" $
      -- √2 and √3 are distinct radicands; consolidation must not spuriously merge them
      let expr = normalize (Ext (Ext (rat 0) (rat 1) (rat 2) 2) (rat 1) (rat 3) 2)
      in do assertBool ("depth should be 2, got " ++ show (radDepth expr)) (radDepth expr == 2)
            approxD "value" (sqrt 2 + sqrt 3) (toDouble expr)

  , testCase "consolidation: √r - √r = 0" $
      -- Ext(0, 1, 5, 2) - Ext(0, 1, 5, 2) must consolidate to 0
      let e = Ext (rat 0) (rat 1) (rat 5) 2
          r = e - e
      in approxD "value" 0.0 (toDouble r)

  , testCase "rationalisation: (2+√3)·(2-√3) = 1" $
      -- Multiplying conjugates should consolidate to a rational
      let pos = Ext (rat 2) (rat 1) (rat 3) 2
          neg = Ext (rat 2) (rat (-1)) (rat 3) 2
          r   = pos * neg
      in do assertBool "result should be rational" (isRational r)
            approxD "value" 1.0 (toDouble r)

  , testCase "consolidation after division: (√3+√3)/√3 = 2" $
      -- (√3+√3) = 2√3, so (√3+√3)/√3 = 2
      let num   = Ext (Ext (rat 0) (rat 1) (rat 3) 2) (rat 1) (rat 3) 2
          denom = Ext (rat 0) (rat 1) (rat 3) 2
          r     = num / denom
      in do assertBool "result should be rational" (isRational r)
            approxD "value" 2.0 (toDouble r)

  , testCase "regression" $
      assertEqual "value" (Rational 2) (normalize (Ext (rat 1) (rat 1) (rat 1) 2))

  , testProperty "normalize is value-preserving" $
      forAll genSimpleRadical $ \x ->
        let d1 = toDouble x
            d2 = toDouble (normalize x)
        in not (isNaN d1 || isNaN d2 || isInfinite d1 || isInfinite d2) ==>
           abs (d1 - d2) < 1e-9

  , testProperty "x + x has correct value" $
      forAll genSimpleRadical $ \x ->
        let d  = toDouble x
            r  = x + x
        in not (isNaN d || isInfinite d) ==>
           abs (toDouble r - 2 * d) < 1e-9

  , testProperty "x + x stays compact" $
      forAll genSimpleRadical $ \x ->
        radSize (x + x) <= radSize x * 2 + 4

  , testProperty "x * x has correct value" $
      forAll genSimpleRadical $ \x ->
        let d = toDouble x
            r = x * x
        in not (isNaN d || isInfinite d) ==>
           abs (toDouble r - d * d) < 1e-6

  , testProperty "x * x stays compact" $
      forAll genSimpleRadical $ \x ->
        radSize (x * x) <= 30
  ]

-- | Generator for simple Radical values suitable for consolidation property tests.
-- Produces rationals and single/double square-root extensions with small coefficients.
genSimpleRadical :: Gen Radical
genSimpleRadical = frequency
  [ (3, fmap rat genSmallRat)
  , (4, do a <- fmap rat genSmallRat
           b <- fmap rat genSmallRat
           r <- elements [2, 3, 5, 6, 7]
           return (Ext a b (rat r) 2))
  , (3, do a1 <- fmap rat genSmallRat
           b1 <- fmap rat genSmallRat
           r1 <- elements [2, 3, 5]
           b2 <- fmap rat genSmallRat
           r2 <- elements [2, 3, 5]
           return (Ext (Ext a1 b1 (rat r1) 2) b2 (rat r2) 2))
  ]

genSmallRat :: Gen Rational
genSmallRat = oneof
  [ fmap fromIntegral (elements ([-3..3] :: [Int]))
  , do n <- elements ([1..4] :: [Int])
       d <- elements ([1..4] :: [Int])
       return (fromIntegral n % fromIntegral d)
  ]

-- -----------------------------------------------------------------------
-- 14. Root index reduction
--
-- Strategy: when a radicand r = p^k and gcd(k, n) = g > 1, reduce the
-- root index.  E.g. 4^(1/4) = (2^2)^(1/4) = 2^(2/4) = 2^(1/2) = √2.
--
-- Currently factorRadical only strips *complete* nth powers (so 4 under
-- a 4th root is unchanged), and nthPowerFree 4 4 = (1,4).  No GCD-based
-- index reduction is applied, so these all fail.
-- -----------------------------------------------------------------------

rootIndexReductionTests :: TestTree
rootIndexReductionTests = testGroup "Root index reduction"
  [ testCase "∜4 = √2  (4 = 2², gcd(2,4) = 2)" $
      -- 4^(1/4) = (2^2)^(1/4) = 2^(1/2)
      radicalEq "∜4" (sqrtC (rat 2)) (nthRootC 4 (rat 4))

  , testCase "∜9 = √3  (9 = 3², gcd(2,4) = 2)" $
      -- 9^(1/4) = (3^2)^(1/4) = 3^(1/2)
      radicalEq "∜9" (sqrtC (rat 3)) (nthRootC 4 (rat 9))

  , testCase "8^(1/6) = √2  (8 = 2³, gcd(3,6) = 3)" $
      -- 8^(1/6) = (2^3)^(1/6) = 2^(3/6) = 2^(1/2)
      radicalEq "8^(1/6)" (sqrtC (rat 2)) (nthRootC 6 (rat 8))

  , testCase "4^(1/6) = ∛2  (4 = 2², gcd(2,6) = 2)" $
      -- 4^(1/6) = (2^2)^(1/6) = 2^(2/6) = 2^(1/3)
      radicalEq "4^(1/6)" (nthRootC 3 (rat 2)) (nthRootC 6 (rat 4))
  ]

-- -----------------------------------------------------------------------
-- 15. Nested root composition
--
-- Strategy: Ext 0 b (Ext 0 1 r n) m  =  b · (r^(1/n))^(1/m)
--                                     =  b · r^(1/(m·n))
--         = Ext 0 b r (m*n)
--
-- Currently sqrtC (sqrtC (rat 2)) normalises to Ext 0 1 (Ext 0 1 2 2) 2
-- while nthRootC 4 (rat 2) = Ext 0 1 2 4.  Neither step unifies them.
-- -----------------------------------------------------------------------

nestedRootCompositionTests :: TestTree
nestedRootCompositionTests = testGroup "Nested root composition"
  [ testCase "√(√2) and ∜2 are structurally equal after normalize" $
      -- Ext 0 1 (Ext 0 1 2 2) 2  should reduce to  Ext 0 1 2 4
      assertBool "√(√2) radEq ∜2"
        (normalize (sqrtC (sqrtC (rat 2))) `radEq` normalize (nthRootC 4 (rat 2)))

  , testCase "√(∛2) and 2^(1/6) are structurally equal after normalize" $
      -- Ext 0 1 (Ext 0 1 2 3) 2  should reduce to  Ext 0 1 2 6
      assertBool "√(∛2) radEq 2^(1/6)"
        (normalize (sqrtC (nthRootC 3 (rat 2))) `radEq` normalize (nthRootC 6 (rat 2)))

  , testCase "∜5 == √(√5) via Eq" $
      -- nthRootC 4 5 = Ext 0 1 5 4
      -- sqrtC(sqrtC 5) = Ext 0 1 (Ext 0 1 5 2) 2
      -- Currently these are structurally different so Eq returns False.
      assertBool "∜5 == √(√5)" (nthRootC 4 (rat 5) == sqrtC (sqrtC (rat 5)))

  , testCase "∜4 == √(√4) — combines index reduction and composition" $
      -- √(√4) = √2 (since √4 = 2), and ∜4 = √2 by index reduction.
      -- Both should be the same canonical form.
      assertBool "∜4 == √(√4)" (nthRootC 4 (rat 4) == sqrtC (sqrtC (rat 4)))
  ]

-- -----------------------------------------------------------------------
-- 16. Canonical ordering of radicands
--
-- Strategy: impose a total order on radicands so that addition is
-- structurally commutative after normalization.  E.g. always place the
-- larger radicand at the outermost Ext layer.
--
-- Currently addR nests the left argument inside, so
--   √2 + √3  →  Ext (Ext 0 1 3 2) 1 2 2   (outer: √2, inner: √3)
--   √3 + √2  →  Ext (Ext 0 1 2 2) 1 3 2   (outer: √3, inner: √2)
-- radEq returns False on these, so the Eq instance is not commutative.
-- -----------------------------------------------------------------------

canonicalOrderingTests :: TestTree
canonicalOrderingTests = testGroup "Canonical ordering"
  []
  -- TODO: These introduce perf regression (possible lopp too?) in other test cases
  --[ testCase "√2 + √3 == √3 + √2  (Eq must be commutative)" $
  --    assertBool "commutative add"
  --      (sqrtC (rat 2) + sqrtC (rat 3) == sqrtC (rat 3) + sqrtC (rat 2))

  --, testCase "normalize(√2+√3) `radEq` normalize(√3+√2)" $
  --    assertBool "canonical structure"
  --      (normalize (sqrtC (rat 2) + sqrtC (rat 3))
  --        `radEq`
  --       normalize (sqrtC (rat 3) + sqrtC (rat 2)))

  --, testCase "√2 + √3 + √5 == √5 + √3 + √2  (Eq, three radicands)" $
  --    let lhs = sqrtC (rat 2) + sqrtC (rat 3) + sqrtC (rat 5)
  --        rhs = sqrtC (rat 5) + sqrtC (rat 3) + sqrtC (rat 2)
  --    in assertBool "commutative three" (lhs == rhs)

  --, testCase "(1 + √2) * (1 + √3) == (1 + √3) * (1 + √2)  (Eq)" $
  --    let a = rat 1 + sqrtC (rat 2)
  --        b = rat 1 + sqrtC (rat 3)
  --    in assertBool "mul commutative" (a * b == b * a)
  --]
