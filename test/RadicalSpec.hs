module RadicalSpec (radicalTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))

import Flag.Construction.Radical

radicalTests :: TestTree
radicalTests = testGroup "Radical"
  [ normalisationTests
  , arithmeticTests
  , sqrtTests
  , nthRootTests
  , toDoubleTests
  , toKaTeXTests
  , comparisonTests
  , isNaturalTests
  , isIntegerTests
  , radicandsTests
  , minPolyExtTests
  , minPolyArithmeticTests
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

-- -----------------------------------------------------------------------
-- 1. Normalisation
-- -----------------------------------------------------------------------

normalisationTests :: TestTree
normalisationTests = testGroup "Normalisation"
  [ testCase "Ext a 0 r 2 collapses to a" $
      radicalEq "zero coeff" (rat 5) (normalize (Ext (rat 5) (rat 0) (rat 3) 2))

  , testCase "Ext 0 1 4 2 collapses to Rational 2" $
      isRat "√4" 2 (normalize (Ext (rat 0) (rat 1) (rat 4) 2))

  , testCase "Ext 0 1 12 2 normalises to Ext 0 2 3 2" $
      radicalEq "√12 = 2√3"
        (Ext (rat 0) (rat 2) (rat 3) 2)
        (normalize (Ext (rat 0) (rat 1) (rat 12) 2))

  , testCase "Rational 0 + Rational 0 = Rational 0" $
      isRat "0+0" 0 (rat 0 + rat 0)

  , testCase "Deeply nested: Ext (Ext 0 0 2 2) 1 3 2 collapses" $
      radicalEq "nested zero"
        (Ext (rat 0) (rat 1) (rat 3) 2)
        (normalize (Ext (Ext (rat 0) (rat 0) (rat 2) 2) (rat 1) (rat 3) 2))

  , testCase "Ext 0 1 9 2 collapses to Rational 3" $
      isRat "√9" 3 (normalize (Ext (rat 0) (rat 1) (rat 9) 2))

  , testCase "Ext 0 1 (1/4) 2 collapses to Rational (1/2)" $
      isRat "√(1/4)" (1 % 2) (normalize (Ext (rat 0) (rat 1) (rat (1 % 4)) 2))

  , testCase "Ext 0 1 (9/4) 2 collapses to Rational (3/2)" $
      isRat "√(9/4)" (3 % 2) (normalize (Ext (rat 0) (rat 1) (rat (9 % 4)) 2))

  , testCase "Cube root: Ext 0 1 8 3 collapses to Rational 2" $
      isRat "∛8" 2 (normalize (Ext (rat 0) (rat 1) (rat 8) 3))

  , testCase "Cube root canonical: Ext 0 1 24 3 normalises to Ext 0 2 3 3" $
      radicalEq "∛24 = 2∛3"
        (Ext (rat 0) (rat 2) (rat 3) 3)
        (normalize (Ext (rat 0) (rat 1) (rat 24) 3))

  , testCase "Ext with zero radicand collapses" $
      isRat "0 radicand" 5 (normalize (Ext (rat 5) (rat 3) (rat 0) 2))
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
-- 11. MinPolyExt / Chebyshev / Heptagon smoke tests
-- -----------------------------------------------------------------------

minPolyExtTests :: TestTree
minPolyExtTests = testGroup "MinPolyExt"
  [ testCase "chebyshev T_2 gives cos(4pi/7)" $ do
      let mp = cosMinPoly 7
          x = MinPolyExt mp (chebyshevT mp 2)
      approxD "P2.x" (cos (4 * pi / 7)) (toDouble x)

  , testCase "heptagon y coordinate uses sqrt(1-c^2)" $ do
      let mp = cosMinPoly 7
          oneMinusC2 = MinPolyExt mp [1, 0, -1]
          yCoeff = MinPolyExt mp (chebyshevU mp 1)
          y = Ext (Rational 0) yCoeff oneMinusC2 2
      approxD "P2.y" (sin (4 * pi / 7)) (toDouble y)

  , testCase "sin^2 = 1 - c^2 for heptagon" $ do
      let mp = cosMinPoly 7
          oneMinusC2 = MinPolyExt mp [1, 0, -1]
          sinVal = Ext (Rational 0) (Rational 1) oneMinusC2 2
          sinSq = sinVal * sinVal
      approxD "sin^2" (sin (2 * pi / 7) ^ 2) (toDouble sinSq)

  , testCase "fieldLabels picks up MinPolyExt label" $ do
      let mp = cosMinPoly 7
      fieldLabels (MinPolyExt mp [0,1,0]) @?= [mpLabel mp]
  ]


-- -----------------------------------------------------------------------
-- 12. MinPolyExt arithmetic (tests first)
-- -----------------------------------------------------------------------

minPolyArithmeticTests :: TestTree
minPolyArithmeticTests = testGroup "MinPolyExtArithmetic"
  [ testCase "addition of MinPolyExt vectors" $ do
      let mp = cosMinPoly 7
          a = MinPolyExt mp [1,0,0]
          b = MinPolyExt mp [0,1,0]
          expected = MinPolyExt mp [1,1,0]
      approxD "add coeffs" (toDouble expected) (toDouble (a + b))

  , testCase "scale MinPolyExt by rational" $ do
      let mp = cosMinPoly 7
          a = MinPolyExt mp [1,2,0]
          res = a * Rational 2
          expected = MinPolyExt mp [2,4,0]
      approxD "scale" (toDouble expected) (toDouble res)

  , testCase "division by rational" $ do
      let mp = cosMinPoly 7
          a = MinPolyExt mp [1,2,0]
          res = a / Rational 2
          expected = MinPolyExt mp [1 % 2,1,0]
      approxD "div by rat" (toDouble expected) (toDouble res)

  , testCase "division MinPolyExt / MinPolyExt (numeric)" $ do
      let mp = cosMinPoly 7
          a = MinPolyExt mp [1,0,0]
          b = MinPolyExt mp [0,1,0]
          res = a / b
      approxD "div fields" (toDouble a / toDouble b) (toDouble res)
  ]
