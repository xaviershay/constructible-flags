module FieldNumberSpec (fieldNumberTests) where

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ratio ((%))
import Flag.Construction.FieldNumber
import Test.QuickCheck (Gen, choose, elements, frequency, suchThat)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (forAll, testProperty, (==>))

fieldNumberTests :: TestTree
fieldNumberTests =
  testGroup
    "FieldNumber"
    [ constructionTests,
      trigWrapperTests,
      arithmeticFieldTests,
      sqrtTests,
      predicateTests,
      eqOrdTests,
      numericCorrectnessTests,
      toRationalTests,
      displayTests,
      properties
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

assertNear :: String -> Double -> Double -> Assertion
assertNear msg expected actual =
  assertBool
    (msg ++ ": expected ~" ++ show expected ++ " got " ++ show actual)
    (abs (expected - actual) < 1e-9)

assertField :: String -> Field -> FieldNumber -> Assertion
assertField msg expected fn =
  assertEqual (msg ++ " field") expected (fieldOf fn)

-- ---------------------------------------------------------------------------
-- 1. Construction
-- ---------------------------------------------------------------------------

constructionTests :: TestTree
constructionTests =
  testGroup
    "Construction"
    [ testCase "fnInteger 0 has field FInteger" $
        assertField "0" FInteger (fnInteger 0),
      testCase "fnInteger 42 has field FInteger" $
        assertField "42" FInteger (fnInteger 42),
      testCase "fnInteger (-3) has field FInteger" $
        assertField "-3" FInteger (fnInteger (-3)),
      testCase "fnInteger 5 has value 5.0" $
        getValue (fnInteger 5) @?= 5.0,
      testCase "fnRational (1%2) has field FRational" $
        assertField "1/2" FRational (fnRational (1 % 2)),
      testCase "fnRational (3%1) has field FInteger (whole number)" $
        assertField "3%1" FInteger (fnRational (3 % 1)),
      testCase "fnRational (-6%1) has field FInteger" $
        assertField "-6%1" FInteger (fnRational ((-6) % 1)),
      testCase "fnRational (1%3) has value ~0.333" $
        assertNear "1/3" (1 / 3) (getValue (fnRational (1 % 3))),
      testCase "fromInteger produces FInteger" $
        assertField "fromInteger 5" FInteger (5 :: FieldNumber),
      testCase "fromRational (1%3) produces FRational" $
        assertField "fromRational 1/3" FRational (fromRational (1 % 3) :: FieldNumber),
      testCase "fromRational (6%2) produces FInteger" $
        assertField "fromRational 6/2" FInteger (fromRational (6 % 2) :: FieldNumber),
      testCase "fromRational (0%1) produces FInteger" $
        assertField "fromRational 0" FInteger (fromRational (0 % 1) :: FieldNumber)
    ]

-- ---------------------------------------------------------------------------
-- 2. Trig wrappers
-- ---------------------------------------------------------------------------

trigWrapperTests :: TestTree
trigWrapperTests =
  testGroup
    "Trig wrappers"
    [ testCase "fnCos result has field FCyclomatic" $
        assertField "fnCos" FCyclomatic (fnCos (fnInteger 1)),
      testCase "fnSin result has field FCyclomatic" $
        assertField "fnSin" FCyclomatic (fnSin (fnInteger 1)),
      testCase "fnCos of FRational gives FCyclomatic" $
        assertField "fnCos FRational" FCyclomatic (fnCos (fnRational (1 % 4))),
      testCase "fnSin of FIrrational gives FCyclomatic" $
        assertField "fnSin FIrrational" FCyclomatic (fnSin (sqrt (fnInteger 2))),
      testCase "cos via Floating instance gives FCyclomatic" $
        assertField "cos" FCyclomatic (cos (fnInteger 1 :: FieldNumber)),
      testCase "sin via Floating instance gives FCyclomatic" $
        assertField "sin" FCyclomatic (sin (fnRational (1 % 4) :: FieldNumber)),
      testCase "tan gives FCyclomatic" $
        assertField "tan" FCyclomatic (tan (fnInteger 1 :: FieldNumber)),
      testCase "asin gives FCyclomatic" $
        assertField "asin" FCyclomatic (asin (fnRational (1 % 2) :: FieldNumber)),
      testCase "acos gives FCyclomatic" $
        assertField "acos" FCyclomatic (acos (fnRational (1 % 2) :: FieldNumber)),
      testCase "atan gives FCyclomatic" $
        assertField "atan" FCyclomatic (atan (fnInteger 1 :: FieldNumber)),
      testCase "exp gives FReal" $
        assertField "exp" FReal (exp (fnInteger 1 :: FieldNumber)),
      testCase "log gives FReal" $
        assertField "log" FReal (log (fnInteger 2 :: FieldNumber)),
      testCase "pi has field FReal" $
        assertField "pi" FReal (pi :: FieldNumber),
      testCase "pi value is approximately 3.14159265358979" $
        assertNear "pi" 3.141592653589793 (getValue (pi :: FieldNumber)),
      testCase "fnCos (fnInteger 0) ≈ 1.0" $
        assertNear "cos(0)" 1.0 (getValue (fnCos (fnInteger 0))),
      testCase "fnSin (fnInteger 0) ≈ 0.0" $
        assertNear "sin(0)" 0.0 (getValue (fnSin (fnInteger 0))),
      testCase "fnCos (pi) ≈ -1.0" $
        assertNear "cos(pi)" (-1.0) (getValue (fnCos (pi :: FieldNumber)))
    ]

-- ---------------------------------------------------------------------------
-- 3. Arithmetic field promotion
-- ---------------------------------------------------------------------------

arithmeticFieldTests :: TestTree
arithmeticFieldTests =
  testGroup
    "Arithmetic field promotion"
    [ testGroup
        "Addition"
        [ testCase "FInteger + FInteger -> FInteger" $
            assertField "i+i" FInteger (fnInteger 2 + fnInteger 3),
          testCase "FInteger + FRational -> FRational" $
            assertField "i+r" FRational (fnInteger 1 + fnRational (1 % 3)),
          testCase "FRational + FIrrational -> FIrrational" $
            assertField "r+irr" FIrrational (fnRational (1 % 2) + sqrt (fnInteger 2)),
          testCase "FIrrational + FCyclomatic -> FCyclomatic" $
            assertField "irr+cyc" FCyclomatic (sqrt (fnInteger 2) + fnCos (fnInteger 1)),
          testCase "FCyclomatic + FReal -> FReal" $
            assertField "cyc+real" FReal (fnCos (fnInteger 1) + (pi :: FieldNumber))
        ],
      testGroup
        "Subtraction"
        [ testCase "FInteger - FInteger -> FInteger" $
            assertField "i-i" FInteger (fnInteger 5 - fnInteger 3),
          testCase "FRational - FRational -> FRational" $
            assertField "r-r" FRational (fnRational (1 % 3) - fnRational (1 % 6)),
          testCase "FIrrational - FIrrational -> FIrrational" $
            assertField "irr-irr" FIrrational (sqrt (fnInteger 2) - sqrt (fnInteger 3))
        ],
      testGroup
        "Multiplication"
        [ testCase "FInteger * FInteger -> FInteger" $
            assertField "i*i" FInteger (fnInteger 3 * fnInteger 4),
          testCase "FRational * FIrrational -> FIrrational" $
            assertField "r*irr" FIrrational (fnRational (1 % 2) * sqrt (fnInteger 3)),
          testCase "FIrrational * FCyclomatic -> FCyclomatic" $
            assertField "irr*cyc" FCyclomatic (sqrt (fnInteger 2) * fnCos (fnInteger 1)),
          testCase "FIrrational * FReal -> FReal" $
            assertField "irr*real" FReal (sqrt (fnInteger 2) * (pi :: FieldNumber))
        ],
      testGroup
        "Division"
        [ testCase "FInteger / FInteger -> FInteger (exact: 6/3=2)" $
            assertField "6/3" FInteger (fnInteger 6 / fnInteger 3),
          testCase "FInteger / FInteger -> FRational (inexact: 1/3)" $
            assertField "1/3" FRational (fnInteger 1 / fnInteger 3),
          testCase "FRational / FRational -> FRational" $
            assertField "r/r" FRational (fnRational (1 % 3) / fnRational (1 % 2)),
          testCase "FRational / FInteger -> FRational (2/3 / 1 = 2/3)" $
            assertField "r/i" FRational (fnRational (2 % 3) / fnInteger 1),
          testCase "FIrrational / FIrrational -> FIrrational" $
            assertField "irr/irr" FIrrational (sqrt (fnInteger 2) / sqrt (fnInteger 3)),
          testCase "FReal / FReal -> FReal" $
            assertField "real/real" FReal ((pi :: FieldNumber) / (pi :: FieldNumber))
        ],
      testGroup
        "Negate / abs / signum"
        [ testCase "negate FInteger -> FInteger" $
            assertField "neg i" FInteger (negate (fnInteger 3)),
          testCase "negate FIrrational -> FIrrational" $
            assertField "neg irr" FIrrational (negate (sqrt (fnInteger 2))),
          testCase "abs FRational -> FRational" $
            assertField "abs r" FRational (abs (fnRational ((-1) % 3))),
          testCase "signum FIrrational -> FInteger (result is always -1, 0, or 1)" $
            assertField "signum irr" FInteger (signum (sqrt (fnInteger 2))),
          testCase "signum FReal -> FInteger" $
            assertField "signum real" FInteger (signum (pi :: FieldNumber))
        ]
    ]

-- ---------------------------------------------------------------------------
-- 4. sqrt field rules
-- ---------------------------------------------------------------------------

sqrtTests :: TestTree
sqrtTests =
  testGroup
    "sqrt"
    [ testGroup
        "Perfect-square integers -> FInteger"
        [ testCase "sqrt(fnInteger 0) -> FInteger 0" $ do
            let r = sqrt (fnInteger 0)
            assertField "sqrt(0)" FInteger r
            assertNear "value" 0.0 (getValue r),
          testCase "sqrt(fnInteger 1) -> FInteger 1" $ do
            let r = sqrt (fnInteger 1)
            assertField "sqrt(1)" FInteger r
            assertNear "value" 1.0 (getValue r),
          testCase "sqrt(fnInteger 4) -> FInteger 2" $ do
            let r = sqrt (fnInteger 4)
            assertField "sqrt(4)" FInteger r
            assertNear "value" 2.0 (getValue r),
          testCase "sqrt(fnInteger 9) -> FInteger 3" $ do
            let r = sqrt (fnInteger 9)
            assertField "sqrt(9)" FInteger r
            assertNear "value" 3.0 (getValue r),
          testCase "sqrt(fnInteger 100) -> FInteger 10" $ do
            let r = sqrt (fnInteger 100)
            assertField "sqrt(100)" FInteger r
            assertNear "value" 10.0 (getValue r)
        ],
      testGroup
        "Non-perfect-square integers -> FIrrational"
        [ testCase "sqrt(fnInteger 2) -> FIrrational" $
            assertField "sqrt(2)" FIrrational (sqrt (fnInteger 2)),
          testCase "sqrt(fnInteger 3) -> FIrrational" $
            assertField "sqrt(3)" FIrrational (sqrt (fnInteger 3)),
          testCase "sqrt(fnInteger 5) -> FIrrational" $
            assertField "sqrt(5)" FIrrational (sqrt (fnInteger 5)),
          testCase "sqrt(fnInteger 7) -> FIrrational" $
            assertField "sqrt(7)" FIrrational (sqrt (fnInteger 7)),
          testCase "sqrt(fnInteger 2) value ≈ 1.41421" $
            assertNear "sqrt(2)" (sqrt 2) (getValue (sqrt (fnInteger 2)))
        ],
      testGroup
        "Perfect-square rationals -> FRational"
        [ testCase "sqrt(fnRational (1%4)) -> FRational, value 0.5" $ do
            let r = sqrt (fnRational (1 % 4))
            assertField "sqrt(1/4)" FRational r
            assertNear "value" 0.5 (getValue r),
          testCase "sqrt(fnRational (9%4)) -> FRational, value 1.5" $ do
            let r = sqrt (fnRational (9 % 4))
            assertField "sqrt(9/4)" FRational r
            assertNear "value" 1.5 (getValue r),
          testCase "sqrt(fnRational (4%9)) -> FRational, value 2/3" $ do
            let r = sqrt (fnRational (4 % 9))
            assertField "sqrt(4/9)" FRational r
            assertNear "value" (2 / 3) (getValue r)
        ],
      testGroup
        "Non-perfect-square rationals -> FIrrational"
        [ testCase "sqrt(fnRational (1%2)) -> FIrrational" $
            assertField "sqrt(1/2)" FIrrational (sqrt (fnRational (1 % 2))),
          testCase "sqrt(fnRational (2%3)) -> FIrrational" $
            assertField "sqrt(2/3)" FIrrational (sqrt (fnRational (2 % 3)))
        ],
      testGroup
        "Higher fields: field unchanged"
        [ testCase "sqrt(FIrrational) stays FIrrational" $
            assertField "sqrt(sqrt(2))" FIrrational (sqrt (sqrt (fnInteger 2))),
          testCase "sqrt(FCyclomatic) stays FCyclomatic" $
            assertField "sqrt(cos(1))" FCyclomatic (sqrt (abs (fnCos (fnInteger 1)))),
          testCase "sqrt(FReal) stays FReal" $
            assertField "sqrt(pi)" FReal (sqrt (pi :: FieldNumber))
        ],
      testGroup
        "sqrt value correctness"
        [ testCase "sqrt(2) * sqrt(2) ≈ 2" $
            assertNear "sq" 2.0 (getValue (sqrt (fnInteger 2) * sqrt (fnInteger 2))),
          testCase "sqrt(3) * sqrt(3) ≈ 3" $
            assertNear "sq" 3.0 (getValue (sqrt (fnInteger 3) * sqrt (fnInteger 3)))
        ]
    ]

-- ---------------------------------------------------------------------------
-- 5. Predicates
-- ---------------------------------------------------------------------------

predicateTests :: TestTree
predicateTests =
  testGroup
    "Predicates"
    [ testGroup
        "isZero"
        [ testCase "isZero: fnInteger 0 -> True" $
            isZero (fnInteger 0) @?= True,
          testCase "isZero: fnInteger 1 -> False" $
            isZero (fnInteger 1) @?= False,
          testCase "isZero: 1e-20 is zero (below threshold)" $
            isZero (FieldNumber FReal 1e-20) @?= True,
          testCase "isZero: 1e-14 is not zero (above threshold)" $
            isZero (FieldNumber FReal 1e-14) @?= False
        ],
      testGroup
        "isInteger"
        [ testCase "isInteger: FInteger -> True" $
            isInteger (fnInteger 5) @?= True,
          testCase "isInteger: FRational -> False" $
            isInteger (fnRational (1 % 3)) @?= False,
          testCase "isInteger: FIrrational -> False" $
            isInteger (sqrt (fnInteger 2)) @?= False
        ],
      testGroup
        "isNatural"
        [ testCase "isNatural: fnInteger 0 -> True" $
            isNatural (fnInteger 0) @?= True,
          testCase "isNatural: fnInteger 7 -> True" $
            isNatural (fnInteger 7) @?= True,
          testCase "isNatural: fnInteger (-1) -> False" $
            isNatural (fnInteger (-1)) @?= False,
          testCase "isNatural: FRational -> False" $
            isNatural (fnRational (1 % 2)) @?= False
        ],
      testGroup
        "isRational"
        [ testCase "isRational: FInteger -> True" $
            isRational (fnInteger 3) @?= True,
          testCase "isRational: FRational -> True" $
            isRational (fnRational (1 % 3)) @?= True,
          testCase "isRational: FIrrational -> False" $
            isRational (sqrt (fnInteger 2)) @?= False,
          testCase "isRational: FCyclomatic -> False" $
            isRational (fnCos (fnInteger 1)) @?= False
        ],
      testGroup
        "isIrrational"
        [ testCase "isIrrational: FIrrational -> True" $
            isIrrational (sqrt (fnInteger 2)) @?= True,
          testCase "isIrrational: FInteger -> False" $
            isIrrational (fnInteger 2) @?= False,
          testCase "isIrrational: FCyclomatic -> False" $
            isIrrational (fnCos (fnInteger 1)) @?= False
        ]
    ]

-- ---------------------------------------------------------------------------
-- 6. Eq and Ord
-- ---------------------------------------------------------------------------

eqOrdTests :: TestTree
eqOrdTests =
  testGroup
    "Eq and Ord"
    [ testCase "Eq: same value, same field" $
        fnInteger 3 @?= fnInteger 3,
      testCase "Eq: same value, different field (field tag ignored)" $
        fnInteger 2 @?= fnRational (2 % 1),
      testCase "Eq: sqrt(2)^2 ≈ 2" $
        assertBool "sqrt2 sq" (sqrt (fnInteger 2) * sqrt (fnInteger 2) == fnInteger 2),
      testCase "Eq: clearly different values are not equal" $
        assertBool "neq" (fnInteger 1 /= fnInteger 2),
      testCase "Ord: 1 < 2" $
        assertBool "lt" (fnInteger 1 < fnInteger 2),
      testCase "Ord: sqrt(2) < 2" $
        assertBool "sqrt2 lt 2" (sqrt (fnInteger 2) < fnInteger 2),
      testCase "Ord: negative < positive" $
        assertBool "neg lt pos" (fnInteger (-1) < fnInteger 1),
      testCase "Ord: compare (fnInteger 1) (fnInteger 1) == EQ" $
        compare (fnInteger 1) (fnInteger 1) @?= EQ,
      testCase "Ord: compare is consistent with Eq" $
        assertBool "compare eq" (compare (fnInteger 3) (fnInteger 3) == EQ),
      testCase "Ord: compare 0 1 == LT" $
        compare (fnInteger 0) (fnInteger 1) @?= LT,
      testCase "Ord: compare 1 0 == GT" $
        compare (fnInteger 1) (fnInteger 0) @?= GT
    ]

-- ---------------------------------------------------------------------------
-- 7. Numeric correctness
-- ---------------------------------------------------------------------------

numericCorrectnessTests :: TestTree
numericCorrectnessTests =
  testGroup
    "Numeric correctness"
    [ testCase "(1 + sqrt(2))^2 ≈ 3 + 2*sqrt(2)" $ do
        let x = 1 + sqrt (fnInteger 2)
            expected = 3 + 2 * sqrt 2
        assertNear "sq" expected (getValue (x * x)),
      testCase "Quadratic x^2 - 5x + 6 = 0: roots are FInteger 2 and 3" $ do
        let b = fnInteger (-5)
            c = fnInteger 6
            disc = sqrt (b * b - 4 * c)
            x1 = (-b - disc) / 2
            x2 = (-b + disc) / 2
        assertNear "root1" 2.0 (getValue x1)
        assertNear "root2" 3.0 (getValue x2)
        assertField "root1 field" FInteger x1
        assertField "root2 field" FInteger x2,
      testCase "Quadratic x^2 - 2 = 0: root is FIrrational" $ do
        let disc = sqrt (fnInteger 8)
            x = disc / 2
        assertNear "root" (sqrt 2) (getValue x)
        assertField "field" FIrrational x,
      testCase "Golden ratio (1+sqrt(5))/2 is FIrrational" $ do
        let phi = (1 + sqrt (fnInteger 5)) / 2
        assertField "phi field" FIrrational phi
        assertNear "phi value" 1.6180339887498949 (getValue phi),
      testCase "NGon vertex: theta = 2*pi/7 is FReal (pi promotes)" $ do
        let theta = 2 * (pi :: FieldNumber) / fnInteger 7
        assertField "theta" FReal theta,
      testCase "NGon vertex: cos(2*pi/7) is FCyclomatic" $ do
        let theta = 2 * (pi :: FieldNumber) / fnInteger 7
        assertField "cos(2pi/7)" FCyclomatic (cos theta),
      testCase "Pythagorean triple: sqrt(3^2 + 4^2) = FInteger 5" $ do
        let h = sqrt (fnInteger 9 + fnInteger 16)
        assertField "hyp field" FInteger h
        assertNear "hyp value" 5.0 (getValue h),
      testCase "Arithmetic value: (2 + sqrt(3)) * (2 - sqrt(3)) ≈ 1" $
        assertNear "conj" 1.0 (getValue ((2 + sqrt (fnInteger 3)) * (2 - sqrt (fnInteger 3))))
    ]

-- ---------------------------------------------------------------------------
-- 8. toRational'
-- ---------------------------------------------------------------------------

toRationalTests :: TestTree
toRationalTests =
  testGroup
    "toRational'"
    [ testCase "FInteger 5 -> Just (5%1)" $
        toRational' (fnInteger 5) @?= Just (5 % 1),
      testCase "FInteger 0 -> Just (0%1)" $
        toRational' (fnInteger 0) @?= Just (0 % 1),
      testCase "FInteger (-3) -> Just ((-3)%1)" $
        toRational' (fnInteger (-3)) @?= Just ((-3) % 1),
      testCase "FRational (1%3) -> Just (1%3) approx" $ do
        let r = toRational' (fnRational (1 % 3))
        assertBool "is Just" (isJust r)
        assertNear "value" (1 / 3) (fromRational (fromJust r)),
      testCase "FRational (1%2) -> Just (1%2)" $ do
        let r = toRational' (fnRational (1 % 2))
        r @?= Just (1 % 2),
      testCase "FIrrational -> Nothing" $
        isNothing (toRational' (sqrt (fnInteger 2))) @?= True,
      testCase "FCyclomatic -> Nothing" $
        isNothing (toRational' (fnCos (fnInteger 1))) @?= True,
      testCase "FReal -> Nothing" $
        isNothing (toRational' (pi :: FieldNumber)) @?= True
    ]

-- ---------------------------------------------------------------------------
-- 9. Display
-- ---------------------------------------------------------------------------

displayTests :: TestTree
displayTests =
  testGroup
    "Display"
    [ testCase "toKaTeX FInteger 3 -> \"3\"" $
        toKaTeX (fnInteger 3) @?= "3",
      testCase "toKaTeX FInteger (-2) -> \"-2\"" $
        toKaTeX (fnInteger (-2)) @?= "-2",
      testCase "toKaTeX FIrrational starts with \\approx" $
        assertBool "approx prefix" ("\\approx" `isPrefixOf` toKaTeX (sqrt (fnInteger 2))),
      testCase "showFN includes field tag" $
        assertBool "starts with ℤ" ("ℤ" `isPrefixOf` showFN (fnInteger 3)),
      testCase "showFN FIrrational includes I" $
        assertBool "starts with I" ("I" `isPrefixOf` showFN (sqrt (fnInteger 2)))
    ]

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_ : xs) = s : tails xs

-- ---------------------------------------------------------------------------
-- 10. QuickCheck properties
-- ---------------------------------------------------------------------------

genField :: Gen Field
genField = elements [FInteger, FRational, FIrrational, FCyclomatic, FReal]

genFieldNumber :: Gen FieldNumber
genFieldNumber =
  frequency
    [ (3, fnInteger . fromIntegral <$> (choose (-50, 50) :: Gen Int)),
      ( 2,
        fnRational <$> do
          n <- choose (-10, 10)
          d <- choose (1, 10) `suchThat` (> 0)
          return (n % d)
      ),
      ( 2,
        sqrt . fnInteger . fromIntegral
          <$> (choose (1, 20) :: Gen Int)
      ),
      (1, fnCos . fnInteger . fromIntegral <$> (choose (0, 10) :: Gen Int)),
      (1, pure (pi :: FieldNumber))
    ]

genNonNeg :: Gen FieldNumber
genNonNeg = abs <$> genFieldNumber

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ testProperty "field promotion: fieldOf (a+b) >= fieldOf a" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          fieldOf (a + b) >= fieldOf a,
      testProperty "field promotion: fieldOf (a+b) >= fieldOf b" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          fieldOf (a + b) >= fieldOf b,
      testProperty "field promotion symmetric: fieldOf (a+b) == fieldOf (b+a)" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          fieldOf (a + b) == fieldOf (b + a),
      testProperty "field promotion: fieldOf (a*b) == fieldOf (b*a)" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          fieldOf (a * b) == fieldOf (b * a),
      testProperty "toDouble preserves addition" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          abs (getValue (a + b) - (getValue a + getValue b)) < 1e-9,
      testProperty "toDouble preserves multiplication" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          abs (getValue (a * b) - (getValue a * getValue b)) < 1e-9,
      testProperty "sqrt field never decreases" $
        forAll genNonNeg $ \a ->
          fieldOf (sqrt a) >= fieldOf a,
      testProperty "sqrt roundtrip: sqrt(x)^2 ≈ x for x >= 0" $
        forAll genNonNeg $ \a ->
          let s = sqrt a
           in abs (getValue (s * s) - getValue a) < 1e-9,
      testProperty "negate is involution" $
        forAll genFieldNumber $ \a ->
          negate (negate a) == a,
      testProperty "negate preserves field" $
        forAll genFieldNumber $ \a ->
          fieldOf (negate a) == fieldOf a,
      testProperty "abs is non-negative" $
        forAll genFieldNumber $ \a ->
          getValue (abs a) >= -1e-9,
      testProperty "isZero agrees with value" $
        forAll genFieldNumber $ \a ->
          isZero a == (abs (getValue a) < 1e-15),
      testProperty "isInteger implies isRational" $
        forAll genFieldNumber $ \a ->
          not (isInteger a) || isRational a,
      testProperty "isNatural implies isInteger" $
        forAll genFieldNumber $ \a ->
          not (isNatural a) || isInteger a,
      testProperty "Ord: compare consistent with EQ" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          (compare a b == EQ) == (a == b),
      testProperty "field of a+a equals field of a" $
        forAll genFieldNumber $ \a ->
          fieldOf (a + a) == fieldOf a,
      testProperty "fnCos result always FCyclomatic" $
        forAll genFieldNumber $ \a ->
          fieldOf (fnCos a) == FCyclomatic,
      testProperty "fnSin result always FCyclomatic" $
        forAll genFieldNumber $ \a ->
          fieldOf (fnSin a) == FCyclomatic,
      testProperty "exp result always FReal" $
        forAll genFieldNumber $ \a ->
          fieldOf (exp a) == FReal,
      testProperty "field of (a+b) >= max (fieldOf a) (fieldOf b)" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          fieldOf (a + b) >= max (fieldOf a) (fieldOf b),
      testProperty "signum result always FInteger" $
        forAll genFieldNumber $ \a ->
          fieldOf (signum a) == FInteger,
      testProperty "division of rationals produces rational or integer" $
        forAll ((,) <$> genFieldNumber <*> genFieldNumber) $ \(a, b) ->
          isZero b
            || fieldOf a > FRational
            || fieldOf b > FRational
            || fieldOf (a / b) <= FRational
    ]
