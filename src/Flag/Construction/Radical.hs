-- | Exact representation of numbers built from rationals and nth roots.
--
-- The 'Radical' type can represent any number expressible as a finite
-- combination of rationals, field operations (+, −, ×, ÷), and nth roots.
-- Square roots arise naturally from compass-and-straightedge constructions;
-- higher roots (cube roots, etc.) are supported structurally for future
-- construction methods (neusis, origami).
--
-- Arithmetic is fully implemented for square-root extensions (n=2).
-- Operations on higher-root extensions (n>2) will raise errors until the
-- corresponding construction primitives are added.
module Flag.Construction.Radical
    ( Radical(..)
    , MinPoly(..)
    , fieldLabels
    , cosMinPoly
    , chebyshevT
    , chebyshevU
    , normalize
    , toDouble
    , toKaTeX
    , sqrtC
    , nthRootC
    , isZero
    , isRational
    , toRational'
    , radical
    , radicands
    , isInteger
    , isNatural
    , nthPowerFree
    ) where

import Data.List (nub)
import Data.Ratio (numerator, denominator, (%))
import Debug.Trace (trace)

-- ---------------------------------------------------------------------------
-- Core data type
-- ---------------------------------------------------------------------------

-- | Exact representation of a number built from rationals and nth roots.
--
-- A value is either a rational, or lives in a radical extension:
--   a + b · r^(1/n)
-- where a, b, r are themselves Radical values and n ≥ 2 is the root index.
data Radical
  = Rational !Rational                -- ^ An exact rational value
  | Ext !Radical !Radical !Radical !Int
    -- ^ @Ext a b r n@ = a + b · r^(1/n).u
    -- Invariant (after normalisation):
    --   • n ≥ 2
    --   • b ≠ 0
    --   • r is in canonical form (nth-power-free for rational r)
  | MinPolyExt !MinPoly ![Rational]
    -- ^ Element of Q[x]/(f) represented as coefficient vector
    --   c_0 + c_1·x + ... + c_{d-1}·x^{d-1}
  deriving (Show, Read)

-- | Minimal polynomial descriptor for an algebraic field extension.
data MinPoly = MinPoly
  { mpCoeffs     :: ![Rational]  -- monic polynomial: [c_0 .. c_{d-1}] for x^d + ...
  , mpDegree     :: !Int         -- d
  , mpRootApprox :: !Double      -- which root we mean (for toDouble/Ord)
  , mpLabel      :: !String      -- KaTeX label for field symbol
  }
  deriving (Show, Read, Eq)

-- | Smart constructor: build an extension and normalise.
radical :: Radical -> Radical -> Radical -> Int -> Radical
radical a b r n = normalize (Ext a b r n)

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

isZero :: Radical -> Bool
isZero (Rational r) = r == 0
isZero (Ext a b _ _) = isZero a && isZero b
isZero (MinPolyExt _ cs) = all (== 0) cs

isRational :: Radical -> Bool
isRational (Rational _) = True
isRational _ = False

-- | Extract the rational value, if this is a pure rational.
toRational' :: Radical -> Maybe Rational
toRational' (Rational r) = Just r
toRational' _ = Nothing

-- | Is this a (non-negative) integer?
isNatural :: Radical -> Bool
isNatural (Rational r) = denominator r == 1 && numerator r >= 0
isNatural _ = False

-- | Is this an integer?
isInteger :: Radical -> Bool
isInteger (Rational r) = denominator r == 1
isInteger _ = False

-- | Collect all unique rational radicands with their root indices.
-- For example, @1 + 2√3@ yields @[(3, 2)]@.
radicands :: Radical -> [(Rational, Int)]
radicands = nub . go
  where
    go (Rational _) = []
    go (Ext a b r n) = go a ++ go b ++ case r of
      Rational q -> [(q, n)]
      _          -> go r
    go (MinPolyExt _ _) = []

-- ---------------------------------------------------------------------------
-- Normalisation
-- ---------------------------------------------------------------------------

-- | Normalise a Radical value.
--
-- Pipeline: normaliseNested → factorRadical → mergeRadicand
--           → denestSqrt → zeroElimination
normalize :: Radical -> Radical
normalize rad =
    zeroElimination . denestSqrt . mergeRadicand . factorRadical
                    . normalizeNested $ rad
  where
    -- Recursively normalise sub-expressions.
    normalizeNested (Ext a b c r) = Ext (normalize a) (normalize b) (normalize c) r
    normalizeNested x@(Rational _) = x
    normalizeNested (MinPolyExt mp cs) = MinPolyExt mp (polyReduce mp cs)

    -- Collapse Ext when the coefficient or radicand is zero.
    zeroElimination (Ext a b _ _) | isZero b = a
    zeroElimination (Ext a _ r _) | isZero r = a
    zeroElimination x = x

    -- For rational radicands, factor out perfect nth powers and
    -- rationalise the radicand denominator.
    -- E.g. √12 = 2√3, √(9/4) = 3/2, ∛(-8) = -2, √(1/5) = √5/5.
    factorRadical (Ext a b (Rational r) n)
      | r < 0 && even n = error $ "factorRadical: negative radicand "
                                ++ show r ++ " under even root index " ++ show n
      | r < 0 && odd n  =
          -- ∛(-x) = -∛x: move sign into coefficient
          factorRadical (Ext a (negateR b) (Rational (negate r)) n)
      | otherwise =
          let pn = numerator r
              pd = denominator r
              (dN, kN) = nthPowerFree n (abs pn)
              (dD, kD) = nthPowerFree n pd
              -- r = (dN^n · kN) / (dD^n · kD)
              -- r^(1/n) = (dN/dD) · (kN/kD)^(1/n)
              --
              -- Rationalize the radicand denominator:
              -- (kN/kD)^(1/n) = (kN · kD^(n-1))^(1/n) / kD
              -- Then factor the integer radicand again.
              rawRad  = kN * kD ^ (n - 1)
              (dR, finalRad) = nthPowerFree n rawRad
              factor  = Rational (fromInteger (dN * dR) % fromInteger (dD * kD))
              newR    = Rational (fromInteger finalRad)
              newB    = mulR b factor
          in  if finalRad == 1
                -- radicand was a perfect nth power: no radical remains
                then addR a newB
                else Ext a newB newR n
    factorRadical x = x

    -- Merge nested Ext levels that share the same radicand.
    -- Two rules for flattening elements of Q(√r):
    --   (1) Ext (Ext a1 b1 r n) b r n → Ext a1 (b1+b) r n
    --       because: (a1 + b1·√r) + b·√r = a1 + (b1+b)·√r
    --   (2) Ext a (Ext b1 b2 r n) r n → Ext (a + b2·r) b1 r n
    --       because: a + (b1 + b2·√r)·√r = (a + b2·r) + b1·√r
    mergeRadicand (Ext (Ext a1 b1 r1 n1) b r n)
      | n1 == n && r1 `radEq` r =
          normalize (Ext a1 (addR b1 b) r n)
    mergeRadicand (Ext a (Ext b1 b2 r1 n1) r n)
      | n1 == n && r1 `radEq` r =
          normalize (Ext (addR a (mulR b2 r)) b1 r n)
    mergeRadicand x = x

    -- Denest square roots: √(a + b√r) = √((a+d)/2) + sign(b)·√((a-d)/2)
    -- where d = √(a²-b²r), but ONLY when disc = a²-b²r is a perfect
    -- square (rational d). Otherwise denesting recurses infinitely.
    denestSqrt (Ext a0 b0 (Ext a b r 2) 2)
      | isZero a0 =
          let disc = subR (mulR a a) (mulR (mulR b b) r)
          in case disc of
               Rational d2
                 | d2 >= 0, Just d <- perfectSqrtR d2 ->
                     let half   = Rational (1 % 2)
                         dR     = Rational d
                         t1     = sqrtC (mulR (addR a dR) half)
                         t2     = sqrtC (mulR (subR a dR) half)
                         s      = signR b
                         denest = if s >= 0 then addR t1 t2 else subR t1 t2
                     in  mulR b0 denest
               _ -> Ext a0 b0 (Ext a b r 2) 2
    denestSqrt x = x

-- | Check if a rational is a perfect square and return its sqrt.
perfectSqrtR :: Rational -> Maybe Rational
perfectSqrtR r
  | r < 0     = Nothing
  | r == 0    = Just 0
  | otherwise =
      let p = numerator r
          q = denominator r
      in case (isqrt p, isqrt q) of
           (Just sp, Just sq) -> Just (sp % sq)
           _                  -> Nothing

-- ---------------------------------------------------------------------------
-- Integer nth-power factoring
-- ---------------------------------------------------------------------------

-- | Factor |n| = d^k · m where m is k-th-power-free.
-- Returns (d, m). Assumes n > 0.
nthPowerFree :: Int -> Integer -> (Integer, Integer)
nthPowerFree _ 0 = (0, 0)
nthPowerFree _ 1 = (1, 1)
nthPowerFree k n
  | n < 0     = let (d, m) = nthPowerFree k (negate n) in (d, m)  -- sign handled elsewhere
  | otherwise = go 2 1 n
  where
    go p d m
      | p * p > m = (d, m)  -- for k=2; conservative for higher k
      | otherwise =
          let (cnt, m') = extractFactor p m
              fullPowers = cnt `div` k
          in  if fullPowers > 0
                then go (p + 1) (d * p ^ fullPowers) (m' * p ^ (cnt `mod` k))
                else go (p + 1) d m
    extractFactor p m
      | m `mod` p == 0 = let (cnt, m') = extractFactor p (m `div` p) in (cnt + 1, m')
      | otherwise      = (0, m)

-- | Integer square root, if n is a perfect square.
isqrt :: Integer -> Maybe Integer
isqrt n
  | n < 0     = Nothing
  | n == 0    = Just 0
  | otherwise = let s = floor (sqrt (fromIntegral n :: Double))
                in  -- Check s and s+1 to handle floating-point imprecision
                    if s * s == n then Just s
                    else if (s+1)*(s+1) == n then Just (s+1)
                    else Nothing

-- | Integer nth root, if n is a perfect kth power.
inroot :: Int -> Integer -> Maybe Integer
inroot 2 n = isqrt n
inroot k n
  | n < 0 && even k = Nothing
  | n < 0           = negate <$> inroot k (negate n)
  | n == 0          = Just 0
  | n == 1          = Just 1
  | otherwise       =
      let s = round ((fromIntegral n :: Double) ** (1 / fromIntegral k))
      in  -- Check s-1, s, s+1 for robustness
          if s ^ k == n then Just s
          else if (s+1) ^ k == n then Just (s + 1)
          else if s > 1 && (s-1) ^ k == n then Just (s - 1)
          else Nothing

-- ---------------------------------------------------------------------------
-- Arithmetic
-- ---------------------------------------------------------------------------

addR :: Radical -> Radical -> Radical
addR (Rational a) (Rational b) = Rational (a + b)
addR (Rational a) (Ext a2 b2 r2 n2) = normalize (Ext (addR (Rational a) a2) b2 r2 n2)
addR (Ext a1 b1 r1 n1) (Rational b) = normalize (Ext (addR a1 (Rational b)) b1 r1 n1)
addR (Ext a1 b1 r1 n1) (Ext a2 b2 r2 n2)
  | n1 == n2 && r1 `radEq` r2 = normalize (Ext (addR a1 a2) (addR b1 b2) r1 n1)
  -- Different radicands or indices: nest
  -- (a1 + b1·r1^(1/n1)) + (a2 + b2·r2^(1/n2))
  --   = Ext (Ext (a1+a2) b2 r2 n2) b1 r1 n1
  | otherwise = normalize (Ext (Ext (addR a1 a2) b2 r2 n2) b1 r1 n1)
addR (MinPolyExt mp1 cs1) (MinPolyExt mp2 cs2)
  | mp1 == mp2 = MinPolyExt mp1 (mpAdd cs1 cs2)
  | otherwise = error "addR: incompatible MinPolyExt fields"
addR (MinPolyExt mp cs) (Rational r) = MinPolyExt mp (let (h:ts) = cs in (h + r) : ts)
addR (Rational r) m@(MinPolyExt _ _) = addR m (Rational r)
addR (MinPolyExt mp cs) ext@(Ext a b r n) = normalize (Ext (addR (MinPolyExt mp cs) a) b r n)
addR ext@(Ext a b r n) (MinPolyExt mp cs) = normalize (Ext (addR a (MinPolyExt mp cs)) b r n)

subR :: Radical -> Radical -> Radical
subR a b = addR a (negateR b)

negateR :: Radical -> Radical
negateR (Rational r) = Rational (negate r)
negateR (Ext a b r n) = Ext (negateR a) (negateR b) r n
negateR (MinPolyExt mp cs) = MinPolyExt mp (mpNeg cs)

mulR :: Radical -> Radical -> Radical
mulR (Rational a) (Rational b) = Rational (a * b)
mulR (Rational a) (Ext a2 b2 r2 n2) = normalize (Ext (mulR (Rational a) a2) (mulR (Rational a) b2) r2 n2)
mulR (Ext a1 b1 r1 n1) (Rational b) = normalize (Ext (mulR a1 (Rational b)) (mulR b1 (Rational b)) r1 n1)
mulR (Ext a1 b1 r1 n1) (Ext a2 b2 r2 n2)
  | n1 == 2 && n2 == 2 && r1 `radEq` r2 =
      -- (a1 + b1√r)(a2 + b2√r) = (a1·a2 + b1·b2·r) + (a1·b2 + a2·b1)√r
      let newA = addR (mulR a1 a2) (mulR (mulR b1 b2) r1)
          newB = addR (mulR a1 b2) (mulR a2 b1)
      in  normalize (Ext newA newB r1 2)
  | n1 == 2 && n2 == 2 =
      -- Different radicands: (a1 + b1√r1)(a2 + b2√r2)
      -- = a1·a2 + a1·b2·√r2 + b1·a2·√r1 + b1·b2·√(r1·r2)
      -- Nest: the result lives in Q(√r1, √r2)
      -- Represent as: Ext (a1·(a2+b2√r2)) (b1·(a2+b2√r2)) r1 2 ... but need √(r1·r2) term
      -- For simplicity, distribute:
      let term1 = mulR a1 a2                               -- rational part
          term2 = Ext (Rational 0) (mulR a1 b2) r2 2       -- a1·b2·√r2
          term3 = Ext (Rational 0) (mulR b1 a2) r1 2       -- b1·a2·√r1
          term4r = mulR r1 r2                               -- r1·r2
          term4 = Ext (Rational 0) (mulR b1 b2) term4r 2   -- b1·b2·√(r1·r2)
      in  normalize $ addR (addR term1 term2) (addR term3 term4)
  | otherwise = error $ "mulR: arithmetic for root index "
                     ++ show n1 ++ " and " ++ show n2 ++ " not yet implemented"
mulR (MinPolyExt mp1 cs1) (MinPolyExt mp2 cs2)
  | mp1 == mp2 = MinPolyExt mp1 (mpMul mp1 cs1 cs2)
  | otherwise = error "mulR: incompatible MinPolyExt fields"
mulR (MinPolyExt mp cs) (Rational r) = MinPolyExt mp (mpScale r cs)
mulR (Rational r) m@(MinPolyExt _ _) = mulR m (Rational r)
mulR (MinPolyExt mp cs) ext@(Ext a b r n) = normalize (Ext (mulR (MinPolyExt mp cs) a) (mulR (MinPolyExt mp cs) b) r n)
mulR ext@(Ext a b r n) (MinPolyExt mp cs) = normalize (Ext (mulR a (MinPolyExt mp cs)) (mulR b (MinPolyExt mp cs)) r n)

-- | Structural equality after normalisation (used for radicand comparison).
radEq :: Radical -> Radical -> Bool
radEq (Rational a) (Rational b) = a == b
radEq (Ext a1 b1 r1 n1) (Ext a2 b2 r2 n2) =
  n1 == n2 && radEq a1 a2 && radEq b1 b2 && radEq r1 r2
radEq (MinPolyExt mp1 cs1) (MinPolyExt mp2 cs2) = mp1 == mp2 && cs1 == cs2
radEq _ _ = False

divR :: Radical -> Radical -> Radical
divR _ b | isZero b = error "divR: division by zero"
divR a (Rational b) = mulR a (Rational (1 / b))
divR a (Ext a2 b2 r2 2)
    -- Special case: a / (b2·√r2) = a·√r2 / (b2·r2)
    -- Avoids conjugate multiplication when a2=0, reducing nesting.
    | isZero a2 =
        let sqrtR = Ext (Rational 0) (Rational 1) r2 2
            num   = mulR a sqrtR
            denom = mulR b2 r2
        in  divR num denom
    -- General: multiply by conjugate (a2 - b2·√r2)
    -- Denominator: a2² - b2²·r2
    | otherwise =
        let conj  = Ext a2 (negateR b2) r2 2
            denom = subR (mulR a2 a2) (mulR (mulR b2 b2) r2)
            num   = mulR a conj
        in  divR num denom   -- denom should now be simpler (no √r2)
divR (MinPolyExt mp1 cs1) (MinPolyExt mp2 cs2)
  | mp1 == mp2 = MinPolyExt mp1 (mpDiv mp1 cs1 cs2)
  | otherwise = error "divR: incompatible MinPolyExt fields"
divR (MinPolyExt mp cs) (Rational r) = MinPolyExt mp (mpScale (1 / r) cs)
divR (Rational r) (MinPolyExt mp cs) =
  let d = mpDegree mp
      vec = (r : replicate (d - 1) 0)
  in  MinPolyExt mp (mpMul mp vec (mpInv mp cs))
divR _ b = error $ "divR: division by non-rational, non-quadratic radical: " ++ show b

absR :: Radical -> Radical
absR x = if signR x < 0 then negateR x else x

-- | Determine the sign of a Radical: -1, 0, or 1.
signR :: Radical -> Int
signR (Rational r) = case compare r 0 of
  LT -> -1
  EQ -> 0
  GT -> 1
signR (Ext a b r n) =
    -- Approximate via Double; for exact sign of a + b·r^(1/n),
    -- we'd need algebraic number theory. The Double approximation is
    -- sufficient for all practical flag constructions.
    let d = toDouble (Ext a b r n)
    in  if abs d < 1e-15 then 0
        else if d < 0 then -1 else 1
signR (MinPolyExt mp cs) =
  let d = toDouble (MinPolyExt mp cs)
  in  if abs d < 1e-15 then 0 else if d < 0 then -1 else 1

signumR :: Radical -> Radical
signumR x = case signR x of
  -1 -> Rational (-1)
  0  -> Rational 0
  1  -> Rational 1
  _  -> error "signumR: impossible"

-- ---------------------------------------------------------------------------
-- Square root / nth root
-- ---------------------------------------------------------------------------

-- | Square root of a Radical value.
sqrtC :: Radical -> Radical
sqrtC = nthRootC 2

-- | Nth root of a Radical value.
--
-- For rational arguments, checks for perfect nth powers and factors out
-- nth-power components.  For Ext arguments, builds the extension and lets
-- 'normalize' handle denesting.
nthRootC :: Int -> Radical -> Radical
nthRootC n (Rational r)
  | r == 0    = Rational 0
  | r < 0 && even n = error $ "nthRootC: negative radicand " ++ show r
                            ++ " under even root index " ++ show n
  | r < 0 && odd n  = negateR (nthRootC n (Rational (negate r)))
  | otherwise =
      let p = numerator r
          q = denominator r
      in case (inroot n p, inroot n q) of
           (Just sp, Just sq) -> Rational (sp % sq)
           _ -> normalize (Ext (Rational 0) (Rational 1) (Rational r) n)
nthRootC n x = normalize (Ext (Rational 0) (Rational 1) x n)

-- ---------------------------------------------------------------------------
-- Type class instances
-- ---------------------------------------------------------------------------

instance Eq Radical where
  a == b = radEq (normalize a) (normalize b)

instance Ord Radical where
  compare a b = let d = toDouble (subR a b)
                in  if abs d < 1e-15 then EQ
                    else if d < 0 then LT else GT

instance Num Radical where
  (+)         = addR
  (-)         = subR
  (*)         = mulR
  negate      = negateR
  abs         = absR
  signum      = signumR
  fromInteger = Rational . fromInteger

instance Fractional Radical where
  (/)            = divR
  fromRational   = Rational

instance Floating Radical where
  sqrt  = sqrtC
  pi    = error "pi is not algebraic"
  exp   = error "exp is not algebraic in general"
  log   = error "log is not algebraic in general"
  sin   = error "sin is not algebraic in general"
  cos   = error "cos is not algebraic in general"
  asin  = error "asin is not algebraic in general"
  acos  = error "acos is not algebraic in general"
  atan  = error "atan is not algebraic in general"
  sinh  = error "sinh is not algebraic in general"
  cosh  = error "cosh is not algebraic in general"
  asinh = error "asinh is not algebraic in general"
  acosh = error "acosh is not algebraic in general"
  atanh = error "atanh is not algebraic in general"

-- ---------------------------------------------------------------------------
-- Conversion to Double
-- ---------------------------------------------------------------------------

-- | Convert a Radical to a Double approximation.
toDouble :: Radical -> Double
toDouble (Rational r)    = fromRational r
toDouble (MinPolyExt mp cs) =
  let x = mpRootApprox mp
  in sum [ fromRational c * (x ** fromIntegral i) | (c, i) <- zip cs [0 :: Int ..] ]
toDouble (Ext a b r n)   = toDouble a + toDouble b * (toDouble r ** (1 / fromIntegral n))

-- ---------------------------------------------------------------------------
-- Conversion to KaTeX
-- ---------------------------------------------------------------------------

-- | Render a Radical as a KaTeX math string.
toKaTeX :: Radical -> String
toKaTeX (Rational r)
  | denominator r == 1 =
      let n = numerator r
      in  if n < 0 then "-" ++ show (abs n) else show n
  | otherwise =
      let n = numerator r
          d = denominator r
          sign = if n < 0 then "-" else ""
      in  sign ++ "\\frac{" ++ show (abs n) ++ "}{" ++ show d ++ "}"
toKaTeX (MinPolyExt mp cs) =
  let lbl = mpLabel mp
      terms = zip cs [0 :: Int ..]
      renderTerm (c, i)
        | c == 0 = Nothing
        | i == 0 = Just (toKaTeX (Rational c))
        | i == 1 = if c == 1 then Just lbl else if c == -1 then Just ("-" ++ lbl) else if c < 0 then Just ("-" ++ toKaTeX (Rational (abs c)) ++ lbl) else Just (toKaTeX (Rational c) ++ lbl)
        | otherwise = if c == 1 then Just (lbl ++ "^{" ++ show i ++ "}") else if c == -1 then Just ("-" ++ lbl ++ "^{" ++ show i ++ "}") else if c < 0 then Just ("-" ++ toKaTeX (Rational (abs c)) ++ lbl ++ "^{" ++ show i ++ "}") else Just (toKaTeX (Rational c) ++ lbl ++ "^{" ++ show i ++ "}")
      parts = [ s | Just s <- map renderTerm terms ]
  in if null parts then "0" else foldl1 (\a b -> a ++ " + " ++ b) parts
toKaTeX (Ext a b r n)
  = let bStr = toKaTeXCoeff b
        radStr = renderRadical r n
        aStr = toKaTeX a
        aIsZero = isZero a
        bSign = signR b
    in  if aIsZero
          then case bSign of
            0  -> "0"
            -1 -> "-" ++ toKaTeXCoeffAbs b ++ radStr
            _  -> toKaTeXCoeffAbs b ++ radStr
          else case bSign of
            0  -> aStr
            -1 -> aStr ++ " - " ++ toKaTeXCoeffAbs b ++ radStr
            _  -> aStr ++ " + " ++ bStr ++ radStr

-- | Render the radical part: √{r} or ∛{r} etc.
renderRadical :: Radical -> Int -> String
renderRadical r 2 = "\\sqrt{" ++ toKaTeX r ++ "}"
renderRadical r n = "\\sqrt[" ++ show n ++ "]{" ++ toKaTeX r ++ "}"

-- | Render coefficient b for multiplication with a radical.
-- Returns "" for b=1, the KaTeX string otherwise.
toKaTeXCoeff :: Radical -> String
toKaTeXCoeff (Rational r)
  | r == 1    = ""
  | r == -1   = "-"
  | otherwise = toKaTeX (Rational (abs r))
toKaTeXCoeff x = toKaTeX x

-- | Render |b| as a coefficient (empty string for 1).
toKaTeXCoeffAbs :: Radical -> String
toKaTeXCoeffAbs (Rational r)
  | abs r == 1 = ""
  | otherwise  = toKaTeX (Rational (abs r))
toKaTeXCoeffAbs x = toKaTeX (absR x)

-- ---------------------------------------------------------------------------
-- Minimal polynomial arithmetic helpers (coeff vectors)
-- ---------------------------------------------------------------------------

-- | Reduce a coefficient vector modulo the monic minimal polynomial.
-- The polynomial is x^d + c_{d-1} x^{d-1} + ... + c_0 where
-- mpCoeffs = [c_0 .. c_{d-1}]. Returns a vector of length d.
polyReduce :: MinPoly -> [Rational] -> [Rational]
polyReduce mp cs =
  let d = mpDegree mp
      coeffs = cs ++ replicate (max 0 (d - length cs)) 0
      -- reduce higher-degree terms into lower ones
      reduce vec i
        | i < d = take d vec
        | otherwise =
            let ai = vec !! i
                vec' = if ai == 0 then vec
                       else
                         -- For x^i with i >= d, use relation x^d = -sum_{k=0..d-1} c_k x^k
                         foldl (\buf j ->
                           let idx = i - d + j
                               c = mpCoeffs mp !! j
                               old = buf !! idx
                               new = old + ai * (negate c)
                           in take idx buf ++ [new] ++ drop (idx + 1) buf
                         ) vec [0 .. d - 1]
            in  reduce vec' (i - 1)
  in  reduce coeffs (length coeffs - 1)

-- | Add two coefficient vectors (pad/truncate to degree).
mpAdd :: [Rational] -> [Rational] -> [Rational]
mpAdd a b = let n = max (length a) (length b)
            in  [ (if i < length a then a !! i else 0) + (if i < length b then b !! i else 0) | i <- [0..n-1] ]

mpNeg :: [Rational] -> [Rational]
mpNeg = map negate

mpScale :: Rational -> [Rational] -> [Rational]
mpScale s = map (s *)

mpSub :: [Rational] -> [Rational] -> [Rational]
mpSub a b = mpAdd a (mpNeg b)

-- | Multiply two polynomials and reduce modulo the minimal polynomial.
mpMul :: MinPoly -> [Rational] -> [Rational] -> [Rational]
mpMul mp a b =
  let la = length a
      lb = length b
      prodLen = max 1 (la + lb - 1)
      prod = replicate prodLen 0
      prod' = foldl (\buf (i, ai) ->
                foldl (\buf2 (j, bj) ->
                  let idx = i + j
                      old = buf2 !! idx
                      new = old + ai * bj
                  in take idx buf2 ++ [new] ++ drop (idx + 1) buf2
                ) buf (zip [0..] b)
              ) prod (zip [0..] a)
  in polyReduce mp prod'

-- | Division and inversion are non-trivial; provide stubs for now.
mpDiv :: MinPoly -> [Rational] -> [Rational] -> [Rational]
mpDiv mp a b =
  -- a / b in the field Q[x]/(f) == a * b^{-1} mod f
  let invB = mpInv mp b
  in mpMul mp a invB

-- Raw polynomial helpers (no modulo reduction)
polyTrim :: [Rational] -> [Rational]
polyTrim = reverse . dropWhile (== 0) . reverse

polyDegree :: [Rational] -> Int
polyDegree p = length (polyTrim p) - 1

polyAddRaw :: [Rational] -> [Rational] -> [Rational]
polyAddRaw a b = let n = max (length a) (length b)
                 in [ (if i < length a then a !! i else 0) + (if i < length b then b !! i else 0) | i <- [0..n-1] ]

polySubRaw :: [Rational] -> [Rational] -> [Rational]
polySubRaw a b = polyAddRaw a (map negate b)

polyScaleRaw :: Rational -> [Rational] -> [Rational]
polyScaleRaw s = map (s *)

polyMulRaw :: [Rational] -> [Rational] -> [Rational]
polyMulRaw a b =
  let la = length a
      lb = length b
      prod = replicate (max 1 (la + lb - 1)) 0
  in foldl (\buf (i, ai) -> foldl (\buf2 (j, bj) ->
                    let idx = i + j
                        old = buf2 !! idx
                        new = old + ai * bj
                    in take idx buf2 ++ [new] ++ drop (idx + 1) buf2
                  ) buf (zip [0..] b)
           ) prod (zip [0..] a)

-- Polynomial division (quotient and remainder), raw over Q.
polyDivMod :: [Rational] -> [Rational] -> ([Rational], [Rational])
polyDivMod a b
  | null tb = error "polyDivMod: division by zero"
  | degreeA < degreeB = ([0], polyTrim a)
  | otherwise = go (polyTrim a) []
  where
    tb = polyTrim b
    degreeB = length tb - 1
    degreeA = length (polyTrim a) - 1
    leadB = last tb
    go r q
      | length r - 1 < degreeB = (if null q then [0] else reverse q, polyTrim r)
      | otherwise =
          let degR = length r - 1
              coeff = (last r) / leadB
              shift = degR - degreeB
              term = replicate shift 0 ++ map (coeff *) tb
              r' = polyTrim (polySubRaw r term)
          in go r' (coeff : q)

-- Extended GCD for polynomials over Q. Returns (g, s, t) with s*a + t*b = g.
polyExtendedGCD :: [Rational] -> [Rational] -> ([Rational], [Rational], [Rational])
polyExtendedGCD a b = go a b [1] [0] [0] [1]
  where
    go r0 r1 s0 s1 t0 t1
      | all (==0) r1 = (polyTrim r0, s0, t0)
      | otherwise =
          let (q, r2) = polyDivMod r0 r1
              s2 = polySubRaw s0 (polyMulRaw q s1)
              t2 = polySubRaw t0 (polyMulRaw q t1)
          in go r1 r2 s1 s2 t1 t2

-- Solve linear system M * t = e0 over Q, where M_j = polyReduce(mp, a * x^j).
mpInv :: MinPoly -> [Rational] -> [Rational]
mpInv mp a
  | all (==0) (polyTrim a) = error "mpInv: inverse of zero"
  | otherwise =
      let d = mpDegree mp
          aTrim = polyTrim a
          -- build matrix columns: for basis vector with 1 at pos j, compute col = polyReduce(mp, a * x^j)
          unit j = replicate j 0 ++ [1]
          cols = [ polyReduce mp (polyMulRaw aTrim (unit j)) | j <- [0 .. d - 1] ]
          -- matrix as list of rows: row i contains cols[j][i]
          rows = [ [ if i < length (cols !! j) then (cols !! j) !! i else 0 | j <- [0 .. d - 1] ] | i <- [0 .. d - 1] ]
          rhs = 1 : replicate (d - 1) 0
          solution = solveLinearSystem rows rhs
      in polyReduce mp solution

-- Gaussian elimination for rational matrices. Rows given as list of lists.
solveLinearSystem :: [[Rational]] -> [Rational] -> [Rational]
solveLinearSystem a b =
  let n = length a
      -- augment matrix
      aug = [ (row ++ [bval]) | (row, bval) <- zip a b ]
      forward mat col
        | col >= n = mat
        | otherwise =
            -- find pivot
            case findIndexNonZero mat col col of
              Nothing -> error "solveLinearSystem: singular matrix"
              Just piv ->
                let mat1 = swapRows mat col piv
                    pivotRow = mat1 !! col
                    pivotVal = pivotRow !! col
                    pivotRowNorm = map (/ pivotVal) pivotRow
                    mat2 = [ if i == col then pivotRowNorm else
                               let factor = (mat1 !! i) !! col
                               in zipWith (-) (mat1 !! i) (map (factor *) pivotRowNorm)
                           | i <- [0 .. n - 1] ]
                in forward mat2 (col + 1)
      backsub mat =
        let xs = replicate n 0
        in foldr (\i acc ->
            let row = mat !! i
                rhsVal = last row - sum [ (row !! j) * (acc !! j) | j <- [i+1 .. n-1] ]
            in take i acc ++ [rhsVal / (row !! i)] ++ drop (i+1) acc
          ) xs [0..n-1]
      matF = forward aug 0
  in backsub matF

findIndexNonZero :: [[Rational]] -> Int -> Int -> Maybe Int
findIndexNonZero mat start col =
  let idxs = [start .. length mat - 1]
  in foldr (\i acc -> if acc == Nothing && (mat !! i) !! col /= 0 then Just i else acc) Nothing idxs

swapRows :: [[Rational]] -> Int -> Int -> [[Rational]]
swapRows mat i j
  | i == j = mat
  | otherwise = [ rowAt k | k <- [0 .. length mat - 1] ]
  where
    rowAt k
      | k == i = mat !! j
      | k == j = mat !! i
      | otherwise = mat !! k

-- | Collect labels of any MinPolyExt fields reachable from a Radical.
fieldLabels :: Radical -> [String]
fieldLabels (Rational _) = []
fieldLabels (Ext a b r _) = fieldLabels a ++ fieldLabels b ++ fieldLabels r
fieldLabels (MinPolyExt mp _) = [mpLabel mp]

-- | Minimal polynomial for cos(2π/n) where supported.
cosMinPoly :: Int -> MinPoly
cosMinPoly 7 = MinPoly
  { mpCoeffs = [(-1) % 8, (-1) % 2, 1 % 2]
  , mpDegree = 3
  , mpRootApprox = 0.6234898018559786
  , mpLabel = "\\zeta_7"
  }
cosMinPoly 4 = MinPoly
  { mpCoeffs = [0 % 1]
  , mpDegree = 1
  , mpRootApprox = 0.0
  , mpLabel = "\\zeta_4"
  }
cosMinPoly n = error $ "cosMinPoly: n=" ++ show n ++ " not yet supported"

-- | Chebyshev polynomials of the first kind (T_k) and second kind (U_k),
-- represented as coefficient vectors reduced modulo the minimal polynomial.
chebyshevT :: MinPoly -> Int -> [Rational]
chebyshevT mp k
  | k < 0 = error "chebyshevT: negative index"
  | k == 0 = polyReduce mp ([1] ++ replicate (mpDegree mp - 1) 0)
  | k == 1 = polyReduce mp ([0,1] ++ replicate (mpDegree mp - 2) 0)
  | otherwise =
      let two = 2 % 1
          cVec = polyReduce mp ([0,1] ++ replicate (mpDegree mp - 2) 0)
          tPrev = chebyshevT mp (k - 1)
          tPrev2 = chebyshevT mp (k - 2)
          term = mpScale two (mpMul mp cVec tPrev)
      in polyReduce mp (mpSub term tPrev2)

chebyshevU :: MinPoly -> Int -> [Rational]
chebyshevU mp k
  | k < 0 = error "chebyshevU: negative index"
  | k == 0 = polyReduce mp ([1] ++ replicate (mpDegree mp - 1) 0)
  | k == 1 = polyReduce mp ([0,2] ++ replicate (mpDegree mp - 2) 0)
  | otherwise =
      let two = 2 % 1
          cVec = polyReduce mp ([0,1] ++ replicate (mpDegree mp - 2) 0)
          uPrev = chebyshevU mp (k - 1)
          uPrev2 = chebyshevU mp (k - 2)
          term = mpScale two (mpMul mp cVec uPrev)
      in polyReduce mp (mpSub term uPrev2)
