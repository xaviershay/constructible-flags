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
  | Real !Double
    -- ^ An approximate real value (floating-point).
    --   \"Contagious\": any operation combining Real with Ext collapses to Real.
  deriving (Show, Read)

-- | Smart constructor: build an extension and normalise.
radical :: Radical -> Radical -> Radical -> Int -> Radical
radical a b r n = normalize (Ext a b r n)

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

isZero :: Radical -> Bool
isZero (Rational r) = r == 0
isZero (Ext a b _ _) = isZero a && isZero b
isZero (Real d) = abs d < 1e-15

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
    go (Real _) = []

-- ---------------------------------------------------------------------------
-- Normalisation
-- ---------------------------------------------------------------------------

-- | Normalise a Radical value.
--
-- Pipeline: normaliseNested → factorRadical → consolidateRadicands
--           → denestSqrt → zeroElimination
normalize :: Radical -> Radical
normalize rad =
    zeroElimination . denestSqrt . consolidateRadicands . factorRadical
                    . normalizeNested $ rad
  where
    -- Recursively normalise sub-expressions.
    normalizeNested (Ext a b c r) = Ext (normalize a) (normalize b) (normalize c) r
    normalizeNested x@(Rational _) = x
    normalizeNested x@(Real _) = x

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

    -- Consolidate all occurrences of a radicand into a single Ext layer.
    -- Generalises the old mergeRadicand (which only handled adjacent levels)
    -- to handle the same radicand appearing at any nesting depth.
    -- Uses extractCoeff to decompose a and b into parts with/without √r,
    -- then recombines into a single Ext layer.
    consolidateRadicands (Ext a b r n)
      | containsRadicand a r n || containsRadicand b r n =
          let (aw, ac) = extractCoeff a r n
              (bw, bc) = extractCoeff b r n
              -- expr = (aw + ac·√r) + (bw + bc·√r)·√r
              --      = (aw + bc·r) + (ac + bw)·√r
              a' = addR aw (mulR bc r)
              b' = addR ac bw
          in  normalize (Ext a' b' r n)
    consolidateRadicands x = x

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
addR (Real d1) (Real d2) = Real (d1 + d2)
addR (Real d) (Rational r) = Real (d + fromRational r)
addR (Rational r) (Real d) = Real (fromRational r + d)
addR (Real d) x = Real (d + toDouble x)
addR x (Real d) = Real (toDouble x + d)

subR :: Radical -> Radical -> Radical
subR a b = addR a (negateR b)

negateR :: Radical -> Radical
negateR (Rational r) = Rational (negate r)
negateR (Ext a b r n) = Ext (negateR a) (negateR b) r n
negateR (Real d) = Real (negate d)

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
mulR (Real d1) (Real d2) = Real (d1 * d2)
mulR (Real d) (Rational r) = Real (d * fromRational r)
mulR (Rational r) (Real d) = Real (fromRational r * d)
mulR (Real d) x = Real (d * toDouble x)
mulR x (Real d) = Real (toDouble x * d)

-- | Structural equality after normalisation (used for radicand comparison).
radEq :: Radical -> Radical -> Bool
radEq (Rational a) (Rational b) = a == b
radEq (Ext a1 b1 r1 n1) (Ext a2 b2 r2 n2) =
  n1 == n2 && radEq a1 a2 && radEq b1 b2 && radEq r1 r2
radEq (Real d1) (Real d2) = abs (d1 - d2) < 1e-15
radEq _ _ = False

divR :: Radical -> Radical -> Radical
divR _ b | isZero b = error "divR: division by zero"
divR a (Rational b) = mulR a (Rational (1 / b))
divR a (Ext a2 b2 r2 2) =
    -- Consolidate: collect all √r2 from a2 and b2 into a single layer.
    -- Without this, conjugate multiplication can reintroduce √r2 from
    -- inner nesting levels, causing non-termination.
    let (aw, ac) = extractCoeff a2 r2 2
        (bw, bc) = extractCoeff b2 r2 2
        -- denom = (aw + ac·√r2) + (bw + bc·√r2)·√r2
        --       = (aw + bc·r2) + (ac + bw)·√r2
        a2' = addR aw (mulR bc r2)
        b2' = addR ac bw
    in if isZero b2'
       then divR a a2'  -- no √r2 remains in denominator
       else if isZero a2'
            -- Special case: a / (b2'·√r2) = a·√r2 / (b2'·r2)
            then let sqrtR = Ext (Rational 0) (Rational 1) r2 2
                     num   = mulR a sqrtR
                     denom = mulR b2' r2
                 in  divR num denom
            -- General: multiply by conjugate (a2' - b2'·√r2)
            -- Denominator: a2'² - b2'²·r2  (no √r2 remains)
            else let conj  = Ext a2' (negateR b2') r2 2
                     denom = subR (mulR a2' a2') (mulR (mulR b2' b2') r2)
                     num   = mulR a conj
                 in  divR num denom
divR (Real d1) (Real d2) = Real (d1 / d2)
divR (Rational r) (Real d) = Real (fromRational r / d)
divR (Real d) x = Real (d / toDouble x)
divR x (Real d) = Real (toDouble x / d)
divR _ b = error $ "divR: division by non-rational, non-quadratic radical: " ++ show b

-- | Check whether a Radical expression contains a given radicand at any depth.
containsRadicand :: Radical -> Radical -> Int -> Bool
containsRadicand (Rational _) _ _ = False
containsRadicand (Real _) _ _ = False
containsRadicand (Ext a b r n) targetR targetN
    | n == targetN && r `radEq` targetR = True
    | otherwise = containsRadicand a targetR targetN
               || containsRadicand b targetR targetN

-- | Decompose a Radical into parts with and without a given radicand.
-- @extractCoeff expr r n@ returns @(without, coeff)@ such that
-- @expr = without + coeff * r^(1\/n)@ and neither @without@ nor @coeff@
-- contains @r^(1\/n)@.
--
-- This is used by 'normalize' (consolidateRadicands) and 'divR' to
-- consolidate all occurrences of a radicand into a single Ext layer,
-- preventing expression blowup and non-termination.
extractCoeff :: Radical -> Radical -> Int -> (Radical, Radical)
extractCoeff (Rational q) _ _ = (Rational q, Rational 0)
extractCoeff (Real d) _ _ = (Real d, Rational 0)
extractCoeff (Ext a b r n) targetR targetN
    | n == targetN && r `radEq` targetR =
        -- This layer has the target radicand: a + b·√target.
        -- Recursively extract √target from a and b too.
        let (aw, ac) = extractCoeff a targetR targetN
            (bw, bc) = extractCoeff b targetR targetN
            -- expr = (aw + ac·√t) + (bw + bc·√t)·√t
            --      = (aw + bc·t) + (ac + bw)·√t
        in (addR aw (mulR bc targetR), addR ac bw)
    | otherwise =
        -- Different radicand. Extract target from sub-expressions.
        let (aw, ac) = extractCoeff a targetR targetN
            (bw, bc) = extractCoeff b targetR targetN
            -- expr = (aw + ac·√target) + (bw + bc·√target)·√r
            --      = (aw + bw·√r) + (ac + bc·√r)·√target
        in (normalize (Ext aw bw r n), normalize (Ext ac bc r n))

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
signR (Real d) =
  if abs d < 1e-15 then 0 else if d < 0 then -1 else 1

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
nthRootC n (Real d)
  | d < 0 && even n = error $ "nthRootC: negative radicand " ++ show d
                            ++ " under even root index " ++ show n
  | d < 0 && odd n  = Real (negate (abs d ** (1 / fromIntegral n)))
  | otherwise        = Real (d ** (1 / fromIntegral n))
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
toDouble (Real d)        = d
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
toKaTeX (Real d) = show d
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

