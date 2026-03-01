-- | Numeric type that tracks which algebraic field a value inhabits.
--
-- All arithmetic is backed by 'Double'. The 'Field' tag records the
-- minimal field containing the value: integers, rationals, algebraic
-- irrationals (from sqrt), cyclomatic algebraics (from cos/sin), or
-- transcendentals (real/complex exponentials).
--
-- Field tags propagate conservatively: operations promote to the more
-- complex field, never demote. This means @cos(pi/3) = 0.5@ is tagged
-- 'FCyclomatic' even though 0.5 is rational — detecting such collapses
-- would require exact arithmetic.
module Flag.Construction.FieldNumber
    ( -- * Types
      Field(..)
    , FieldNumber(..)
      -- * Smart constructors
    , fnInteger
    , fnRational
      -- * Trig wrappers (introduce FCyclomatic)
    , fnCos
    , fnSin
      -- * Field queries
    , fieldOf
    , getValue
      -- * Predicates
    , isZero
    , isInteger
    , isNatural
    , isRational
    , isIrrational
      -- * Conversion
    , toDouble
    , toRational'
      -- * Display
    , toKaTeX
    , showFN
    ) where

import Data.List (find)
import Data.Ratio (Rational, numerator, denominator, (%))

-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

-- | Hierarchy of algebraic fields, ordered by complexity.
-- A 'FieldNumber' value's tag is the minimal field that contains it.
data Field
  = FInteger     -- ^ A whole number: 0, 1, -3
  | FRational    -- ^ A ratio of integers: 1\/3, -7\/5
  | FIrrational  -- ^ Algebraic irrational arising from sqrt: √2, (1+√5)\/2
  | FCyclomatic  -- ^ Algebraic from trig of rational-π multiples: cos(2π\/7)
  | FReal        -- ^ Transcendental: π, e
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A numeric value tagged with its algebraic field.
-- The 'Field' tag cannot decrease: once promoted by an operation, always
-- at least that field within this computation.
data FieldNumber = FieldNumber !Field !Double
  deriving (Show, Read)

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Construct an exact integer.
fnInteger :: Integer -> FieldNumber
fnInteger n = FieldNumber FInteger (fromInteger n)

-- | Construct an exact rational. Detects whole numbers (denominator 1)
-- and tags them as 'FInteger'.
fnRational :: Rational -> FieldNumber
fnRational r
  | denominator r == 1 = FieldNumber FInteger (fromIntegral (numerator r))
  | otherwise          = FieldNumber FRational (fromRational r)

-- | Cosine wrapper — result is always 'FCyclomatic'.
fnCos :: FieldNumber -> FieldNumber
fnCos (FieldNumber _ d) = FieldNumber FCyclomatic (cos d)

-- | Sine wrapper — result is always 'FCyclomatic'.
fnSin :: FieldNumber -> FieldNumber
fnSin (FieldNumber _ d) = FieldNumber FCyclomatic (sin d)

-- ---------------------------------------------------------------------------
-- Field queries
-- ---------------------------------------------------------------------------

-- | The algebraic field this value belongs to.
fieldOf :: FieldNumber -> Field
fieldOf (FieldNumber f _) = f

-- | The underlying Double approximation.
getValue :: FieldNumber -> Double
getValue (FieldNumber _ d) = d

-- | Alias for 'getValue'.
toDouble :: FieldNumber -> Double
toDouble = getValue

-- ---------------------------------------------------------------------------
-- Predicates
-- ---------------------------------------------------------------------------

-- | True if the value is within 1e-15 of zero.
isZero :: FieldNumber -> Bool
isZero (FieldNumber _ d) = abs d < 1e-15

-- | True if the value's field is 'FInteger'.
isInteger :: FieldNumber -> Bool
isInteger (FieldNumber f _) = f == FInteger

-- | True if the value is a non-negative integer.
isNatural :: FieldNumber -> Bool
isNatural (FieldNumber f d) = f == FInteger && d >= 0

-- | True if the field is 'FInteger' or 'FRational'.
isRational :: FieldNumber -> Bool
isRational (FieldNumber f _) = f <= FRational

-- | True if the field is exactly 'FIrrational'.
isIrrational :: FieldNumber -> Bool
isIrrational (FieldNumber f _) = f == FIrrational

-- ---------------------------------------------------------------------------
-- Conversion
-- ---------------------------------------------------------------------------

-- | Reconstruct a 'Rational' for integer and rational values.
-- Uses a small-denominator search (denominators up to 1000).
-- Returns 'Nothing' for 'FIrrational', 'FCyclomatic', and 'FReal'.
toRational' :: FieldNumber -> Maybe Rational
toRational' (FieldNumber FInteger d) = Just (round d % 1)
toRational' (FieldNumber FRational d) =
  case find (\q -> let pf = d * fromIntegral q
                       p  = round pf :: Integer
                   in abs (fromIntegral p - pf) < 1e-9)
            ([1..1000] :: [Integer]) of
    Just q  -> let p = round (d * fromIntegral q) :: Integer in Just (p % q)
    Nothing -> Nothing
toRational' _ = Nothing

-- ---------------------------------------------------------------------------
-- Display
-- ---------------------------------------------------------------------------

-- | Human-readable display with field annotation.
showFN :: FieldNumber -> String
showFN (FieldNumber f d) = show d ++ " [" ++ show f ++ "]"

-- | KaTeX rendering for debug overlays.
toKaTeX :: FieldNumber -> String
toKaTeX (FieldNumber FInteger d) = show (round d :: Integer)
toKaTeX (FieldNumber FRational d) = show d
toKaTeX (FieldNumber _ d) = "\\approx " ++ show d

-- ---------------------------------------------------------------------------
-- Internal: field promotion
-- ---------------------------------------------------------------------------

promoteField :: Field -> Field -> Field
promoteField = max

-- | Determine the field for a division result when both inputs are at
-- most 'FRational'. Integer-valued quotients stay 'FInteger'; others
-- become 'FRational'.
divField :: Field -> Field -> Double -> Field
divField f1 f2 result
  | f1 <= FRational && f2 <= FRational =
      if abs (result - fromInteger (round result)) < 1e-9
        then FInteger
        else FRational
  | otherwise = promoteField f1 f2

-- | Detect the field for the square root of a rational\/integer Double.
-- Checks for near-integer results (→ 'FInteger') and small-denominator
-- rational results (→ 'FRational'). Everything else → 'FIrrational'.
detectSqrtField :: Double -> FieldNumber
detectSqrtField d
  | d == 0    = FieldNumber FInteger 0
  | d < 0     = error "FieldNumber.sqrt: negative argument"
  | otherwise =
      let s = sqrt d
      in if isNearInt s
           then FieldNumber FInteger (fromInteger (round s))
           else case find (\q -> isNearInt (s * fromIntegral q))
                         ([2..12] :: [Integer]) of
                  Just _  -> FieldNumber FRational s
                  Nothing -> FieldNumber FIrrational s
  where
    isNearInt x = abs (x - fromInteger (round x)) < 1e-9

-- ---------------------------------------------------------------------------
-- Type class instances
-- ---------------------------------------------------------------------------

-- | Approximate equality: values within 1e-9 are considered equal.
-- Field tags are ignored for equality; only the Double value matters.
instance Eq FieldNumber where
  FieldNumber _ d1 == FieldNumber _ d2 = abs (d1 - d2) < 1e-9

-- | Ordering consistent with 'Eq': values within 1e-9 compare as 'EQ'.
instance Ord FieldNumber where
  compare (FieldNumber _ d1) (FieldNumber _ d2)
    | abs (d1 - d2) < 1e-9 = EQ
    | d1 < d2               = LT
    | otherwise             = GT

instance Num FieldNumber where
  FieldNumber f1 d1 + FieldNumber f2 d2 = FieldNumber (promoteField f1 f2) (d1 + d2)
  FieldNumber f1 d1 - FieldNumber f2 d2 = FieldNumber (promoteField f1 f2) (d1 - d2)
  FieldNumber f1 d1 * FieldNumber f2 d2 = FieldNumber (promoteField f1 f2) (d1 * d2)
  negate (FieldNumber f d) = FieldNumber f (negate d)
  abs    (FieldNumber f d) = FieldNumber f (abs d)
  -- signum result is always -1, 0, or 1 — tag as FInteger regardless of input
  signum (FieldNumber _ d) = FieldNumber FInteger (signum d)
  fromInteger n = FieldNumber FInteger (fromInteger n)

instance Fractional FieldNumber where
  FieldNumber f1 d1 / FieldNumber f2 d2 =
    let result = d1 / d2
    in FieldNumber (divField f1 f2 result) result
  fromRational r
    | denominator r == 1 = FieldNumber FInteger (fromIntegral (numerator r))
    | otherwise          = FieldNumber FRational (fromRational r)

instance Floating FieldNumber where
  sqrt (FieldNumber f d)
    | f <= FRational = detectSqrtField d
    | otherwise      = FieldNumber f (sqrt d)

  pi = FieldNumber FReal 3.141592653589793

  cos  = fnCos
  sin  = fnSin
  tan  (FieldNumber _ d) = FieldNumber FCyclomatic (tan d)
  asin (FieldNumber _ d) = FieldNumber FCyclomatic (asin d)
  acos (FieldNumber _ d) = FieldNumber FCyclomatic (acos d)
  atan (FieldNumber _ d) = FieldNumber FCyclomatic (atan d)

  exp  (FieldNumber _ d) = FieldNumber FReal (exp d)
  log  (FieldNumber _ d) = FieldNumber FReal (log d)

  sinh  (FieldNumber _ d) = FieldNumber FReal (sinh d)
  cosh  (FieldNumber _ d) = FieldNumber FReal (cosh d)
  tanh  (FieldNumber _ d) = FieldNumber FReal (tanh d)
  asinh (FieldNumber _ d) = FieldNumber FReal (asinh d)
  acosh (FieldNumber _ d) = FieldNumber FReal (acosh d)
  atanh (FieldNumber _ d) = FieldNumber FReal (atanh d)
