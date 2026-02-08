{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( frenchFlag
    , Construction(..)
    , natural
    , naturalProportion
    , runConstructionSVG
    , runConstructionTrace
    , Sourced(..)
    , Source
    , mkSource
    , sourced
    , sourcedM
    , runSourcedPure
    , runSourcedTrace
    , PantoneId(..)
    , pmsToRGB
    , Flag(..)
    ) where

import Diagrams.Prelude hiding (trace, Dynamic)
import Diagrams.Backend.SVG
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (runState, modify)

-- | Source information for attributed data
data Source = Source
  { sourceId          :: String
  , sourceUrl         :: String
  , sourceDescription :: String
  , sourceYear        :: Int
  }
  deriving (Show, Eq)

-- | Create a Source with the given information
mkSource :: String -> String -> String -> Int -> Source
mkSource = Source

-- | A flag with its metadata and construction
data Flag es = CountryFlag
  { flagIsoCode     :: String
  , flagName        :: String
  , flagDescription :: Eff es String
  , flagDesign      :: Eff es (Diagram B)
  }

londonOlympics2012 :: Source
londonOlympics2012 = mkSource
  "LondonOlympics2012"
  "https://library.olympics.com/Default/doc/SYRACUSE/34593/flags-and-anthems-manual-london-2012-spp-final-version-london-organising-committee-of-the-olympic-ga?_lg=en-GB"
  "The London Olympic committee specified PMS color values as part of their flag manual, which were approved by relevant governments. Flag manuals from Olympics since (last checked 2024) do not contain PMS values."
  2012

pantone :: Source
pantone = mkSource
  "Pantone"
  "https://www.pantone.com/"
  "RGB approximation used by Pantone for its own colors, using C (coated) variants."
  2024

fotw :: Source
fotw = mkSource
  "Flags of the World"
  "https://www.crwflags.com/fotw/flags/index.html"
  "Self-proclaimed \"largest site devoted to vexillology\". Used as a secondary source when primary has not yet been identified."
  2024

habitual :: Source
habitual = mkSource
  "Habitual"
  ""
  "No primary source exists but unanimous agreement among other sources."
  2024

-- | Effect for sourced/attributed values
data Sourced :: Effect where
  GetSourced :: String -> Source -> a -> Sourced m a

type instance DispatchOf Sourced = 'Dynamic

-- | Get a value with its source attribution
-- First argument is a name describing what is being sourced (e.g., "Red", "Blue", "RGB Conversion")
sourced :: Sourced :> es => String -> Source -> a -> Eff es a
sourced name src val = send (GetSourced name src val)

-- | Monadic variant of 'sourced' that runs an effectful computation first
sourcedM :: Sourced :> es => String -> Source -> Eff es a -> Eff es a
sourcedM name src m = m >>= sourced name src

-- | Interpreter that just returns the value (ignores source metadata)
runSourcedPure :: Eff (Sourced : es) a -> Eff es a
runSourcedPure = interpret_ $ \case
  GetSourced _ _ val -> pure val

-- | Interpreter that traces all sourced values
runSourcedTrace :: Eff (Sourced : es) a -> Eff es (a, [String])
runSourcedTrace = reinterpret_ (runState @[String] []) $ \case
  GetSourced name src val -> do
    modify (++ [name ++ " sourced from " ++ sourceId src ++ " (" ++ show (sourceYear src) ++ ")"])
    pure val

-- | Effect for geometric constructions
data Construction :: Effect where
  -- | Get a natural number (1, 2, 3, ...)
  Natural :: Int -> Construction m Double

type instance DispatchOf Construction = 'Dynamic

-- | Get a natural number as a constructible value
natural :: Construction :> es => Int -> Eff es Double
natural n = send (Natural n)

-- | Get a proportion as a pair of natural numbers (width, height)
naturalProportion :: Construction :> es => Int -> Int -> Eff es (Double, Double)
naturalProportion w h = (,) <$> natural w <*> natural h

-- | Interpreter that produces an SVG diagram
runConstructionSVG :: Eff (Construction : es) (Diagram B) -> Eff es (Diagram B)
runConstructionSVG = interpret_ $ \case
  Natural n -> pure (fromIntegral n)

-- | Interpreter that traces all construction operations
runConstructionTrace :: Eff (Construction : es) a -> Eff es (a, [String])
runConstructionTrace = reinterpret_ (runState @[String] []) $ \case
  Natural n -> do
    modify (++ ["natural " ++ show n])
    pure (fromIntegral n)

-- | Pantone color identifier
data PantoneId =
     PMSRed032
  | PMSReflexBlue
  deriving (Show, Eq)

-- | Convert a Pantone identifier to RGB color (sourced from Pantone website)
pmsToRGB :: Sourced :> es => PantoneId -> Eff es (Colour Double)
pmsToRGB PMSRed032 = sourced "RGB Conversion" pantone (sRGB24 230 49 62)
pmsToRGB PMSReflexBlue = sourced "RGB Conversion" pantone (sRGB24 16 11 136)

-- French flag: three vertical stripes (blue, white, red)
frenchFlag :: (Construction :> es, Sourced :> es) => Eff es (Diagram B)
frenchFlag = do
  (w, h) <- sourcedM "2:3 proportion" habitual $ naturalProportion 1 2
  redColor <- sourced "Red" londonOlympics2012 PMSRed032 >>= pmsToRGB
  whiteColor <- sourced "White" habitual white
  blueColor <- sourced "Blue" londonOlympics2012 PMSReflexBlue >>= pmsToRGB
  let stripe c = rect w h # fc c # lw none
  pure $ hcat $ map stripe [blueColor, whiteColor, redColor]

france :: (Construction :> es, Sourced :> es) => Flag es
france = CountryFlag
  { flagIsoCode = "FRA"
  , flagName = "France"
  , flagDescription = sourced "Description" constitution "The national emblem is the tricolour flag, blue, white, red."
  , flagDesign = frenchFlag
  }

  where
    constitution = mkSource
        "FrenchConstitution"
        "https://www.legifrance.gouv.fr/loda/article_lc/LEGIARTI000006527453"
        "French Constitution, Article 2 (translated)"
        2024