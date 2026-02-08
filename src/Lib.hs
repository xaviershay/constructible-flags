{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( Construction(..)
    , natural
    , naturalProportion
    , runConstructionPure
    , runConstructionSVG
    , runConstructionTrace
    , Sourced(..)
    , Source(..)
    , sourced
    , sourcedM
    , runSourcedPure
    , runSourcedTrace
    , runSourcedCollect
    , PantoneId(..)
    , pmsToRGB
    , Flag(..)
    , france
    , allCountryFlags
    ) where

import Diagrams.Prelude hiding (trace, Dynamic)
import Diagrams.Backend.SVG
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (runState, modify)

type URL = String
type Title = String

-- | Source information for attributed data
data Source = SourceHabitual | SourceAuthoritativeWebsite Title URL | SourceLaw Title URL | SourcePublication Title URL
  deriving (Show, Eq)

-- | A flag with its metadata and construction
data Flag es = CountryFlag
  { flagIsoCode     :: String
  , flagName        :: String
  , flagDescription :: Eff es String
  , flagDesign      :: Eff es (Diagram B)
  }

sourceDescription :: Source -> Maybe String
sourceDescription (SourcePublication "London Olympics 2012 Flag & Anthems Manual" _) = Just "The London Olympic committee specified PMS color values as part of their flag manual, which were approved by relevant governments. Flag manuals from Olympics since (last checked 2024) do not contain PMS values."
sourceDescription _ = Nothing

londonOlympics2012 :: Source
londonOlympics2012 = SourcePublication
  "London Olympics 2012 Flag & Anthems Manual"
  "https://library.olympics.com/Default/doc/SYRACUSE/34593/flags-and-anthems-manual-london-2012-spp-final-version-london-organising-committee-of-the-olympic-ga?_lg=en-GB"

pantone :: Source
pantone = SourceAuthoritativeWebsite "Pantone" "https://www.pantone.com/"

habitual :: Source
habitual = SourceHabitual

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
    let srcDesc = case src of
          SourceHabitual -> "habitual"
          SourceAuthoritativeWebsite title _ -> title
          SourceLaw title _ -> title
          SourcePublication title _ -> title
    modify (++ [name ++ " sourced from " ++ srcDesc])
    pure val

-- | Interpreter that collects all Source values
runSourcedCollect :: Eff (Sourced : es) a -> Eff es (a, [Source])
runSourcedCollect = reinterpret_ (runState @[Source] []) $ \case
  GetSourced _ src val -> do
    modify (++ [src])
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

-- | Interpreter that just evaluates constructions (for any return type)
runConstructionPure :: Eff (Construction : es) a -> Eff es a
runConstructionPure = interpret_ $ \case
  Natural n -> pure (fromIntegral n)

-- | Interpreter that traces all construction operations
runConstructionTrace :: Eff (Construction : es) a -> Eff es (a, [String])
runConstructionTrace = reinterpret_ (runState @[String] []) $ \case
  Natural n -> do
    modify (++ ["natural " ++ show n])
    pure (fromIntegral n)

-- | Pantone color identifier
data PantoneId =
    PMSRed032C
  | PMSReflexBlueC
  deriving (Show, Eq)

-- | Convert a Pantone identifier to RGB color (sourced from Pantone website)
pmsToRGB :: Sourced :> es => PantoneId -> Eff es (Colour Double)
pmsToRGB PMSRed032C = sourced "RGB Conversion" pantone (sRGB24 230 49 62)
pmsToRGB PMSReflexBlueC = sourced "RGB Conversion" pantone (sRGB24 16 11 136)


france :: (Construction :> es, Sourced :> es) => Flag es
france = CountryFlag
  { flagIsoCode = "FRA"
  , flagName = "France"
  , flagDescription = sourced "Description" constitution "The national emblem is the tricolour flag, blue, white, red."
  , flagDesign = design
  }

  where
    govWebsite = SourceAuthoritativeWebsite
        "Official French Government Color Guidelines"
        "https://www.info.gouv.fr/marque-de-letat/les-couleurs#les-couleurs-principales"

    --[ "https://www.info.gouv.fr/upload/media/default/0001/08/8df5f17cebb84f2c19a9953154719c80086d6c3b.png"
    --]
    constitution = SourceLaw
        "French Constitution, Article 2 (translated)"
        "https://www.legifrance.gouv.fr/loda/article_lc/LEGIARTI000006527453"

    design :: (Construction :> es, Sourced :> es) => Eff es (Diagram B)
    design = do
        (w, h) <- sourcedM "2:3 proportion" habitual $ naturalProportion 1 2
        blueColor <- sourced "Blue" govWebsite (sRGB24 0 0 145)
        whiteColor <- sourced "White" govWebsite (sRGB24 255 255 255)
        redColor <- sourced "Red" govWebsite (sRGB24 255 0 14)
        let stripe c = rect w h # fc c # lw none
        pure $ hcat $ map stripe [blueColor, whiteColor, redColor]

allCountryFlags :: [Flag (Sourced : Construction : '[])]
allCountryFlags =
    [ france
    ]