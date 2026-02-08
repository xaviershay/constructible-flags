{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( Construction(..)
    , ConstructionElement(..)
    , natural
    , naturalProportion
    , runConstructionPure
    , runConstructionSVG
    , runConstructionTrace
    , runConstructionCollect
    , Sourced(..)
    , Source(..)
    , SourcedElement
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

import Diagrams.Prelude hiding (trace, Dynamic, boxCenter)
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

-- | A sourced element: the element name and its source
type SourcedElement = (String, Source)

-- | Interpreter that collects all sourced elements with their sources
runSourcedCollect :: Eff (Sourced : es) a -> Eff es (a, [SourcedElement])
runSourcedCollect = reinterpret_ (runState @[SourcedElement] []) $ \case
  GetSourced name src val -> do
    modify (++ [(name, src)])
    pure val

-- | Effect for geometric constructions
data Construction :: Effect where
  -- | Get a natural number (1, 2, 3, ...)
  Natural :: Int -> Construction m Double
  -- | Get a rational number as a ratio of two naturals
  Rational :: Int -> Int -> Construction m Double
  -- | Get the center point of a box with given (width, height)
  BoxCenter :: (Double, Double) -> Construction m (Double, Double)

type instance DispatchOf Construction = 'Dynamic

-- | Get a natural number as a constructible value
natural :: Construction :> es => Int -> Eff es Double
natural n = send (Natural n)

-- | Get a rational number as a ratio of two naturals
rational :: Construction :> es => Int -> Int -> Eff es Double
rational num denom = send (Rational num denom)

-- | Get the center point of a box with given (width, height)
boxCenter :: Construction :> es => (Double, Double) -> Eff es (Double, Double)
boxCenter dims = send (BoxCenter dims)

-- | Get a proportion as a pair of natural numbers (width, height)
naturalProportion :: Construction :> es => Int -> Int -> Eff es (Double, Double)
naturalProportion w h = (,) <$> natural w <*> natural h

-- | Interpreter that produces an SVG diagram
runConstructionSVG :: Eff (Construction : es) (Diagram B) -> Eff es (Diagram B)
runConstructionSVG = interpret_ $ \case
  Natural n -> pure (fromIntegral n)
  Rational num denom -> pure (fromIntegral num / fromIntegral denom)
  BoxCenter (w, h) -> pure (w / 2, h / 2)

-- | Interpreter that just evaluates constructions (for any return type)
runConstructionPure :: Eff (Construction : es) a -> Eff es a
runConstructionPure = interpret_ $ \case
  Natural n -> pure (fromIntegral n)
  Rational num denom -> pure (fromIntegral num / fromIntegral denom)
  BoxCenter (w, h) -> pure (w / 2, h / 2)

-- | Interpreter that traces all construction operations
runConstructionTrace :: Eff (Construction : es) a -> Eff es (a, [String])
runConstructionTrace = reinterpret_ (runState @[String] []) $ \case
  Natural n -> do
    modify (++ ["natural " ++ show n])
    pure (fromIntegral n)
  Rational num denom -> do
    modify (++ ["rational " ++ show num ++ "/" ++ show denom])
    pure (fromIntegral num / fromIntegral denom)
  BoxCenter (w, h) -> do
    modify (++ ["boxCenter (" ++ show w ++ ", " ++ show h ++ ")"])
    pure (w / 2, h / 2)

-- | A construction element: the technique name and its parameter
data ConstructionElement
  = ConstructionNatural Int
  | ConstructionRational Int Int
  | ConstructionBoxCenter (Double, Double)
  deriving (Show, Eq)

-- | Interpreter that collects all construction operations
runConstructionCollect :: Eff (Construction : es) a -> Eff es (a, [ConstructionElement])
runConstructionCollect = reinterpret_ (runState @[ConstructionElement] []) $ \case
  Natural n -> do
    modify (++ [ConstructionNatural n])
    pure (fromIntegral n)
  Rational num denom -> do
    modify (++ [ConstructionRational num denom])
    pure (fromIntegral num / fromIntegral denom)
  BoxCenter dims@(w, h) -> do
    modify (++ [ConstructionBoxCenter dims])
    pure (w / 2, h / 2)

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
  , flagDescription = sourced "Description" constitution "A tricolour flag, blue, white, red."
  , flagDesign = design
  }

  where
    govWebsite = SourceAuthoritativeWebsite
        "Official French Government Color Guidelines"
        "https://www.info.gouv.fr/marque-de-letat/les-couleurs#les-couleurs-principales"

    --[ "https://www.info.gouv.fr/upload/media/default/0001/08/8df5f17cebb84f2c19a9953154719c80086d6c3b.png"
    --]
    constitution = SourceLaw
        "French Constitution, Article 2"
        "https://www.legifrance.gouv.fr/loda/article_lc/LEGIARTI000006527453"

    design :: (Construction :> es, Sourced :> es) => Eff es (Diagram B)
    design = do
        (w, h) <- sourcedM "2:3 proportion" habitual $ naturalProportion 1 2
        blueColor <- sourced "Blue" govWebsite (sRGB24 0 0 145)
        whiteColor <- sourced "White" govWebsite (sRGB24 255 255 255)
        redColor <- sourced "Red" govWebsite (sRGB24 255 0 14)
        let stripe c = rect w h # fc c # lw none
        pure $ hcat $ map stripe [blueColor, whiteColor, redColor]

japan :: (Construction :> es, Sourced :> es) => Flag es
japan = CountryFlag
  { flagIsoCode = "JPN"
  , flagName = "Japan"
  , flagDescription = sourced "Description" flagLaw "A white rectangular flag with a red disc at the center."
  , flagDesign = design
  }

  where
    flagLaw = SourceLaw
        "Act on National Flag and Anthem (Law #127 of 1999)"
        "https://elaws.e-gov.go.jp/document?lawid=411AC0000000127"

    govWebsite = SourceAuthoritativeWebsite
      "Official Government Website"
      "https://www.japan.go.jp/japan/flagandanthem/index.html"
      -- Exact colors are not specified in any primay document. Those used here
      -- match those used in the example flag given by current government
      -- website. Other common cited goverment sources are not available online
      -- to verify, and are also much older (latest being 2008).

    design :: (Construction :> es, Sourced :> es) => Eff es (Diagram B)
    design = do
        -- 2:3 proportion (height:width)
        (h, w) <- sourcedM "2:3 proportion" flagLaw $ naturalProportion 2 3

        (cx, cy) <- boxCenter (w, h)

        discRadius <- sourcedM "Disc Height" flagLaw $ rational 3 5

        whiteColor <- sourced "White" govWebsite (sRGB24 255 255 255)
        redColor <- sourced "Red" govWebsite (sRGB24 245 43 42)

        let background = rect w h # alignBL # fc whiteColor # lw none
        let disc = circle discRadius # fc redColor # lw none # moveTo (p2 (cx, cy))

        pure $ disc <> background

allCountryFlags :: [Flag (Sourced : Construction : '[])]
allCountryFlags =
    [ france
    , japan
    ]