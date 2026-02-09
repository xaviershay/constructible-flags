{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Lib
    ( Sourced(..)
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
    , japan
    , allCountryFlags
    ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (runState, modify)
import Data.Colour
import Data.Colour.SRGB (sRGB24)
import Control.Arrow (returnA)

import FlagConstruction

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
  , flagDesign      :: Eff es (FlagA (Point, Point) Drawing)
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

-- | Pantone color identifier
data PantoneId =
    PMSRed032C
  | PMSReflexBlueC
  deriving (Show, Eq)

-- | Convert a Pantone identifier to RGB color (sourced from Pantone website)
pmsToRGB :: Sourced :> es => PantoneId -> Eff es (Colour Double)
pmsToRGB PMSRed032C = sourced "RGB Conversion" pantone (sRGB24 230 49 62)
pmsToRGB PMSReflexBlueC = sourced "RGB Conversion" pantone (sRGB24 16 11 136)


france :: Sourced :> es => Flag es
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

    constitution = SourceLaw
        "French Constitution, Article 2"
        "https://www.legifrance.gouv.fr/loda/article_lc/LEGIARTI000006527453"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        blueColor  <- sourced "Blue"  govWebsite (sRGB24 0 0 145)
        whiteColor <- sourced "White" govWebsite (sRGB24 255 255 255)
        redColor   <- sourced "Red"   govWebsite (sRGB24 255 0 14)
        _ <- sourced "2:3 proportion" habitual ()
        pure $ proc (a, b) -> do
            c <- naturalMult 2 -< (a, b)
            d <- naturalMult 2 -< (b, c)

            d1 <- fillBox blueColor  1 2 -< (a, b)
            d2 <- fillBox whiteColor 1 2 -< (b, c)
            d3 <- fillBox redColor   1 2 -< (c, d)

            returnA -< d1 <> d2 <> d3

allCountryFlags :: [Flag (Sourced : '[])]
allCountryFlags =
    [ france
    , japan
    ]

japan :: Sourced :> es => Flag es
japan = CountryFlag
  { flagIsoCode = "JPN"
  , flagName = "Japan"
  , flagDescription = sourced "Description" flagLaw "A white rectangular flag with a crimson disc at the center."
  , flagDesign = design
  }

  where
    flagLaw = SourceLaw
        "Act on National Flag and Anthem (Law #127 of 1999)"
        "https://elaws.e-gov.go.jp/document?lawid=411AC0000000127"

    design :: Sourced :> es => Eff es (FlagA (Point, Point) Drawing)
    design = do
        whiteColor <- sourced "White" flagLaw (sRGB24 255 2 255)
        redColor   <- sourced "Crimson" flagLaw (sRGB24 188 0 45)
        (h, w) <- sourced "2:3 proportion" flagLaw (2, 3)
        (n, d) <- sourced "Disc diameter 3/5 of height" flagLaw (3, 5)
        pure $ proc (a, b) -> do
            (tl, tr, br, bl) <- boxNatural w h -< (a, b)

            -- White background
            bg <- fillRectangle whiteColor -< (tl, tr, br, bl)

            ---- Center: intersection of diagonals
            center <- intersectLL -< ((tl, br), (tr, bl))

            edge <- rationalMult n d -< (center, tl)

            disc <- fillCircle redColor -< (center, edge)

            returnA -< bg <> disc
