{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Source
    ( Source(..)
    , Sourced(..)
    , SourcedElement
    , sourced
    , sourcedM
    , runSourcedPure
    , runSourcedTrace
    , runSourcedCollect
    , sourceDescription
    ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (runState, modify)

type URL = String
type Title = String

-- | Source information for attributed data
data Source = SourceHabitual | SourceAuthoritativeWebsite Title URL | SourceLaw Title URL | SourcePublication Title URL
  deriving (Show, Eq)

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

sourceDescription :: Source -> Maybe String
sourceDescription (SourcePublication "London Olympics 2012 Flag & Anthems Manual" _) = Just "The London Olympic committee specified PMS color values as part of their flag manual, which were approved by relevant governments. Flag manuals from Olympics since (last checked 2024) do not contain PMS values."
sourceDescription _ = Nothing
