{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Source
    ( Source(..)
    , Agent(..)
    , Entity(..)
    , Sourced(..)
    , SourcedElement
    , sourced
    , sourcedM
    , reference
    , impliedReference
    , unsightedReference
    , editorial
    , mkAgentOrg
    , mkEntity
    , attributeTo
    , screenshot
    , translated
    , runSourcedPure
    , runSourcedTrace
    , runSourcedCollect
    ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (runState, modify)

-- | An agent responsible for a source entity
data Agent = AgentOrg
  { agentId   :: String
  , agentName :: String
  } deriving (Show, Eq)

-- | A source entity (document, specification, etc.)
data Entity = Entity
  { entityTitle      :: String
  , entityUrl        :: String
  , entityAgent      :: Maybe Agent
  , entityScreenshot :: Maybe (String, String)  -- (date, imagePath)
  , entityTranslated :: Maybe String            -- date of translation access
  } deriving (Show, Eq)

-- | Source information for attributed data
data Source
  = SourceReference Entity
  | SourceImpliedReference Entity
  | SourceUnsightedReference Entity [Entity]
  | SourceEditorial [Entity]
  deriving (Show, Eq)

-- | Effect for sourced/attributed values
data Sourced :: Effect where
  GetSourced :: String -> Source -> a -> Sourced m a

type instance DispatchOf Sourced = 'Dynamic

-- | Low-level sourcing function
sourced :: Sourced :> es => String -> Source -> a -> Eff es a
sourced name src val = send (GetSourced name src val)

-- | Monadic variant of 'sourced' that runs an effectful computation first
sourcedM :: Sourced :> es => String -> Source -> Eff es a -> Eff es a
sourcedM name src m = m >>= sourced name src

-- | Value explicitly referenced from a document
reference :: Sourced :> es => String -> Entity -> a -> Eff es a
reference name entity val = sourced name (SourceReference entity) val

-- | Value implied by a document (not explicitly stated)
impliedReference :: Sourced :> es => String -> Entity -> a -> Eff es a
impliedReference name entity val = sourced name (SourceImpliedReference entity) val

-- | Value from an unsighted specification, corroborated by other references
unsightedReference :: Sourced :> es => String -> Entity -> [Entity] -> a -> Eff es a
unsightedReference name entity refs val = sourced name (SourceUnsightedReference entity refs) val

-- | Editorial decision by the editor, with supporting references
editorial :: Sourced :> es => String -> [Entity] -> a -> Eff es a
editorial name refs val = sourced name (SourceEditorial refs) val

-- | Create an agent representing an organization
mkAgentOrg :: String -> String -> Agent
mkAgentOrg = AgentOrg

-- | Create a source entity with title and URL
mkEntity :: String -> String -> Entity
mkEntity title url = Entity
  { entityTitle      = title
  , entityUrl        = url
  , entityAgent      = Nothing
  , entityScreenshot = Nothing
  , entityTranslated = Nothing
  }

-- | Attribute an entity to an agent
attributeTo :: Agent -> Entity -> Entity
attributeTo agent entity = entity { entityAgent = Just agent }

-- | Attach a screenshot to an entity
screenshot :: String -> String -> Entity -> Entity
screenshot date path entity = entity { entityScreenshot = Just (date, path) }

-- | Mark an entity as a translation (accessed at given date)
translated :: String -> Entity -> Entity
translated date entity = entity { entityTranslated = Just date }

-- | Interpreter that just returns the value (ignores source metadata)
runSourcedPure :: Eff (Sourced : es) a -> Eff es a
runSourcedPure = interpret_ $ \case
  GetSourced _ _ val -> pure val

-- | Interpreter that traces all sourced values
runSourcedTrace :: Eff (Sourced : es) a -> Eff es (a, [String])
runSourcedTrace = reinterpret_ (runState @[String] []) $ \case
  GetSourced name src val -> do
    let srcDesc = case src of
          SourceReference entity -> entityTitle entity
          SourceImpliedReference entity -> entityTitle entity ++ " (implied)"
          SourceUnsightedReference entity _ -> entityTitle entity ++ " (unsighted)"
          SourceEditorial _ -> "editorial decision"
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
