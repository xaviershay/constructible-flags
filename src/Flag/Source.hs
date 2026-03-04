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
    , SourcedElement(..)
    , elementDisplayPair
    , sourced
    , sourcedM
    , reference
    , impliedReference
    , unsightedReference
    , editorial
    , approximation
    , derivedFrom
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
  , entityScreenshots :: [(String, String)]  -- [(date, imagePath)]
  , entityTranslated :: Maybe String            -- date of translation access
  } deriving (Show, Eq)

-- | Source information for attributed data
data Source
  = SourceReference Entity
  | SourceImpliedReference Entity
  | SourceUnsightedReference Entity [Entity]
  | SourceEditorial [Entity]
  | SourceApproximation String [Entity]
  -- ^ Editorial choice of a value to approximate a previously-sourced attribute.
  -- The 'String' names the attribute being approximated; the '[Entity]' are
  -- supporting references consulted.  Produces 'wasInfluencedBy' in PROV
  -- (not 'wasDerivedFrom') because the relationship is editorial, not causal.
  deriving (Show, Eq)

-- | Effect for sourced/attributed values
data Sourced :: Effect where
  GetSourced :: String -> Source -> a -> Sourced m a
  GetDerived :: String   -- ^ label for this attribute
             -> String   -- ^ label of the parent attribute it was derived from
             -> Entity   -- ^ entity used during the derivation (e.g. a Pantone chip)
             -> a
             -> Sourced m a

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

-- | An attribute chosen editorially to approximate a previously-sourced
-- attribute (e.g. selecting a Pantone code to match a dye name in a spec).
-- @approxOf@ must match the name used when the approximated attribute was
-- sourced, so the PROV graph can emit a @wasInfluencedBy@ link.
approximation :: Sourced :> es => String -> String -> [Entity] -> a -> Eff es a
approximation name approxOf refs val = sourced name (SourceApproximation approxOf refs) val

-- | Value derived from a previously-sourced attribute, via a specific entity.
-- Use this when one attribute is computed from another (e.g. Pantone→RGB).
-- The parent attribute name creates a wasDerivedFrom link in PROV output,
-- and the via-entity becomes the subject of a color-sample (or similar) activity.
derivedFrom :: Sourced :> es => String -> String -> Entity -> a -> Eff es a
derivedFrom name from entity val = send (GetDerived name from entity val)

-- | Create an agent representing an organization
mkAgentOrg :: String -> String -> Agent
mkAgentOrg = AgentOrg

-- | Create a source entity with title and URL
mkEntity :: String -> String -> Entity
mkEntity title url = Entity
  { entityTitle      = title
  , entityUrl        = url
  , entityAgent      = Nothing
  , entityScreenshots = []
  , entityTranslated = Nothing
  }

-- | Attribute an entity to an agent
attributeTo :: Agent -> Entity -> Entity
attributeTo agent entity = entity { entityAgent = Just agent }

-- | Attach a screenshot to an entity.  Multiple calls accumulate; all
-- screenshots are stored and displayed in order.
screenshot :: String -> String -> Entity -> Entity
screenshot date path entity = entity { entityScreenshots = (date, path) : entityScreenshots entity }

-- | Mark an entity as a translation (accessed at given date)
translated :: String -> Entity -> Entity
translated date entity = entity { entityTranslated = Just date }

-- | Interpreter that just returns the value (ignores source metadata)
runSourcedPure :: Eff (Sourced : es) a -> Eff es a
runSourcedPure = interpret_ $ \case
  GetSourced _ _ val     -> pure val
  GetDerived _ _ _ val   -> pure val

-- | Interpreter that traces all sourced values
runSourcedTrace :: Eff (Sourced : es) a -> Eff es (a, [String])
runSourcedTrace = reinterpret_ (runState @[String] []) $ \case
  GetSourced name src val -> do
    let srcDesc = case src of
          SourceReference entity -> entityTitle entity
          SourceImpliedReference entity -> entityTitle entity ++ " (implied)"
          SourceUnsightedReference entity _ -> entityTitle entity ++ " (unsighted)"
          SourceEditorial _ -> "editorial decision"
          SourceApproximation approxOf _ -> "approximation of " ++ approxOf
    modify (++ [name ++ " sourced from " ++ srcDesc])
    pure val
  GetDerived name from _ val -> do
    modify (++ [name ++ " derived from " ++ from])
    pure val

-- | A sourced element: either directly sourced from a document, or derived
-- from another named attribute via a specific entity (e.g. Pantone chip).
data SourcedElement
  = SourcedAttr String Source
  | DerivedAttr String String Entity
  -- ^ DerivedAttr name parentAttrName viaEntity
  deriving (Show, Eq)

-- | Convert any 'SourcedElement' to a @(name, Source)@ pair suitable for
-- display purposes. 'DerivedAttr' is rendered as a 'SourceReference' to
-- its via-entity.
elementDisplayPair :: SourcedElement -> (String, Source)
elementDisplayPair (SourcedAttr name src) = (name, src)
elementDisplayPair (DerivedAttr name _ e) = (name, SourceReference e)

-- | Interpreter that collects all sourced elements with their sources
runSourcedCollect :: Eff (Sourced : es) a -> Eff es (a, [SourcedElement])
runSourcedCollect = reinterpret_ (runState @[SourcedElement] []) $ \case
  GetSourced name src val -> do
    modify (++ [SourcedAttr name src])
    pure val
  GetDerived name from entity val -> do
    modify (++ [DerivedAttr name from entity])
    pure val
