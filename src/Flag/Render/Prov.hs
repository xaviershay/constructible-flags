{-# LANGUAGE OverloadedStrings #-}
module Flag.Render.Prov
    ( generateProvJson
    ) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key (fromString)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)
import Data.List (nub)

import Flag.Source (Source(..), Entity(..), Agent(..), SourcedElement(..))

-- | Generate a W3C PROV-JSON document for a flag's provenance.
generateProvJson :: String -> String -> [SourcedElement] -> String
generateProvJson isoCode flagName' sources =
  BSL.unpack $ encodePretty doc
  where
    isoLower = map toLower isoCode

    -- Namespace helper
    cf :: String -> String
    cf x = "cf:" ++ x

    -- QName type annotation for PROV-JSON
    qname :: String -> Value
    qname n = object ["$" .= n, "type" .= ("xsd:QName" :: String)]

    -- Build a JSON object from a list of dynamic string keys
    makeMap :: [(String, Value)] -> Value
    makeMap kvs = object [(fromString k, v) | (k, v) <- kvs]

    -- Number relation Values to create unique blank node IDs
    numberedRels :: String -> [Value] -> [(String, Value)]
    numberedRels prefix rels =
      zipWith (\i v -> ("_:" ++ prefix ++ show (i :: Int), v)) [1..] rels

    -- -------------------------------------------------------------------------
    -- Collect unique entities and agents from sources
    -- -------------------------------------------------------------------------

    allEntities :: [Entity]
    allEntities = nub [e | se <- sources, e <- sourceEntities se]

    sourceEntities :: SourcedElement -> [Entity]
    sourceEntities (SourcedAttr _ (SourceReference e))              = [e]
    sourceEntities (SourcedAttr _ (SourceImpliedReference e))       = [e]
    sourceEntities (SourcedAttr _ (SourceUnsightedReference e refs)) = e : refs
    sourceEntities (SourcedAttr _ (SourceEditorial refs))            = refs
    sourceEntities (SourcedAttr _ (SourceApproximation _ refs))      = refs
    sourceEntities (DerivedAttr _ _ e)                               = [e]

    -- The primary source entity for a directly-sourced attribute (Nothing for editorial)
    primaryEntity :: Source -> Maybe Entity
    primaryEntity (SourceReference e)            = Just e
    primaryEntity (SourceImpliedReference e)     = Just e
    primaryEntity (SourceUnsightedReference e _) = Just e
    primaryEntity (SourceEditorial _)            = Nothing
    primaryEntity (SourceApproximation _ _)      = Nothing

    allAgents :: [Agent]
    allAgents = nub [a | se <- sources, a <- sourceAgents se]

    sourceAgents :: SourcedElement -> [Agent]
    sourceAgents (SourcedAttr _ (SourceReference e))              = maybeAgent e
    sourceAgents (SourcedAttr _ (SourceImpliedReference e))       = maybeAgent e
    sourceAgents (SourcedAttr _ (SourceUnsightedReference e refs)) = maybeAgent e ++ concatMap maybeAgent refs
    sourceAgents (SourcedAttr _ (SourceEditorial refs))            = concatMap maybeAgent refs
    sourceAgents (SourcedAttr _ (SourceApproximation _ refs))      = concatMap maybeAgent refs
    sourceAgents (DerivedAttr _ _ e)                               = maybeAgent e

    maybeAgent :: Entity -> [Agent]
    maybeAgent e = maybe [] (:[]) (entityAgent e)

    hasTranslations :: Bool
    hasTranslations = any ((/= Nothing) . entityTranslated) allEntities

    -- Entities that are the via-entity in a DerivedAttr (trigger color-sample activities)
    derivedEntities :: [Entity]
    derivedEntities = nub [e | DerivedAttr _ _ e <- sources]

    -- -------------------------------------------------------------------------
    -- ID helpers
    -- -------------------------------------------------------------------------

    entityIdStr :: Entity -> String
    entityIdStr = escId . entityTitle

    screenshotIdStr :: Entity -> String
    screenshotIdStr e = "screenshot_" ++ entityIdStr e

    attrId :: String -> String
    attrId name = isoLower ++ "_" ++ escId name

    viewActivityId :: Entity -> String
    viewActivityId e = cf ("view_" ++ entityIdStr e)

    translateActivityId :: Entity -> String
    translateActivityId e = cf ("translate_" ++ entityIdStr e)

    colorSampleActivityId :: Entity -> String
    colorSampleActivityId e = cf ("color-sample-" ++ escId (entityTitle e))

    -- -------------------------------------------------------------------------
    -- Agents
    -- -------------------------------------------------------------------------

    agentEntries :: [(String, Value)]
    agentEntries =
      [ (cf "editor", object
          [ "prov:label" .= ("Xavier Shay" :: String)
          , "prov:type"  .= qname "prov:Person" ]) ] ++
      [ (cf "google_translate", object
          [ "prov:label" .= ("Google Translate" :: String)
          , "prov:type"  .= qname "prov:SoftwareAgent" ])
        | hasTranslations ] ++
      [ (cf (agentId a), object
          [ "prov:label" .= agentName a
          , "prov:type"  .= qname "prov:Organization" ])
        | a <- allAgents ]

    -- -------------------------------------------------------------------------
    -- Entities
    -- -------------------------------------------------------------------------

    flagEntityId :: String
    flagEntityId = cf (isoLower ++ "_flag")

    constructionId :: String
    constructionId = cf (isoLower ++ "_construction")

    -- Build entity entries for a source document (+ screenshot + translation)
    buildSourceEntityEntries :: Entity -> [(String, Value)]
    buildSourceEntityEntries e =
      [ (cf (entityIdStr e), object $
          [ "prov:label" .= entityTitle e
          , "prov:type"  .= qname "cf:SourceDocument" ] ++
          [ "prov:location" .= entityUrl e | not (null (entityUrl e)) ]) ] ++
      -- Screenshot entity (if present)
      maybe [] (\(_, path) ->
        [ (cf (screenshotIdStr e), object
            [ "prov:label"    .= ("Screenshot of " ++ entityTitle e)
            , "prov:type"     .= qname "cf:Screenshot"
            , "prov:location" .= path ]) ])
        (entityScreenshot e)

    allEntityEntries :: [(String, Value)]
    allEntityEntries =
      [ (flagEntityId, object
          [ "prov:label" .= ("Flag of " ++ flagName')
          , "prov:type"  .= qname "cf:Flag" ]) ] ++
      concatMap buildSourceEntityEntries allEntities ++
      -- Attribute entities (one per sourced or derived attribute; nub on name to
      -- avoid duplicates when the same label appears more than once, e.g. BGD's
      -- two "Pantone Colors (RGB)" conversions sharing a parent label)
      [ (cf (attrId name), object
          [ "prov:label" .= name
          , "prov:type"  .= qname "cf:Attribute" ])
        | name <- nub [ n | se <- sources
                          , let n = case se of
                                  SourcedAttr n' _ -> n'
                                  DerivedAttr n' _ _ -> n' ] ]

    -- -------------------------------------------------------------------------
    -- Activities
    -- -------------------------------------------------------------------------

    allActivityEntries :: [(String, Value)]
    allActivityEntries =
      [ (constructionId, object
          [ "prov:label" .= ("Construction" :: String)
          , "prov:type"  .= qname "cf:Construction" ]) ] ++
      -- View activities (one per entity that has a screenshot)
      [ (viewActivityId e, object
          [ "prov:label"     .= ("View" :: String)
          , "prov:type"      .= qname "cf:View"
          , "prov:startTime" .= (date ++ "T00:00:00")
          , "prov:endTime"   .= (date ++ "T00:00:00") ])
        | e <- allEntities, Just (date, _) <- [entityScreenshot e] ] ++
      -- Translate activities (one per entity that has a translation)
      [ (translateActivityId e, object
          [ "prov:label"     .= ("Translate" :: String)
          , "prov:type"      .= qname "cf:Translation"
          , "prov:startTime" .= (date ++ "T00:00:00")
          , "prov:endTime"   .= (date ++ "T00:00:00") ])
        | e <- allEntities, Just date <- [entityTranslated e] ] ++
      -- Color-sample activities (one per unique derived entity)
      [ (colorSampleActivityId e, object
          [ "prov:label" .= ("Sample" :: String)
          , "prov:type"  .= qname "cf:color-sample" ])
        | e <- derivedEntities ]

    -- -------------------------------------------------------------------------
    -- Relations
    -- -------------------------------------------------------------------------

    -- wasGeneratedBy
    wgbRels :: [Value]
    wgbRels =
      -- Flag wasGeneratedBy construction
      [ object ["prov:entity" .= flagEntityId, "prov:activity" .= constructionId] ] ++
      -- Screenshot wasGeneratedBy view
      [ object ["prov:entity" .= cf (screenshotIdStr e), "prov:activity" .= viewActivityId e]
        | e <- allEntities, Just _ <- [entityScreenshot e] ] ++
      -- SourcedAttr wasGeneratedBy translate (translate activity bridges source to attribute)
      [ object ["prov:entity" .= cf (attrId name), "prov:activity" .= translateActivityId e]
        | SourcedAttr name src <- sources
        , Just e <- [primaryEntity src]
        , Just _ <- [entityTranslated e] ] ++
      -- DerivedAttr wasGeneratedBy color-sample
      [ object ["prov:entity" .= cf (attrId name), "prov:activity" .= colorSampleActivityId e]
        | DerivedAttr name _ e <- sources ]

    -- SourcedAttr names that are consumed by a DerivedAttr or approximated by a
    -- SourceApproximation (i.e. intermediates, not directly used by the construction).
    intermediateNames :: [String]
    intermediateNames =
      [from    | DerivedAttr _ from _ <- sources] ++
      [approxOf | SourcedAttr _ (SourceApproximation approxOf _) <- sources]

    -- used
    usedRels :: [Value]
    usedRels =
      -- Construction used directly-sourced attrs that aren't consumed by a DerivedAttr ...
      [ object ["prov:activity" .= constructionId, "prov:entity" .= cf (attrId name)]
        | SourcedAttr name _ <- sources, name `notElem` intermediateNames ] ++
      -- ... plus derived attrs (the terminal converted values, e.g. "Green Pantone (RGB)")
      [ object ["prov:activity" .= constructionId, "prov:entity" .= cf (attrId name)]
        | DerivedAttr name _ _ <- sources ] ++
      -- View used source entity
      [ object ["prov:activity" .= viewActivityId e, "prov:entity" .= cf (entityIdStr e)]
        | e <- allEntities, Just _ <- [entityScreenshot e] ] ++
      -- Translate used source entity
      [ object ["prov:activity" .= translateActivityId e
               ,"prov:entity"   .= cf (entityIdStr e)]
        | e <- allEntities, Just _ <- [entityTranslated e] ] ++
      -- Color-sample used its via-entity
      [ object ["prov:activity" .= colorSampleActivityId e, "prov:entity" .= cf (entityIdStr e)]
        | e <- derivedEntities ]

    -- wasDerivedFrom
    wdfRels :: [Value]
    wdfRels =
      -- Screenshot derived from source entity
      [ object ["prov:generatedEntity" .= cf (screenshotIdStr e)
               ,"prov:usedEntity"      .= cf (entityIdStr e)]
        | e <- allEntities, Just _ <- [entityScreenshot e] ] ++
      -- Attribute derived from source (for reference and unsighted)
      concatMap attrWasDerivedFrom sources

    attrWasDerivedFrom :: SourcedElement -> [Value]
    attrWasDerivedFrom (SourcedAttr name (SourceReference e)) =
      [ object ["prov:generatedEntity" .= cf (attrId name)
               ,"prov:usedEntity"      .= cf (entityIdStr e)] ]
    attrWasDerivedFrom (SourcedAttr name (SourceUnsightedReference e _)) =
      [ object ["prov:generatedEntity" .= cf (attrId name)
               ,"prov:usedEntity"      .= cf (entityIdStr e)] ]
    -- DerivedAttr: attr-to-attr link (e.g. "RGB Conversion" wasDerivedFrom "Green Pantone")
    attrWasDerivedFrom (DerivedAttr name from _) =
      [ object ["prov:generatedEntity" .= cf (attrId name)
               ,"prov:usedEntity"      .= cf (attrId from)] ]
    attrWasDerivedFrom _ = []

    -- wasAttributedTo
    watRels :: [Value]
    watRels =
      -- Source entity attributed to its agent
      [ object ["prov:entity" .= cf (entityIdStr e), "prov:agent" .= cf (agentId a)]
        | e <- allEntities, Just a <- [entityAgent e] ] ++
      -- Editorial and approximation attributes attributed to editor
      [ object ["prov:entity" .= cf (attrId name), "prov:agent" .= cf "editor"]
        | SourcedAttr name src <- sources
        , case src of { SourceEditorial _ -> True; SourceApproximation _ _ -> True; _ -> False } ]

    -- wasAssociatedWith
    wawRels :: [Value]
    wawRels =
      -- Construction associated with editor
      [ object ["prov:activity" .= constructionId, "prov:agent" .= cf "editor"] ] ++
      -- View associated with editor
      [ object ["prov:activity" .= viewActivityId e, "prov:agent" .= cf "editor"]
        | e <- allEntities, Just _ <- [entityScreenshot e] ] ++
      -- Translate associated with editor and google_translate
      concatMap (\e ->
        [ object ["prov:activity" .= translateActivityId e, "prov:agent" .= cf "editor"]
        , object ["prov:activity" .= translateActivityId e, "prov:agent" .= cf "google_translate"] ])
        [e | e <- allEntities, Just _ <- [entityTranslated e]] ++
      -- Color-sample associated with editor
      [ object ["prov:activity" .= colorSampleActivityId e, "prov:agent" .= cf "editor"]
        | e <- derivedEntities ]

    -- wasInfluencedBy
    wibRels :: [Value]
    wibRels = concatMap attrWasInfluencedBy sources

    attrWasInfluencedBy :: SourcedElement -> [Value]
    attrWasInfluencedBy (SourcedAttr name (SourceImpliedReference e)) =
      [ object ["prov:influencee" .= cf (attrId name)
               ,"prov:influencer" .= cf (entityIdStr e)] ]
    attrWasInfluencedBy (SourcedAttr name (SourceUnsightedReference _ refs)) =
      [ object ["prov:influencee" .= cf (attrId name)
               ,"prov:influencer" .= cf (entityIdStr e)]
        | e <- refs ]
    attrWasInfluencedBy (SourcedAttr name (SourceEditorial refs)) =
      [ object ["prov:influencee" .= cf (attrId name)
               ,"prov:influencer" .= cf (entityIdStr e)]
        | e <- refs ]
    attrWasInfluencedBy (SourcedAttr name (SourceApproximation approxOf refs)) =
      -- attr-to-attr: this attribute was chosen to approximate a previously-sourced attribute
      [ object ["prov:influencee" .= cf (attrId name)
               ,"prov:influencer" .= cf (attrId approxOf)] ] ++
      -- attr-to-docs: supporting references consulted when making the choice
      [ object ["prov:influencee" .= cf (attrId name)
               ,"prov:influencer" .= cf (entityIdStr e)]
        | e <- refs ]
    attrWasInfluencedBy _ = []

    -- -------------------------------------------------------------------------
    -- Build the PROV-JSON document
    -- -------------------------------------------------------------------------

    doc :: Value
    doc = object
      [ "prefix"            .= object
          [ "cf"   .= ("http://constructibleflags.com/ns#" :: String)
          , "prov" .= ("http://www.w3.org/ns/prov#" :: String)
          , "xsd"  .= ("http://www.w3.org/2001/XMLSchema#" :: String) ]
      , "entity"            .= makeMap allEntityEntries
      , "activity"          .= makeMap allActivityEntries
      , "agent"             .= makeMap agentEntries
      , "wasGeneratedBy"    .= makeMap (numberedRels "wgb" wgbRels)
      , "used"              .= makeMap (numberedRels "u"   usedRels)
      , "wasDerivedFrom"    .= makeMap (numberedRels "wdf" wdfRels)
      , "wasAttributedTo"   .= makeMap (numberedRels "wat" watRels)
      , "wasAssociatedWith" .= makeMap (numberedRels "waw" wawRels)
      , "wasInfluencedBy"   .= makeMap (numberedRels "wib" wibRels)
      ]

    escId :: String -> String
    escId = map (\c -> if c `elem` validIdChars then c else '_')
      where validIdChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-."
