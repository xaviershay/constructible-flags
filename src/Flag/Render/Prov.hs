{-# LANGUAGE OverloadedStrings #-}
module Flag.Render.Prov
    ( generateProvJson
    ) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key (fromString)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)
import Data.List (nub, isPrefixOf)

import Flag.Source (Source(..), Entity(..), Agent(..), SourcedElement)

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
    allEntities = nub [e | (_, src) <- sources, e <- sourceEntities src]

    sourceEntities :: Source -> [Entity]
    sourceEntities (SourceReference e)              = [e]
    sourceEntities (SourceImpliedReference e)       = [e]
    sourceEntities (SourceUnsightedReference e refs) = e : refs
    sourceEntities (SourceEditorial refs)            = refs

    -- The primary source entity for an attribute (Nothing for editorial)
    primaryEntity :: Source -> Maybe Entity
    primaryEntity (SourceReference e)           = Just e
    primaryEntity (SourceImpliedReference e)    = Just e
    primaryEntity (SourceUnsightedReference e _) = Just e
    primaryEntity (SourceEditorial _)            = Nothing

    allAgents :: [Agent]
    allAgents = nub [a | (_, src) <- sources, a <- sourceAgents src]

    sourceAgents :: Source -> [Agent]
    sourceAgents (SourceReference e)              = maybeAgent e
    sourceAgents (SourceImpliedReference e)       = maybeAgent e
    sourceAgents (SourceUnsightedReference e refs) = maybeAgent e ++ concatMap maybeAgent refs
    sourceAgents (SourceEditorial refs)            = concatMap maybeAgent refs

    maybeAgent :: Entity -> [Agent]
    maybeAgent e = maybe [] (:[]) (entityAgent e)

    hasTranslations :: Bool
    hasTranslations = any ((/= Nothing) . entityTranslated) allEntities

    -- Pantone source entities (used for color-sample activities)
    pantoneEntities :: [Entity]
    pantoneEntities = nub
      [e | (_, SourceReference e) <- sources, "PMS" `isPrefixOf` entityTitle e]

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
          [ "prov:label" .= flagName'
          , "prov:type"  .= qname "cf:Flag" ]) ] ++
      concatMap buildSourceEntityEntries allEntities ++
      -- Attribute entities (derived from source entities)
      [ (cf (attrId name), object
          [ "prov:label" .= name
          , "prov:type"  .= qname "cf:Attribute" ])
        | (name, _) <- sources ]

    -- -------------------------------------------------------------------------
    -- Activities
    -- -------------------------------------------------------------------------

    allActivityEntries :: [(String, Value)]
    allActivityEntries =
      [ (constructionId, object
          [ "prov:label" .= ("Construction of " ++ flagName' ++ " flag")
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
      -- Color-sample activities (one per unique pantone entity)
      [ (colorSampleActivityId e, object
          [ "prov:label" .= ("color-sample " ++ entityTitle e)
          , "prov:type"  .= qname "cf:color-sample" ])
        | e <- pantoneEntities ]

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
      -- Attribute wasGeneratedBy translate (translate activity bridges source to attribute)
      [ object ["prov:entity" .= cf (attrId name), "prov:activity" .= translateActivityId e]
        | (name, src) <- sources
        , Just e <- [primaryEntity src]
        , Just _ <- [entityTranslated e] ] ++
      -- Pantone attribute wasGeneratedBy color-sample
      [ object ["prov:entity" .= cf (attrId name), "prov:activity" .= colorSampleActivityId e]
        | (name, SourceReference e) <- sources, "PMS" `isPrefixOf` entityTitle e ]

    -- used
    usedRels :: [Value]
    usedRels =
      -- Construction used each attribute entity
      [ object ["prov:activity" .= constructionId, "prov:entity" .= cf (attrId name)]
        | (name, _) <- sources ] ++
      -- View used source entity
      [ object ["prov:activity" .= viewActivityId e, "prov:entity" .= cf (entityIdStr e)]
        | e <- allEntities, Just _ <- [entityScreenshot e] ] ++
      -- Translate used source entity
      [ object ["prov:activity" .= translateActivityId e
               ,"prov:entity"   .= cf (entityIdStr e)]
        | e <- allEntities, Just _ <- [entityTranslated e] ] ++
      -- Color-sample used pantone entity
      [ object ["prov:activity" .= colorSampleActivityId e, "prov:entity" .= cf (entityIdStr e)]
        | e <- pantoneEntities ]

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
    attrWasDerivedFrom (name, SourceReference e) =
      [ object ["prov:generatedEntity" .= cf (attrId name)
               ,"prov:usedEntity"      .= cf (entityIdStr e)] ]
    attrWasDerivedFrom (name, SourceUnsightedReference e _) =
      [ object ["prov:generatedEntity" .= cf (attrId name)
               ,"prov:usedEntity"      .= cf (entityIdStr e)] ]
    attrWasDerivedFrom _ = []

    -- wasAttributedTo
    watRels :: [Value]
    watRels =
      -- Source entity attributed to its agent
      [ object ["prov:entity" .= cf (entityIdStr e), "prov:agent" .= cf (agentId a)]
        | e <- allEntities, Just a <- [entityAgent e] ] ++
      -- Editorial attribute attributed to editor
      [ object ["prov:entity" .= cf (attrId name), "prov:agent" .= cf "editor"]
        | (name, SourceEditorial _) <- sources ]

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
        | e <- pantoneEntities ]

    -- wasInfluencedBy
    wibRels :: [Value]
    wibRels = concatMap attrWasInfluencedBy sources

    attrWasInfluencedBy :: SourcedElement -> [Value]
    attrWasInfluencedBy (name, SourceImpliedReference e) =
      [ object ["prov:influencee" .= cf (attrId name)
               ,"prov:influencer" .= cf (entityIdStr e)] ]
    attrWasInfluencedBy (name, SourceUnsightedReference _ refs) =
      [ object ["prov:influencee" .= cf (attrId name)
               ,"prov:influencer" .= cf (entityIdStr e)]
        | e <- refs ]
    attrWasInfluencedBy (name, SourceEditorial refs) =
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
