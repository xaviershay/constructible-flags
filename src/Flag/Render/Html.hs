{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Flag.Render.Html
    ( -- * Index page
      generateIndex
      -- * Show page
    , generateShowPage
      -- * Utilities
    , escapeHtml
      -- * Testing helpers
    , formatSteps
    ) where

import Data.Char (toLower)
import Data.List (nub, sortOn, groupBy, intercalate, intersperse, partition)
import Data.Function (on)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (docTypeHtml, (!), toValue, toHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import Flag.Source (Source(..), Entity(..), Agent(..), SourcedElement, elementDisplayPair)
import Flag.Construction.Interpreter (Step(..))

-- ---------------------------------------------------------------------------
-- Main index page
-- ---------------------------------------------------------------------------

-- | Generate the index.html content
-- Tuple: (svgFile, name, desc, isoCode, updatedAt, sources, constructionSteps, field)
generateIndex :: [(String, String, String, String, String, [SourcedElement], [Step], String)] -> String
generateIndex flags = renderHtml $ docTypeHtml $ H.html $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.title "Constructible Flags"
    H.link ! A.rel "stylesheet" ! A.href "/static/main.css"
  H.body $ do
    H.header ! A.class_ "site-header" $ do
      H.h1 "Constructible Flags"
      H.p ! A.class_ "site-description" $ """
        High quality SVG flag productions from official specifications.  Geometric
        features are (as much as possible) exactly programmatically generated.
        Full provenance information and editorial notes are provided for every
        construction.  A byproduct of this method is insight into the minimum
        capabilities required to construct (e.g. straightedge and compass) and
        relative "cost".
        """
      H.h3 "Principles"
      H.ul ! A.class_ "principles" $ do
        H.li "Prefer government and official sources."
        H.li "Constructions aim to map to specifications rather than be minimal."
        H.li $ do
          "Use "
          H.a ! A.href "https://www.constituteproject.org/" $ "Constitute Project"
          " when referencing constitutions, for consistency of presentation."
        H.li "Use Pantone\8217s own RGB approximations if not otherwise provided."
    H.div ! A.class_ "flag-grid" $ mapM_ flagCard flags
    H.footer ! A.class_ "site-footer" $
      H.p $ do
        H.a ! A.href "https://github.com/xaviershay/constructible-flags" $ "GitHub"
        " · "
        H.a ! A.href "https://www.crwflags.com/fotw/flags/" $ "Flags of the World"
        " · Made by "
        H.a ! A.href "https://blog.xaviershay.com" $ "Xavier Shay"
  where
    flagCard (svgFile, name, _desc, isoCode, updatedAt, _sources, constructionSteps, _field) =
      let isoLower = map toLower isoCode
          flagPage = isoLower ++ ".html"
          isConstructible = null [() | StepNGonVertex <- constructionSteps]
          hasSVGOverlay   = not $ null [() | StepSVGOverlay <- constructionSteps]
      in H.a ! A.class_ "flag-card" ! A.href (toValue flagPage) $ do
           H.img ! A.src (toValue svgFile) ! A.alt (toValue name)
           H.div ! A.class_ "flag-card-body" $ do
             H.div ! A.class_ "flag-name" $ toHtml name
             H.div ! A.class_ "flag-meta" $ do
               H.span ! A.class_ "flag-date" $ toHtml updatedAt
               H.span ! A.class_ "flag-icons" $ do
                 if isConstructible
                   then H.span ! A.class_ "icon icon-sc" ! A.title "Constructible with straight-edge and compass" $ "\9711"
                   else H.span ! A.class_ "icon icon-ngon" ! A.title "Requires non-constructible n-gon vertex" $ "\9733"
                 if hasSVGOverlay
                   then H.span ! A.class_ "icon icon-svg" ! A.title "Requires arbitrary SVG overlay" $ "+"
                   else mempty

-- ---------------------------------------------------------------------------
-- Show page
-- ---------------------------------------------------------------------------

-- | Generate a show page for a single flag
-- (svgFile, name, desc, isoCode, updatedAt, sources, constructionSteps, field, editorNote)
generateShowPage :: (String, String, String, String, String, [SourcedElement], [Step], String, String) -> String
generateShowPage (svgFile, name, desc, isoCode, _updatedAt, sources, constructionSteps, _field, editorNote) =
  let isoLower = map toLower isoCode
  in renderHtml $ docTypeHtml $ H.html $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
      H.title $ toHtml name <> " - Constructible Flags"
      H.link ! A.rel "stylesheet" ! A.href "/static/main.css"
      H.link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css"
      H.script ! A.defer "" ! A.src "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js" $ mempty
      H.script ! A.defer "" ! A.src "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js" $ mempty
      H.script ! A.defer "" ! A.src "/static/katex-init.js" $ mempty
      H.script ! A.src "https://d3js.org/d3.v7.min.js" $ mempty
      H.script ! A.src "/static/prov.js" $ mempty
    H.body $ do
      H.p $ H.a ! A.href "index.html" $ "← All flags"
      H.div ! A.class_ "flag-header" $ do
        H.a ! A.href (toValue svgFile) $ H.img ! A.src (toValue svgFile) ! A.alt (toValue name)
        H.div ! A.class_ "flag-info" $ do
          H.h1 $ toHtml name
          H.div ! A.class_ "description" $ toHtml desc
          if not (null editorNote) then
            H.div ! A.class_ "editor-note" $ do
              H.i "Editor's note: "
              toHtml editorNote
          else mempty
      H.h2 "Construction"
      H.div ! A.class_ "construction" $ do
        H.div $ do
          H.a ! A.href (toValue $ "debug-v2/?flag=" ++ isoLower) $ "Interactive viewer"
          toHtml $ " — " ++ show (length constructionSteps) ++ " cost"
        formatSteps constructionSteps
      H.h2 "Sources"
      formatSourceCards sources
      H.h2 "Provenance"
      H.div ! A.style "overflow:hidden;border:1px solid #ddd;background:#fafafa;cursor:grab" $
        H.preEscapedToHtml ("<svg id=\"prov-hier\" width=\"880\" height=\"500\"></svg>" :: String)
      H.script $ H.preEscapedToHtml $ "initProv(\"" ++ isoLower ++ "-prov.json\");"

-- ---------------------------------------------------------------------------
-- Source cards (grouped sources with inline screenshots)
-- ---------------------------------------------------------------------------

-- | Render sources as cards, each with its screenshot (if any) displayed inline.
-- Sources are first grouped by exact Source match (merging elements that share
-- a source), then groups with the same element names are merged (so e.g.
-- multiple Pantone chips for "RGB Conversion" appear in one card).
formatSourceCards :: [SourcedElement] -> H.Html
formatSourceCards [] = H.em "None"
formatSourceCards elems =
  let pairs = map elementDisplayPair elems
      -- Pass 1: group elements by identical Source
      bySource = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) pairs
      -- Each group becomes (elementNames, [sources])
      sourceGroups = [ (nub $ map fst grp, [snd (head grp)])
                     | grp <- bySource ]
      -- Pass 2: merge groups that share the same element names
      merged = mergeByNames $ sortOn fst sourceGroups
  in H.div ! A.class_ "source-cards" $ mapM_ formatMergedCard merged

-- | Merge adjacent groups that share the same element name list.
mergeByNames :: [([String], [Source])] -> [([String], [Source])]
mergeByNames [] = []
mergeByNames [x] = [x]
mergeByNames ((n1, s1):(n2, s2):rest)
  | n1 == n2  = mergeByNames ((n1, s1 ++ s2) : rest)
  | otherwise = (n1, s1) : mergeByNames ((n2, s2) : rest)

-- | Render a merged card: may have multiple sources.
formatMergedCard :: ([String], [Source]) -> H.Html
formatMergedCard (names, srcs) =
  let screenshotEntities = [ e | e <- nub (concatMap screenshotableEntities srcs)
                               , entityScreenshot e /= Nothing ]
      isPantone e = case entityAgent e of
        Just a  -> agentId a == "pantone"
        Nothing -> False
      (pantoneEnts, regularEnts) = partition isPantone screenshotEntities
  in H.div ! A.class_ "source-card" $ do
       H.div ! A.class_ "source-info" $ do
         mconcat $ intersperse ", " $ map formatSourceHeading srcs
         " "
         H.span ! A.class_ "elements" $ "(" <> toHtml (joinElements names) <> ")"
       mapM_ renderScreenshot regularEnts
       renderPantoneChips pantoneEnts

-- | Render the source heading (link + type annotation).
formatSourceHeading :: Source -> H.Html
formatSourceHeading (SourceReference e) = formatEntityLink e
formatSourceHeading (SourceImpliedReference e) = formatEntityLink e <> " (implied)"
formatSourceHeading (SourceUnsightedReference e _) = formatEntityLink e <> " (unsighted)"
formatSourceHeading (SourceEditorial _) = "Editorial decision"
formatSourceHeading (SourceApproximation approxOf _) = "Approximation of " <> H.em (toHtml approxOf)

-- | Render a regular (non-Pantone) screenshot as a block-level image.
renderScreenshot :: Entity -> H.Html
renderScreenshot entity =
  case entityScreenshot entity of
    Just (_, path) ->
      H.div ! A.class_ "source-screenshot" $
        H.img ! A.src (toValue $ "/images/" ++ path)
    Nothing -> mempty

-- | Render Pantone chip images grouped together in a row.
renderPantoneChips :: [Entity] -> H.Html
renderPantoneChips [] = mempty
renderPantoneChips chips =
  H.div ! A.class_ "pantone-chips" $ mapM_ renderChip chips
  where
    renderChip entity = case entityScreenshot entity of
      Just (_, path) ->
        H.div ! A.class_ "pantone-chip" $
          H.img ! A.src (toValue $ "/images/" ++ path)
      Nothing -> mempty

-- | Get all entities from a Source (flat list), for link/reference display.
sourceEntitiesFlat :: Source -> [Entity]
sourceEntitiesFlat (SourceReference e) = [e]
sourceEntitiesFlat (SourceImpliedReference e) = [e]
sourceEntitiesFlat (SourceUnsightedReference e refs) = e : refs
sourceEntitiesFlat (SourceEditorial refs) = refs
sourceEntitiesFlat (SourceApproximation _ refs) = refs

-- | Entities whose screenshots should be displayed in the sources view.
-- Screenshots are shown for directly-cited and corroborating sources, but not
-- for editorial or approximation supporting refs (which are influences, not
-- primary evidence).
screenshotableEntities :: Source -> [Entity]
screenshotableEntities (SourceReference e)               = [e]
screenshotableEntities (SourceImpliedReference e)        = [e]
screenshotableEntities (SourceUnsightedReference e refs) = e : refs
screenshotableEntities (SourceEditorial _)               = []
screenshotableEntities (SourceApproximation _ _)         = []

-- ---------------------------------------------------------------------------
-- Shared formatting helpers
-- ---------------------------------------------------------------------------

formatSteps :: [Step] -> H.Html
formatSteps [] = H.em "None"
formatSteps ss =
  let llCount = length [() | StepIntersectLL <- ss]
      lcCount = length [() | StepIntersectLC <- ss]
      ccCount = length [() | StepIntersectCC <- ss]
      ftCount = length [() | StepFillTriangle <- ss]
      fcCount = length [() | StepFillCircle <- ss]
      ngonCount = length [() | StepNGonVertex <- ss]
      svgCount = length [() | StepSVGOverlay <- ss]
      rows = concat
        [ if llCount > 0 then ["\\text{\9472}\\!\\cap\\!\\text{\9472} &\\times " ++ show llCount] else []
        , if lcCount > 0 then ["\\text{\9472}\\!\\cap\\!\\bigcirc &\\times " ++ show lcCount] else []
        , if ccCount > 0 then ["\\bigcirc\\!\\cap\\!\\bigcirc &\\times " ++ show ccCount] else []
        , if ftCount > 0 then ["\\blacktriangle &\\times " ++ show ftCount] else []
        , if fcCount > 0 then ["\\bullet &\\times " ++ show fcCount] else []
        , if ngonCount > 0 then ["\\star &\\times " ++ show ngonCount] else []
        , if svgCount > 0 then ["+ &\\times " ++ show svgCount] else []
        ]
  in if null rows
     then H.em "None"
     else H.span ! A.style "font-size:75%" $
            H.preEscapedToHtml $ "$$\\begin{aligned}" ++ intercalate "\\\\" rows ++ "\\end{aligned}$$"

formatSources :: [SourcedElement] -> H.Html
formatSources [] = H.em "None"
formatSources elems =
  let pairs = map elementDisplayPair elems
      grouped = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) pairs
  in H.ul $ mapM_ formatSourceGroup grouped

sourceKey :: Source -> String
sourceKey (SourceReference e) = "1" ++ entityTitle e
sourceKey (SourceImpliedReference e) = "2" ++ entityTitle e
sourceKey (SourceUnsightedReference e _) = "3" ++ entityTitle e
sourceKey (SourceEditorial _) = "4"
sourceKey (SourceApproximation approxOf _) = "5" ++ approxOf

formatSourceGroup :: [(String, Source)] -> H.Html
formatSourceGroup [] = mempty
formatSourceGroup grp@((_, src):_) =
  let elementNames = map fst grp
      elementsStr = H.span ! A.class_ "elements" $ "(" <> toHtml (joinElements elementNames) <> ")"
  in formatSourceWithElements src elementsStr

joinElements :: [String] -> String
joinElements xs = intercalate ", " xs

formatSourceWithElements :: Source -> H.Html -> H.Html
formatSourceWithElements (SourceReference e) elems =
  H.li $ formatEntityLink e <> " " <> elems
formatSourceWithElements (SourceImpliedReference e) elems =
  H.li $ formatEntityLink e <> " (implied) " <> elems
formatSourceWithElements (SourceUnsightedReference e _) elems =
  H.li $ formatEntityLink e <> " (unsighted) " <> elems
formatSourceWithElements (SourceEditorial _) elems =
  H.li $ "Editorial decision " <> elems
formatSourceWithElements (SourceApproximation approxOf _) elems =
  H.li $ "Approximation of " <> H.em (toHtml approxOf) <> " " <> elems

formatEntityLink :: Entity -> H.Html
formatEntityLink e
  | null (entityUrl e) = toHtml (entityTitle e)
  | otherwise = H.a ! A.href (toValue (entityUrl e)) $ toHtml (entityTitle e)

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Escape special HTML characters
escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar c   = [c]
