{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Render.Html
    ( -- * Index page
      generateIndex
      -- * Show page
    , generateShowPage
      -- * Utilities
    , escapeHtml
    ) where

import Data.Char (toLower)
import Data.List (nub, sortOn, groupBy, intercalate, partition)
import Data.Function (on)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (docTypeHtml, (!), toValue, toHtml, preEscapedToHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import Flag.Source (Source(..), Entity(..), Agent(..), SourcedElement, elementDisplayPair)
import Flag.Construction.Interpreter (Step(..))

-- ---------------------------------------------------------------------------
-- Main index page
-- ---------------------------------------------------------------------------

-- | Generate the index.html content
generateIndex :: [(String, String, String, String, [SourcedElement], [Step], String)] -> String
generateIndex flags = renderHtml $ docTypeHtml $ H.html $ do
  H.head $ do
    H.meta ! A.charset (toValue "UTF-8")
    H.meta ! A.name (toValue "viewport") ! A.content (toValue "width=device-width, initial-scale=1.0")
    H.title $ toHtml "Constructible Flags"
    -- External CSS and KaTeX init
    H.link ! A.rel (toValue "stylesheet") ! A.href (toValue "/static/main.css")
    H.link ! A.rel (toValue "stylesheet") ! A.href (toValue "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css")
    H.script ! A.defer (toValue "") ! A.src (toValue "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js") $ mempty
    H.script ! A.defer (toValue "") ! A.src (toValue "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js") $ mempty
    H.script ! A.defer (toValue "") ! A.src (toValue "/static/katex-init.js") $ mempty
  H.body $ do
    H.h1 $ toHtml "Constructible Flags"
    H.table $ do
      H.thead $ H.tr $ do
        H.th $ toHtml "Design"
        H.th $ toHtml "Name"
        H.th $ toHtml "Construction"
        H.th $ toHtml "Sources"
      H.tbody $ mapM_ flagRow flags
  where
    flagRow (svgFile, name, _desc, isoCode, sources, constructionSteps, field) = H.tr $ do
      H.td $ H.a ! A.href (toValue svgFile) $ H.img ! A.src (toValue svgFile) ! A.alt (toValue $ escapeHtml name ++ " flag")
      H.td $ H.a ! A.href (toValue $ map toLower isoCode ++ ".html") $ H.toHtml (escapeHtml name)
      H.td $ do
        H.div ! A.style (toValue "text-align:center") $ H.a ! A.href (toValue $ "debug-v2/?flag=" ++ map toLower isoCode) $ H.toHtml $ show (length constructionSteps) ++ " cost"
        H.preEscapedToHtml $ formatSteps constructionSteps
        -- H.preEscapedToHtml $ "<div style=\"text-align:center\">$" ++ field ++"$</div>"
      H.td $ do
        H.preEscapedToHtml $ formatSources sources
        H.preEscapedToHtml $ "<div style=\"margin-top:8px;font-size:0.85em\"><a href=\"" ++ map toLower isoCode ++ "-prov.json\">[PROV]</a></div>"

-- ---------------------------------------------------------------------------
-- Show page
-- ---------------------------------------------------------------------------

-- | Generate a show page for a single flag
-- Now includes editorNote as last field
-- (svgFile, name, desc, isoCode, sources, constructionSteps, field, editorNote)
generateShowPage :: (String, String, String, String, [SourcedElement], [Step], String, String) -> String
generateShowPage (svgFile, name, desc, isoCode, sources, constructionSteps, field, editorNote) =
  let isoLower = map toLower isoCode
  in renderHtml $ docTypeHtml $ H.html $ do
    H.head $ do
      H.meta ! A.charset (toValue "UTF-8")
      H.meta ! A.name (toValue "viewport") ! A.content (toValue "width=device-width, initial-scale=1.0")
      H.title $ H.toHtml $ escapeHtml name ++ " - Constructible Flags"
      H.link ! A.rel (toValue "stylesheet") ! A.href (toValue "/static/main.css")
      H.link ! A.rel (toValue "stylesheet") ! A.href (toValue "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css")
      H.script ! A.defer (toValue "") ! A.src (toValue "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js") $ mempty
      H.script ! A.defer (toValue "") ! A.src (toValue "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js") $ mempty
      H.script ! A.defer (toValue "") ! A.src (toValue "/static/katex-init.js") $ mempty
      H.script ! A.src (toValue "https://d3js.org/d3.v7.min.js") $ mempty
      H.script ! A.src (toValue "/static/prov.js") $ mempty
    H.body $ do
      H.p $ H.a ! A.href (toValue "index.html") $ H.toHtml "← All flags"
      H.div ! A.class_ (toValue "flag-header") $ do
        H.a ! A.href (toValue svgFile) $ H.img ! A.src (toValue svgFile) ! A.alt (toValue $ escapeHtml name)
        H.div ! A.class_ (toValue "flag-info") $ do
          H.h1 $ H.toHtml $ escapeHtml name
          H.div ! A.class_ (toValue "description") $ H.toHtml $ escapeHtml desc
          if not (null editorNote) then
            H.div ! A.class_ (toValue "editor-note") $ do
              H.i $ H.toHtml "Editor's note: "
              H.toHtml $ escapeHtml editorNote
          else mempty
      H.h2 $ toHtml "Construction"
      H.div ! A.class_ (toValue "construction") $ do
        H.div $ do
          H.a ! A.href (toValue $ "debug-v2/?flag=" ++ isoLower) $ H.toHtml "Interactive viewer"
          H.toHtml $ " — " ++ show (length constructionSteps) ++ " cost"
        H.preEscapedToHtml $ formatSteps constructionSteps
      H.h2 $ toHtml "Sources"
      H.preEscapedToHtml $ formatSourceCards sources
      H.h2 $ toHtml "Provenance"
      H.div ! A.style (toValue "overflow:hidden;border:1px solid #ddd;background:#fafafa;cursor:grab") $ H.preEscapedToHtml "<svg id=\"prov-hier\" width=\"880\" height=\"500\"></svg>"
      H.script $ H.preEscapedToHtml $ "initProv(\"" ++ isoLower ++ "-prov.json\");"

-- ---------------------------------------------------------------------------
-- Source cards (grouped sources with inline screenshots)
-- ---------------------------------------------------------------------------

-- | Render sources as cards, each with its screenshot (if any) displayed inline.
-- Sources are first grouped by exact Source match (merging elements that share
-- a source), then groups with the same element names are merged (so e.g.
-- multiple Pantone chips for "RGB Conversion" appear in one card).
formatSourceCards :: [SourcedElement] -> String
formatSourceCards [] = "<em>None</em>"
formatSourceCards elems =
  let pairs = map elementDisplayPair elems
      -- Pass 1: group elements by identical Source
      bySource = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) pairs
      -- Each group becomes (elementNames, [sources])
      sourceGroups = [ (nub $ map fst grp, [snd (head grp)])
                     | grp <- bySource ]
      -- Pass 2: merge groups that share the same element names
      merged = mergeByNames $ sortOn fst sourceGroups
  in "<div class=\"source-cards\">" ++ concatMap formatMergedCard merged ++ "</div>"

-- | Merge adjacent groups that share the same element name list.
mergeByNames :: [([String], [Source])] -> [([String], [Source])]
mergeByNames [] = []
mergeByNames [x] = [x]
mergeByNames ((n1, s1):(n2, s2):rest)
  | n1 == n2  = mergeByNames ((n1, s1 ++ s2) : rest)
  | otherwise = (n1, s1) : mergeByNames ((n2, s2) : rest)

-- | Render a merged card: may have multiple sources.
formatMergedCard :: ([String], [Source]) -> String
formatMergedCard (names, srcs) =
  let elementsStr = escapeHtml (joinElements names)
      headings = intercalate ", " $ map formatSourceHeading srcs
      screenshotEntities = [ e | e <- nub (concatMap screenshotableEntities srcs)
                               , entityScreenshot e /= Nothing ]
      isPantone e = case entityAgent e of
        Just a  -> agentId a == "pantone"
        Nothing -> False
      (pantoneEnts, regularEnts) = partition isPantone screenshotEntities
      screenshotsHtml = concatMap renderScreenshot regularEnts
                     ++ renderPantoneChips pantoneEnts
  in "<div class=\"source-card\">"
     ++ "<div class=\"source-info\">"
     ++ headings
     ++ " <span class=\"elements\">(" ++ elementsStr ++ ")</span>"
     ++ "</div>"
     ++ screenshotsHtml
     ++ "</div>"

-- | Render the source heading (link + type annotation).
formatSourceHeading :: Source -> String
formatSourceHeading (SourceReference e) = formatEntityLink e
formatSourceHeading (SourceImpliedReference e) = formatEntityLink e ++ " (implied)"
formatSourceHeading (SourceUnsightedReference e _) = formatEntityLink e ++ " (unsighted)"
formatSourceHeading (SourceEditorial _) = "Editorial decision"
formatSourceHeading (SourceApproximation approxOf _) = "Approximation of <em>" ++ escapeHtml approxOf ++ "</em>"

-- | Render a regular (non-Pantone) screenshot as a block-level image.
renderScreenshot :: Entity -> String
renderScreenshot entity =
  case entityScreenshot entity of
    Just (_, path) ->
      "<div class=\"source-screenshot\">"
      ++ "<img src=\"/images/" ++ escapeHtml path ++ "\">"
      ++ "</div>"
    Nothing -> ""

-- | Render Pantone chip images grouped together in a row.
renderPantoneChips :: [Entity] -> String
renderPantoneChips [] = ""
renderPantoneChips chips =
  "<div class=\"pantone-chips\">"
  ++ concatMap renderChip chips
  ++ "</div>"
  where
    renderChip entity = case entityScreenshot entity of
      Just (_, path) ->
        "<div class=\"pantone-chip\">"
        ++ "<img src=\"/images/" ++ escapeHtml path ++ "\">"
        ++ "</div>"
      Nothing -> ""

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

formatSteps :: [Step] -> String
formatSteps [] = "<em>None</em>"
formatSteps ss =
  let llCount = length [() | StepIntersectLL <- ss]
      lcCount = length [() | StepIntersectLC <- ss]
      ccCount = length [() | StepIntersectCC <- ss]
      ftCount = length [() | StepFillTriangle <- ss]
      fcCount = length [() | StepFillCircle <- ss]
      ngonCount = length [() | StepNGonVertex <- ss]
      rows = concat
        [ if llCount > 0 then ["\\text{\9472}\\!\\cap\\!\\text{\9472} &\\times " ++ show llCount] else []
        , if lcCount > 0 then ["\\text{\9472}\\!\\cap\\!\\bigcirc &\\times " ++ show lcCount] else []
        , if ccCount > 0 then ["\\bigcirc\\!\\cap\\!\\bigcirc &\\times " ++ show ccCount] else []
        , if ftCount > 0 then ["\\blacktriangle &\\times " ++ show ftCount] else []
        , if fcCount > 0 then ["\\bullet &\\times " ++ show fcCount] else []
        , if ngonCount > 0 then ["\\star &\\times " ++ show ngonCount] else []
        ]
  in if null rows
     then "<em>None</em>"
     else "<span style=\"font-size:75%\">$$\\begin{aligned}" ++ intercalate "\\\\" rows ++ "\\end{aligned}$$</span>"

formatSources :: [SourcedElement] -> String
formatSources [] = "<em>None</em>"
formatSources elems =
  let pairs = map elementDisplayPair elems
      grouped = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) pairs
  in "<ul>" ++ concatMap formatSourceGroup grouped ++ "</ul>"

sourceKey :: Source -> String
sourceKey (SourceReference e) = "1" ++ entityTitle e
sourceKey (SourceImpliedReference e) = "2" ++ entityTitle e
sourceKey (SourceUnsightedReference e _) = "3" ++ entityTitle e
sourceKey (SourceEditorial _) = "4"
sourceKey (SourceApproximation approxOf _) = "5" ++ approxOf

formatSourceGroup :: [(String, Source)] -> String
formatSourceGroup [] = ""
formatSourceGroup grp@((_, src):_) =
  let elementNames = map fst grp
      elementsStr = "<span class=\"elements\">(" ++ escapeHtml (joinElements elementNames) ++ ")</span>"
  in formatSourceWithElements src elementsStr

joinElements :: [String] -> String
joinElements xs = intercalate ", " xs

formatSourceWithElements :: Source -> String -> String
formatSourceWithElements (SourceReference e) elems =
  "<li>" ++ formatEntityLink e ++ " " ++ elems ++ "</li>"
formatSourceWithElements (SourceImpliedReference e) elems =
  "<li>" ++ formatEntityLink e ++ " (implied) " ++ elems ++ "</li>"
formatSourceWithElements (SourceUnsightedReference e _) elems =
  "<li>" ++ formatEntityLink e ++ " (unsighted) " ++ elems ++ "</li>"
formatSourceWithElements (SourceEditorial _) elems =
  "<li>Editorial decision " ++ elems ++ "</li>"
formatSourceWithElements (SourceApproximation approxOf _) elems =
  "<li>Approximation of <em>" ++ escapeHtml approxOf ++ "</em> " ++ elems ++ "</li>"

formatEntityLink :: Entity -> String
formatEntityLink e
  | null (entityUrl e) = escapeHtml (entityTitle e)
  | otherwise = "<a href=\"" ++ escapeHtml (entityUrl e) ++ "\">" ++ escapeHtml (entityTitle e) ++ "</a>"

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
