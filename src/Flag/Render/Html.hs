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
import Data.List (nub, sortOn, groupBy, intercalate)
import Data.Function (on)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (docTypeHtml, (!), toValue, toHtml, preEscapedToHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import Flag.Source (Source(..), Entity(..), SourcedElement)
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
        H.preEscapedToHtml $ "<div style=\"text-align:center\">$" ++ field ++"$</div>"
      H.td $ do
        H.preEscapedToHtml $ formatSources sources
        H.preEscapedToHtml $ "<div style=\"margin-top:8px;font-size:0.85em\"><a href=\"" ++ map toLower isoCode ++ "-prov.xml\">[PROV]</a></div>"

-- ---------------------------------------------------------------------------
-- Show page
-- ---------------------------------------------------------------------------

-- | Generate a show page for a single flag
generateShowPage :: (String, String, String, String, [SourcedElement], [Step], String) -> String
generateShowPage (svgFile, name, desc, isoCode, sources, constructionSteps, field) =
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
      H.h2 $ toHtml "Construction"
      H.div ! A.class_ (toValue "construction") $ do
        H.div $ do
          H.a ! A.href (toValue $ "debug-v2/?flag=" ++ isoLower) $ H.toHtml "Interactive viewer"
          H.toHtml $ " — " ++ show (length constructionSteps) ++ " cost"
        H.preEscapedToHtml $ formatSteps constructionSteps
        H.preEscapedToHtml $ "<div>$" ++ field ++ "$</div>"
      H.h2 $ toHtml "Sources"
      H.div ! A.class_ (toValue "sources") $ H.preEscapedToHtml $ formatSources sources
      H.h2 $ toHtml "Screenshots"
      H.preEscapedToHtml $ screenshotsTable sources
      H.h2 $ toHtml "Provenance"
      H.div ! A.style (toValue "overflow:hidden;border:1px solid #ddd;background:#fafafa;cursor:grab") $ H.preEscapedToHtml "<svg id=\"prov-hier\" width=\"880\" height=\"500\"></svg>"
      -- Call initProv with the provenance file path
      H.script $ H.preEscapedToHtml $ "initProv(\"" ++ isoLower ++ "-prov.xml\");"

-- ---------------------------------------------------------------------------
-- Screenshots table
-- ---------------------------------------------------------------------------

-- | Generate a table of screenshots referenced in provenance.
-- For each entity with a screenshot, shows the image, the attributes derived
-- from it, and any notes (e.g. relationship type like "implied").
screenshotsTable :: [SourcedElement] -> String
screenshotsTable elems =
  let allEntities = nub [ e | (_, src) <- elems, e <- sourceEntitiesFlat src ]
      screenshotEntities = [ e | e <- allEntities, entityScreenshot e /= Nothing ]
  in if null screenshotEntities
     then ""
     else "<table class=\"screenshots-table\"><thead><tr>"
       ++ "<th>Screenshot</th><th>Attributes</th><th>Notes</th>"
       ++ "</tr></thead><tbody>"
       ++ concatMap (screenshotRow elems) screenshotEntities
       ++ "</tbody></table>"

screenshotRow :: [SourcedElement] -> Entity -> String
screenshotRow elems entity =
  let Just (_date, path) = entityScreenshot entity
      -- Find all sourced elements that reference this entity
      attrs = [ (name, noteForSource src entity)
              | (name, src) <- elems
              , entityReferencedBy src entity
              ]
      attrsHtml = if null attrs
                  then "<em>None</em>"
                  else "<ul>" ++ concatMap (\(n, _) -> "<li>" ++ escapeHtml n ++ "</li>") attrs ++ "</ul>"
      notes = filter (not . null) $ map snd attrs
      notesHtml = if null notes
                  then ""
                  else intercalate "<br>" (nub notes)
  in "<tr>"
     ++ "<td><img src=\"/images/" ++ escapeHtml path ++ "\"></td>"
     ++ "<td>" ++ attrsHtml ++ "</td>"
     ++ "<td>" ++ notesHtml ++ "</td>"
     ++ "</tr>"

-- | Does this Source reference the given entity (directly or as corroboration)?
entityReferencedBy :: Source -> Entity -> Bool
entityReferencedBy (SourceReference e) entity = e == entity
entityReferencedBy (SourceImpliedReference e) entity = e == entity
entityReferencedBy (SourceUnsightedReference e refs) entity = e == entity || entity `elem` refs
entityReferencedBy (SourceEditorial refs) entity = entity `elem` refs

-- | Generate a note string for how this source relates to the entity.
noteForSource :: Source -> Entity -> String
noteForSource (SourceReference _) _ = ""
noteForSource (SourceImpliedReference _) _ = "implied"
noteForSource (SourceUnsightedReference e _) entity
  | e == entity = "unsighted"
  | otherwise   = "corroborating"
noteForSource (SourceEditorial _) _ = "editorial"

-- | Get all entities from a Source (flat list).
sourceEntitiesFlat :: Source -> [Entity]
sourceEntitiesFlat (SourceReference e) = [e]
sourceEntitiesFlat (SourceImpliedReference e) = [e]
sourceEntitiesFlat (SourceUnsightedReference e refs) = e : refs
sourceEntitiesFlat (SourceEditorial refs) = refs

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
      rows = concat
        [ if llCount > 0 then ["\\text{\9472}\\!\\cap\\!\\text{\9472} &\\times " ++ show llCount] else []
        , if lcCount > 0 then ["\\text{\9472}\\!\\cap\\!\\bigcirc &\\times " ++ show lcCount] else []
        , if ccCount > 0 then ["\\bigcirc\\!\\cap\\!\\bigcirc &\\times " ++ show ccCount] else []
        , if ftCount > 0 then ["\\blacktriangle &\\times " ++ show ftCount] else []
        , if fcCount > 0 then ["\\bullet &\\times " ++ show fcCount] else []
        ]
  in if null rows
     then "<em>None</em>"
     else "<span style=\"font-size:75%\">$$\\begin{aligned}" ++ intercalate "\\\\" rows ++ "\\end{aligned}$$</span>"

formatSources :: [SourcedElement] -> String
formatSources [] = "<em>None</em>"
formatSources elems =
  let grouped = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) elems
  in "<ul>" ++ concatMap formatSourceGroup grouped ++ "</ul>"

sourceKey :: Source -> String
sourceKey (SourceReference e) = "1" ++ entityTitle e
sourceKey (SourceImpliedReference e) = "2" ++ entityTitle e
sourceKey (SourceUnsightedReference e _) = "3" ++ entityTitle e
sourceKey (SourceEditorial _) = "4"

formatSourceGroup :: [SourcedElement] -> String
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
