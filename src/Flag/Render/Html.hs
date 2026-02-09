{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Flag.Render.Html
    ( -- * Index page
      generateIndex
      -- * Utilities
    , escapeHtml
    ) where

import Data.List (sortOn, groupBy, intercalate)
import Data.Function (on)

import Flag.Source (Source(..), SourcedElement)
import Flag.Construction.Interpreter (Step(..))

-- ---------------------------------------------------------------------------
-- Main index page
-- ---------------------------------------------------------------------------

-- | Generate the index.html content
generateIndex :: [(String, String, String, String, [SourcedElement], [Step])] -> String
generateIndex flags = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>Constructible Flags</title>"
  , "  <style>"
  , "    body { font-family: sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }"
  , "    table { border-collapse: collapse; width: 100%; }"
  , "    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; vertical-align: top; }"
  , "    th { background-color: #f4f4f4; }"
  , "    img { max-width: 150px; height: auto; }"
  , "    ul { margin: 0; padding-left: 20px; }"
  , "    .elements { font-size: 0.9em; color: #666; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <h1>Constructible Flags</h1>"
  , "  <table>"
  , "    <thead>"
  , "      <tr>"
  , "        <th>Design</th>"
  , "        <th>Name</th>"
  , "        <th>Description</th>"
  , "        <th>Construction</th>"
  , "        <th>Sources</th>"
  , "      </tr>"
  , "    </thead>"
  , "    <tbody>"
  , concatMap flagRow flags
  , "    </tbody>"
  , "  </table>"
  , "</body>"
  , "</html>"
  ]
  where
    flagRow (svgFile, name, desc, _, sources, constructionSteps) = unlines
      [ "      <tr>"
      , "        <td><a href=\"" ++ svgFile ++ "\"><img src=\"" ++ svgFile ++ "\" alt=\"" ++ escapeHtml name ++ " flag\"></a></td>"
      , "        <td>" ++ escapeHtml name ++ "</td>"
      , "        <td>" ++ escapeHtml desc ++ "</td>"
      , "        <td>" ++ formatSteps constructionSteps ++ "</td>"
      , "        <td>" ++ formatSources sources ++ "</td>"
      , "      </tr>"
      ]
    
    -- Format construction steps grouped by kind
    formatSteps :: [Step] -> String
    formatSteps [] = "<em>None</em>"
    formatSteps ss =
      let llCount = length [() | StepIntersectLL <- ss]
          lcCount = length [() | StepIntersectLC <- ss]
          ccCount = length [() | StepIntersectCC <- ss]
          ftCount = length [() | StepFillTriangle <- ss]
          fcCount = length [() | StepFillCircle <- ss]
          items = concat
            [ if llCount > 0 then ["<li>Intersect line\8211line \215" ++ show llCount ++ "</li>"] else []
            , if lcCount > 0 then ["<li>Intersect line\8211circle \215" ++ show lcCount ++ "</li>"] else []
            , if ccCount > 0 then ["<li>Intersect circle\8211circle \215" ++ show ccCount ++ "</li>"] else []
            , if ftCount > 0 then ["<li>Fill triangle \215" ++ show ftCount ++ "</li>"] else []
            , if fcCount > 0 then ["<li>Fill circle \215" ++ show fcCount ++ "</li>"] else []
            ]
      in if null items
         then "<em>None</em>"
         else "<ul>" ++ concat items ++ "</ul>"
    
    -- Group elements by source and format
    formatSources :: [SourcedElement] -> String
    formatSources [] = "<em>None</em>"
    formatSources elems = 
      let grouped = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) elems
      in "<ul>" ++ concatMap formatSourceGroup grouped ++ "</ul>"
    
    -- Key for sorting sources (to group same sources together)
    sourceKey :: Source -> String
    sourceKey SourceHabitual = "0"
    sourceKey (SourceLaw title _) = "1" ++ title
    sourceKey (SourceAuthoritativeWebsite title _) = "2" ++ title
    sourceKey (SourcePublication title _) = "3" ++ title
    
    formatSourceGroup :: [SourcedElement] -> String
    formatSourceGroup [] = ""
    formatSourceGroup grp@((_, src):_) = 
      let elementNames = map fst grp
          elementsStr = "<span class=\"elements\">(" ++ escapeHtml (joinElements elementNames) ++ ")</span>"
      in formatSourceWithElements src elementsStr
    
    joinElements :: [String] -> String
    joinElements xs = intercalate ", " xs
    
    formatSourceWithElements :: Source -> String -> String
    formatSourceWithElements SourceHabitual elems = 
      "<li>Habitual practice " ++ elems ++ "</li>"
    formatSourceWithElements (SourceAuthoritativeWebsite title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> " ++ elems ++ "</li>"
    formatSourceWithElements (SourceLaw title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> (Law) " ++ elems ++ "</li>"
    formatSourceWithElements (SourcePublication title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> (Publication) " ++ elems ++ "</li>"

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
