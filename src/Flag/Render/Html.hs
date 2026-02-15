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

import Flag.Source (Source(..), Entity(..), SourcedElement)
import Flag.Construction.Interpreter (Step(..))

-- ---------------------------------------------------------------------------
-- Main index page
-- ---------------------------------------------------------------------------

-- | Generate the index.html content
generateIndex :: [(String, String, String, String, [SourcedElement], [Step], String)] -> String
generateIndex flags = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>Constructible Flags</title>"
  , "  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css\">"
  , "  <script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js\"></script>"
  , "  <script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js\" onload=\"renderMathInElement(document.body, {delimiters: [{left: '$$', right: '$$', display: true}, {left: '$', right: '$', display: false}]});\"></script>"
  , "  <style>"
  , "    body { font-family: sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }"
  , "    table { border-collapse: collapse; width: 100%; }"
  , "    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; vertical-align: top; }"
  , "    th { background-color: #f4f4f4; }"
  , "    img { max-width: 150px; height: auto; }"
  , "    ul { margin: 0; padding-left: 20px; }"
  , "    .katex-display { margin: 0;  }"
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
    flagRow (svgFile, name, _desc, isoCode, sources, constructionSteps, field) = unlines
      [ "      <tr>"
      , "        <td><a href=\"" ++ svgFile ++ "\"><img src=\"" ++ svgFile ++ "\" alt=\"" ++ escapeHtml name ++ " flag\"></a></td>"
      , "        <td><a href=\"" ++ map toLower isoCode ++ ".html\">" ++ escapeHtml name ++ "</a></td>"
      , "        <td>"
          ++ " <div style=\"text-align:center\"><a href=\"debug-v2/?flag=" ++ map toLower isoCode ++ "\">" ++ show (length constructionSteps) ++ " cost</a></div>"
          ++ formatSteps constructionSteps
          ++ "<div style=\"text-align:center\">$" ++ field ++ "$</div>"
          ++ "</td>"
      , "        <td>" ++ formatSources sources ++ "<div style=\"margin-top:8px;font-size:0.85em\"><a href=\"" ++ map toLower isoCode ++ "-prov.xml\">[PROV]</a></div></td>"
      , "      </tr>"
      ]

-- ---------------------------------------------------------------------------
-- Show page
-- ---------------------------------------------------------------------------

-- | Generate a show page for a single flag
generateShowPage :: (String, String, String, String, [SourcedElement], [Step], String) -> String
generateShowPage (svgFile, name, desc, isoCode, sources, constructionSteps, field) =
  let isoLower = map toLower isoCode
  in unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>" ++ escapeHtml name ++ " - Constructible Flags</title>"
  , "  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css\">"
  , "  <script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js\"></script>"
  , "  <script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js\" onload=\"renderMathInElement(document.body, {delimiters: [{left: '$$', right: '$$', display: true}, {left: '$', right: '$', display: false}]}); initProv();\"></script>"
  , "  <script src=\"https://d3js.org/d3.v7.min.js\"></script>"
  , "  <style>"
  , "    body { font-family: sans-serif; max-width: 900px; margin: 0 auto; padding: 20px; }"
  , "    .flag-header { display: flex; gap: 30px; align-items: flex-start; margin-bottom: 30px; }"
  , "    .flag-header img { max-width: 300px; height: auto; border: 1px solid #ddd; }"
  , "    .flag-info h1 { margin-top: 0; }"
  , "    .description { white-space: pre-line; margin-bottom: 20px; }"
  , "    .construction { margin-bottom: 30px; }"
  , "    .sources ul { padding-left: 20px; }"
  , "    .elements { font-size: 0.9em; color: #666; }"
  , "    .hier-link { fill: none; stroke: #999; stroke-opacity: 0.5; }"
  , "    .hier-node text { font-size: 11px; }"
  , "    .screenshots-table { border-collapse: collapse; width: 100%; }"
  , "    .screenshots-table th, .screenshots-table td { border: 1px solid #ddd; padding: 8px; text-align: left; vertical-align: top; }"
  , "    .screenshots-table th { background-color: #f4f4f4; }"
  , "    .screenshots-table img { max-width: 200px; height: auto; }"
  , "    .katex-display { margin: 0; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <p><a href=\"index.html\">&larr; All flags</a></p>"
  , "  <div class=\"flag-header\">"
  , "    <a href=\"" ++ svgFile ++ "\"><img src=\"" ++ svgFile ++ "\" alt=\"" ++ escapeHtml name ++ " flag\"></a>"
  , "    <div class=\"flag-info\">"
  , "      <h1>" ++ escapeHtml name ++ "</h1>"
  , "      <div class=\"description\">" ++ escapeHtml desc ++ "</div>"
  , "    </div>"
  , "  </div>"
  , ""
  , "  <h2>Construction</h2>"
  , "  <div class=\"construction\">"
  , "    <div><a href=\"debug-v2/?flag=" ++ isoLower ++ "\">Interactive viewer</a> &mdash; " ++ show (length constructionSteps) ++ " cost</div>"
  , "    " ++ formatSteps constructionSteps
  , "    <div>$" ++ field ++ "$</div>"
  , "  </div>"
  , ""
  , "  <h2>Sources</h2>"
  , "  <div class=\"sources\">"
  , "    " ++ formatSources sources
  , "  </div>"
  , ""
  , "  <h2>Screenshots</h2>"
  , screenshotsTable sources
  , ""
  , "  <h2>Provenance</h2>"
  , "  <div style=\"overflow:hidden;border:1px solid #ddd;background:#fafafa;cursor:grab\">"
  , "  <svg id=\"prov-hier\" width=\"880\" height=\"500\"></svg>"
  , "  </div>"
  , ""
  , provScript isoLower
  , "</body>"
  , "</html>"
  ]

-- | D3 script for rendering PROV XML as a hierarchical left-to-right graph
provScript :: String -> String
provScript isoLower = unlines
  [ "<script>"
  , "function initProv() {"
  , "  fetch('" ++ isoLower ++ "-prov.xml')"
  , "    .then(r => r.text())"
  , "    .then(xml => {"
  , "      const parser = new DOMParser();"
  , "      const doc = parser.parseFromString(xml, 'application/xml');"
  , ""
  , "      const allNodes = [];"
  , "      const nodeMap = {};"
  , "      function addNode(id, label, provType, cfType) {"
  , "        if (nodeMap[id]) return;"
  , "        const n = {id, label, provType, cfType};"
  , "        nodeMap[id] = n;"
  , "        allNodes.push(n);"
  , "      }"
  , "      function getCfType(el) {"
  , "        const t = el.querySelector('type');"
  , "        return t ? t.textContent.trim() : null;"
  , "      }"
  , ""
  , "      doc.querySelectorAll('entity').forEach(el => {"
  , "        const id = el.getAttribute('prov:id');"
  , "        const label = el.querySelector('label')?.textContent || id;"
  , "        if (id) addNode(id, label, 'entity', getCfType(el));"
  , "      });"
  , "      doc.querySelectorAll('activity').forEach(el => {"
  , "        const id = el.getAttribute('prov:id');"
  , "        const label = el.querySelector('label')?.textContent || id;"
  , "        if (id) addNode(id, label, 'activity', getCfType(el));"
  , "      });"
  , "      doc.querySelectorAll('agent').forEach(el => {"
  , "        const id = el.getAttribute('prov:id');"
  , "        const label = el.querySelector('label')?.textContent || id;"
  , "        if (id) addNode(id, label, 'agent', getCfType(el));"
  , "      });"
  , ""
  , "      const allLinks = [];"
  , "      function ref(el, tag) {"
  , "        const c = el.querySelector(tag);"
  , "        return c ? c.getAttribute('prov:ref') : null;"
  , "      }"
  , ""
  , "      doc.querySelectorAll('wasGeneratedBy').forEach(el => {"
  , "        const e = ref(el,'entity'); const a = ref(el,'activity');"
  , "        if (e && a) allLinks.push({source:a, target:e, label:'wasGeneratedBy'});"
  , "      });"
  , "      doc.querySelectorAll('used').forEach(el => {"
  , "        const a = ref(el,'activity'); const e = ref(el,'entity');"
  , "        if (a && e) allLinks.push({source:a, target:e, label:'used'});"
  , "      });"
  , "      doc.querySelectorAll('wasDerivedFrom').forEach(el => {"
  , "        const gen = ref(el,'generatedEntity'); const used = ref(el,'usedEntity');"
  , "        if (gen && used) allLinks.push({source:used, target:gen, label:'wasDerivedFrom'});"
  , "      });"
  , "      doc.querySelectorAll('wasAttributedTo').forEach(el => {"
  , "        const e = ref(el,'entity'); const a = ref(el,'agent');"
  , "        if (e && a) allLinks.push({source:a, target:e, label:'wasAttributedTo'});"
  , "      });"
  , "      doc.querySelectorAll('wasAssociatedWith').forEach(el => {"
  , "        const act = ref(el,'activity'); const ag = ref(el,'agent');"
  , "        if (act && ag) allLinks.push({source:ag, target:act, label:'wasAssociatedWith'});"
  , "      });"
  , "      doc.querySelectorAll('wasInfluencedBy').forEach(el => {"
  , "        const ee = ref(el,'influencee'); const er = ref(el,'influencer');"
  , "        if (ee && er) allLinks.push({source:er, target:ee, label:'wasInfluencedBy'});"
  , "      });"
  , ""
  , "      // Filter out Flag, Construction, Screenshot, activity, and agent nodes"
  , "      // but bridge their inputs to their outputs to preserve connectivity"
  , "      const excludeTypes = new Set(['cf:Flag', 'cf:Construction', 'cf:Screenshot']);"
  , "      const excludeIds = new Set(allNodes.filter(n => excludeTypes.has(n.cfType) || n.provType === 'activity' || n.provType === 'agent').map(n => n.id));"
  , "      excludeIds.forEach(id => {"
  , "        const incoming = allLinks.filter(l => l.target === id);"
  , "        const outgoing = allLinks.filter(l => l.source === id);"
  , "        for (const i of incoming) {"
  , "          for (const o of outgoing) {"
  , "            allLinks.push({source: i.source, target: o.target, label: o.label});"
  , "          }"
  , "        }"
  , "      });"
  , "      const nodes = allNodes.filter(n => !excludeIds.has(n.id));"
  , "      const filteredNodeMap = {};"
  , "      nodes.forEach(n => filteredNodeMap[n.id] = n);"
  , "      const links = allLinks.filter(l => filteredNodeMap[l.source] && filteredNodeMap[l.target]);"
  , ""
  , "      renderProvHierarchy(nodes, links);"
  , "    });"
  , "}"
  , ""
  , "function nodeColor(d) {"
  , "  if (d.cfType === 'cf:SourcedValue') return '#2171b5';"
  , "  return '#9ecae1';"
  , "}"
  , ""
  , "function nodeRadius(d) {"
  , "  return d.cfType === 'cf:SourcedValue' ? 8 : 6;"
  , "}"
  , ""
  , "function nodeLabel(d) {"
  , "  if (d.cfType === 'cf:Translation') return 'Translation';"
  , "  return d.label;"
  , "}"
  , ""
  , "function renderProvHierarchy(nodes, links) {"
  , "  const svg = d3.select('#prov-hier');"
  , "  const pad = {top: 10, bottom: 10};"
  , ""
  , "  // Classify nodes into columns (left-to-right)"
  , "  // Col 0: source documents (leftmost)"
  , "  // Col 1: translations"
  , "  // Col 2: sourced values (rightmost)"
  , "  nodes.forEach(n => {"
  , "    const ct = n.cfType;"
  , "    if (ct === 'cf:SourcedValue') {"
  , "      n.col = 2;"
  , "    } else if (ct === 'cf:Translation') {"
  , "      n.col = 1;"
  , "    } else {"
  , "      n.col = 0;"
  , "    }"
  , "  });"
  , ""
  , "  const cols = [[], [], []];"
  , "  nodes.forEach(n => cols[n.col].push(n));"
  , ""
  , "  const usedCols = cols.map((_,i) => i).filter(i => cols[i].length > 0);"
  , "  const colCount = usedCols.length;"
  , "  const maxRows = Math.max(...cols.map(c => c.length), 1);"
  , "  const rowH = 22;"
  , "  const height = Math.max(100, maxRows * rowH + pad.top + pad.bottom);"
  , "  const width = +svg.attr('width');"
  , "  const midX = width / 2;"
  , "  svg.attr('height', height);"
  , ""
  , "  // Position columns: spread evenly across the middle of the SVG"
  , "  const spread = width * 0.18;"
  , "  const colX = {};"
  , "  if (colCount === 1) {"
  , "    colX[usedCols[0]] = midX;"
  , "  } else {"
  , "    usedCols.forEach((ci, idx) => {"
  , "      colX[ci] = midX - spread + idx * (spread * 2 / Math.max(1, colCount - 1));"
  , "    });"
  , "  }"
  , ""
  , "  nodes.forEach(n => {"
  , "    n.x = colX[n.col];"
  , "    const colNodes = cols[n.col];"
  , "    const idx = colNodes.indexOf(n);"
  , "    const spacing = (height - pad.top - pad.bottom) / Math.max(1, colNodes.length);"
  , "    n.y = pad.top + spacing * (idx + 0.5);"
  , "  });"
  , ""
  , "  // Align each translation with the source document it derives from"
  , "  const nodeById = {};"
  , "  nodes.forEach(n => nodeById[n.id] = n);"
  , "  nodes.filter(n => n.cfType === 'cf:Translation').forEach(n => {"
  , "    const src = links.find(l => l.target === n.id && nodeById[l.source] && nodeById[l.source].col === 0);"
  , "    if (src) n.y = nodeById[src.source].y;"
  , "  });"
  , ""
  , "  const g = svg.append('g');"
  , ""
  , "  // Pan via click+drag"
  , "  const zoom = d3.zoom()"
  , "    .scaleExtent([1, 1])"
  , "    .on('zoom', e => g.attr('transform', e.transform));"
  , "  svg.call(zoom).on('wheel.zoom', null);"
  , ""
  , "  g.append('defs').append('marker')"
  , "    .attr('id','hier-arrow').attr('viewBox','0 0 10 10')"
  , "    .attr('refX',10).attr('refY',5)"
  , "    .attr('markerWidth',6).attr('markerHeight',6)"
  , "    .attr('orient','auto')"
  , "    .append('path').attr('d','M0,0 L10,5 L0,10 Z').attr('fill','#999');"
  , ""
  , "  const hierNodeMap = {};"
  , "  nodes.forEach(n => hierNodeMap[n.id] = n);"
  , ""
  , "  g.append('g').selectAll('path')"
  , "    .data(links).join('path')"
  , "    .attr('class','hier-link')"
  , "    .attr('d', d => {"
  , "      const s = hierNodeMap[d.source.id || d.source];"
  , "      const t = hierNodeMap[d.target.id || d.target];"
  , "      if (!s || !t) return '';"
  , "      const dx = t.x - s.x;"
  , "      return `M${s.x},${s.y} C${s.x + dx*0.5},${s.y} ${t.x - dx*0.5},${t.y} ${t.x},${t.y}`;"
  , "    })"
  , "    .attr('stroke','#999')"
  , "    .attr('stroke-dasharray', d => d.label === 'wasInfluencedBy' ? '1,3' : null)"
  , "    .attr('stroke-linecap', d => d.label === 'wasInfluencedBy' ? 'round' : null)"
  , "    .attr('marker-end','url(#hier-arrow)');"
  , ""
  , "  const node = g.append('g').selectAll('g')"
  , "    .data(nodes).join('g')"
  , "    .attr('class','hier-node')"
  , "    .attr('transform', d => `translate(${d.x},${d.y})`);"
  , ""
  , "  node.append('circle')"
  , "    .attr('r', d => nodeRadius(d))"
  , "    .attr('fill', d => nodeColor(d))"
  , "    .attr('stroke', '#fff')"
  , "    .attr('stroke-width', d => d.cfType === 'cf:SourcedValue' ? 2 : 1);"
  , ""
  , "  node.append('text')"
  , "    .attr('dx', d => d.col === 0 ? -10 : 10)"
  , "    .attr('dy', 4)"
  , "    .attr('text-anchor', d => d.col === 0 ? 'end' : 'start')"
  , "    .style('font-weight', d => d.cfType === 'cf:SourcedValue' ? 'bold' : 'normal')"
  , "    .text(d => nodeLabel(d));"
  , "}"
  , "</script>"
  ]

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
