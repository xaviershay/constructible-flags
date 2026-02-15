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
import Data.List (sortOn, groupBy, intercalate)
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
  , "    #prov-graph, #prov-hier { border: 1px solid #ddd; margin-top: 10px; background: #fafafa; }"
  , "    .prov-node text { font-size: 11px; }"
  , "    .prov-link { stroke-opacity: 0.6; fill: none; }"
  , "    .prov-link-label { font-size: 9px; fill: #888; }"
  , "    .hier-link { fill: none; stroke: #999; stroke-opacity: 0.5; }"
  , "    .hier-node text { font-size: 10px; }"
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
  , "  <h2>Provenance</h2>"
  , "  <h3>Hierarchical view</h3>"
  , "  <svg id=\"prov-hier\" width=\"880\" height=\"500\"></svg>"
  , "  <h3>Force-directed view</h3>"
  , "  <svg id=\"prov-graph\" width=\"880\" height=\"500\"></svg>"
  , ""
  , provScript isoLower
  , "</body>"
  , "</html>"
  ]

-- | D3 script for rendering PROV XML as a force-directed graph
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
  , "      // Filter out Flag and Construction nodes for visualizations"
  , "      const excludeTypes = new Set(['cf:Flag', 'cf:Construction']);"
  , "      const excludeIds = new Set(allNodes.filter(n => excludeTypes.has(n.cfType)).map(n => n.id));"
  , "      const nodes = allNodes.filter(n => !excludeIds.has(n.id));"
  , "      const filteredNodeMap = {};"
  , "      nodes.forEach(n => filteredNodeMap[n.id] = n);"
  , "      const links = allLinks.filter(l => filteredNodeMap[l.source] && filteredNodeMap[l.target]);"
  , ""
  , "      renderProvGraph(nodes, links);"
  , "      renderProvHierarchy(nodes, links);"
  , "    });"
  , "}"
  , ""
  , "function nodeColor(d) {"
  , "  if (d.cfType === 'cf:SourcedValue') return '#2171b5';"
  , "  if (d.provType === 'entity') return '#9ecae1';"
  , "  if (d.provType === 'activity') return '#fdae6b';"
  , "  return '#74c476';"
  , "}"
  , ""
  , "function nodeRadius(d) {"
  , "  return d.cfType === 'cf:SourcedValue' ? 14 : 10;"
  , "}"
  , ""
  , "function drawNode(g, d) {"
  , "  const r = nodeRadius(d);"
  , "  const fill = nodeColor(d);"
  , "  if (d.provType === 'entity') {"
  , "    g.append('circle').attr('r',r).attr('fill',fill).attr('stroke','#fff').attr('stroke-width', d.cfType === 'cf:SourcedValue' ? 2 : 1);"
  , "  } else if (d.provType === 'activity') {"
  , "    g.append('rect').attr('x',-r).attr('y',-r).attr('width',r*2).attr('height',r*2)"
  , "      .attr('fill',fill).attr('stroke','#fff');"
  , "  } else {"
  , "    const s = r * 1.2;"
  , "    g.append('polygon').attr('points',`0,${-s} ${s*0.95},${-s*0.31} ${s*0.59},${s*0.81} ${-s*0.59},${s*0.81} ${-s*0.95},${-s*0.31}`)"
  , "      .attr('fill',fill).attr('stroke','#fff');"
  , "  }"
  , "}"
  , ""
  , "function renderProvGraph(nodes, links) {"
  , "  const svg = d3.select('#prov-graph');"
  , "  const width = +svg.attr('width');"
  , "  const height = +svg.attr('height');"
  , ""
  , "  svg.append('defs').append('marker')"
  , "    .attr('id','arrow').attr('viewBox','0 0 10 10')"
  , "    .attr('refX',20).attr('refY',5)"
  , "    .attr('markerWidth',6).attr('markerHeight',6)"
  , "    .attr('orient','auto')"
  , "    .append('path').attr('d','M0,0 L10,5 L0,10 Z').attr('fill','#999');"
  , ""
  , "  const sim = d3.forceSimulation(nodes)"
  , "    .force('link', d3.forceLink(links).id(d => d.id).distance(120))"
  , "    .force('charge', d3.forceManyBody().strength(-300))"
  , "    .force('center', d3.forceCenter(width/2, height/2))"
  , "    .force('collide', d3.forceCollide(40));"
  , ""
  , "  const link = svg.append('g')"
  , "    .selectAll('line')"
  , "    .data(links).join('line')"
  , "    .attr('class','prov-link')"
  , "    .attr('stroke','#999')"
  , "    .attr('marker-end','url(#arrow)');"
  , ""
  , "  const linkLabel = svg.append('g')"
  , "    .selectAll('text')"
  , "    .data(links).join('text')"
  , "    .attr('class','prov-link-label')"
  , "    .text(d => d.label);"
  , ""
  , "  const node = svg.append('g')"
  , "    .selectAll('g')"
  , "    .data(nodes).join('g')"
  , "    .attr('class','prov-node')"
  , "    .call(d3.drag()"
  , "      .on('start', (e,d) => { if(!e.active) sim.alphaTarget(0.3).restart(); d.fx=d.x; d.fy=d.y; })"
  , "      .on('drag', (e,d) => { d.fx=e.x; d.fy=e.y; })"
  , "      .on('end', (e,d) => { if(!e.active) sim.alphaTarget(0); d.fx=null; d.fy=null; }));"
  , ""
  , "  node.each(function(d) { drawNode(d3.select(this), d); });"
  , ""
  , "  node.append('text')"
  , "    .attr('dx', 16).attr('dy', 4)"
  , "    .style('font-weight', d => d.cfType === 'cf:SourcedValue' ? 'bold' : 'normal')"
  , "    .text(d => d.label);"
  , ""
  , "  sim.on('tick', () => {"
  , "    link.attr('x1',d=>d.source.x).attr('y1',d=>d.source.y)"
  , "        .attr('x2',d=>d.target.x).attr('y2',d=>d.target.y);"
  , "    linkLabel.attr('x',d=>(d.source.x+d.target.x)/2)"
  , "            .attr('y',d=>(d.source.y+d.target.y)/2);"
  , "    node.attr('transform',d=>`translate(${d.x},${d.y})`);"
  , "  });"
  , "}"
  , ""
  , "function renderProvHierarchy(nodes, links) {"
  , "  const svg = d3.select('#prov-hier');"
  , "  const width = +svg.attr('width');"
  , "  const pad = {top: 50, bottom: 40, left: 40, right: 40};"
  , ""
  , "  // Classify nodes into layers using cf:type"
  , "  // Layer 0: sourced values"
  , "  // Layer 1: screenshots + translations + their activities"
  , "  // Layer 2: source documents"
  , "  // Layer 3: agents"
  , "  nodes.forEach(n => {"
  , "    const ct = n.cfType;"
  , "    if (n.provType === 'agent') {"
  , "      n.layer = 3;"
  , "    } else if (ct === 'cf:SourcedValue') {"
  , "      n.layer = 0;"
  , "    } else if (ct === 'cf:Screenshot' || ct === 'cf:Translation' || ct === 'cf:View') {"
  , "      n.layer = 1;"
  , "    } else {"
  , "      n.layer = 2;"
  , "    }"
  , "  });"
  , ""
  , "  const layers = [[], [], [], []];"
  , "  nodes.forEach(n => layers[n.layer].push(n));"
  , ""
  , "  const usedLayers = layers.map((l,i) => i).filter(i => layers[i].length > 0);"
  , "  const layerCount = usedLayers.length;"
  , "  const height = Math.max(500, layerCount * 120 + pad.top + pad.bottom);"
  , "  svg.attr('height', height);"
  , ""
  , "  const layerY = {};"
  , "  usedLayers.forEach((li, idx) => {"
  , "    layerY[li] = pad.top + idx * ((height - pad.top - pad.bottom) / Math.max(1, layerCount - 1));"
  , "  });"
  , ""
  , "  nodes.forEach(n => {"
  , "    n.y = layerY[n.layer];"
  , "    const layerNodes = layers[n.layer];"
  , "    const idx = layerNodes.indexOf(n);"
  , "    const spacing = (width - pad.left - pad.right) / Math.max(1, layerNodes.length);"
  , "    n.x = pad.left + spacing * (idx + 0.5);"
  , "  });"
  , ""
  , "  svg.append('defs').append('marker')"
  , "    .attr('id','hier-arrow').attr('viewBox','0 0 10 10')"
  , "    .attr('refX',10).attr('refY',5)"
  , "    .attr('markerWidth',6).attr('markerHeight',6)"
  , "    .attr('orient','auto')"
  , "    .append('path').attr('d','M0,0 L10,5 L0,10 Z').attr('fill','#999');"
  , ""
  , "  const hierNodeMap = {};"
  , "  nodes.forEach(n => hierNodeMap[n.id] = n);"
  , ""
  , "  svg.append('g').selectAll('path')"
  , "    .data(links).join('path')"
  , "    .attr('class','hier-link')"
  , "    .attr('d', d => {"
  , "      const s = hierNodeMap[d.source.id || d.source];"
  , "      const t = hierNodeMap[d.target.id || d.target];"
  , "      if (!s || !t) return '';"
  , "      const dy = t.y - s.y;"
  , "      return `M${s.x},${s.y} C${s.x},${s.y + dy*0.5} ${t.x},${t.y - dy*0.5} ${t.x},${t.y}`;"
  , "    })"
  , "    .attr('stroke','#999')"
  , "    .attr('marker-end','url(#hier-arrow)');"
  , ""
  , "  svg.append('g').selectAll('text')"
  , "    .data(links).join('text')"
  , "    .attr('class','prov-link-label')"
  , "    .attr('x', d => {"
  , "      const s = hierNodeMap[d.source.id || d.source];"
  , "      const t = hierNodeMap[d.target.id || d.target];"
  , "      return s && t ? (s.x + t.x) / 2 : 0;"
  , "    })"
  , "    .attr('y', d => {"
  , "      const s = hierNodeMap[d.source.id || d.source];"
  , "      const t = hierNodeMap[d.target.id || d.target];"
  , "      return s && t ? (s.y + t.y) / 2 : 0;"
  , "    })"
  , "    .text(d => d.label);"
  , ""
  , "  const node = svg.append('g').selectAll('g')"
  , "    .data(nodes).join('g')"
  , "    .attr('class','hier-node')"
  , "    .attr('transform', d => `translate(${d.x},${d.y})`);"
  , ""
  , "  node.each(function(d) { drawNode(d3.select(this), d); });"
  , ""
  , "  node.append('text')"
  , "    .attr('text-anchor', d => d.layer === 0 ? 'middle' : 'start')"
  , "    .attr('dx', d => d.layer === 0 ? 0 : 14)"
  , "    .attr('dy', d => d.layer === 0 ? -20 : 4)"
  , "    .style('font-weight', d => d.cfType === 'cf:SourcedValue' ? 'bold' : 'normal')"
  , "    .text(d => d.label);"
  , "}"
  , "</script>"
  ]

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
