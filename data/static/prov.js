function initProv(path) {
  fetch(path)
    .then(r => r.json())
    .then(data => {
      const nodes = [];
      const nodeMap = {};

      function addNode(id, label, provType, cfType) {
        if (nodeMap[id]) return;
        const n = {id, label, provType, cfType};
        nodeMap[id] = n;
        nodes.push(n);
      }

      function getCfType(props) {
        const t = props['prov:type'];
        if (!t) return null;
        return (typeof t === 'object') ? t['$'] : t;
      }

      for (const [id, p] of Object.entries(data.entity   || {}))
        addNode(id, p['prov:label'] || id, 'entity',   getCfType(p));
      for (const [id, p] of Object.entries(data.activity || {}))
        addNode(id, p['prov:label'] || id, 'activity', getCfType(p));
      for (const [id, p] of Object.entries(data.agent    || {}))
        addNode(id, p['prov:label'] || id, 'agent',    getCfType(p));

      const links = [];

      for (const r of Object.values(data.wasGeneratedBy   || {})) {
        const e = r['prov:entity'],    a = r['prov:activity'];
        if (e && a && nodeMap[e] && nodeMap[a])
          links.push({source: a, target: e, rel: 'wasGeneratedBy'});
      }
      for (const r of Object.values(data.used             || {})) {
        const a = r['prov:activity'],  e = r['prov:entity'];
        if (a && e && nodeMap[a] && nodeMap[e])
          links.push({source: a, target: e, rel: 'used'});
      }
      for (const r of Object.values(data.wasDerivedFrom   || {})) {
        const g = r['prov:generatedEntity'], u = r['prov:usedEntity'];
        if (g && u && nodeMap[g] && nodeMap[u])
          links.push({source: u, target: g, rel: 'wasDerivedFrom'});
      }
      for (const r of Object.values(data.wasAttributedTo  || {})) {
        const e = r['prov:entity'],    a = r['prov:agent'];
        if (e && a && nodeMap[e] && nodeMap[a])
          links.push({source: a, target: e, rel: 'wasAttributedTo'});
      }
      for (const r of Object.values(data.wasAssociatedWith || {})) {
        const act = r['prov:activity'], ag = r['prov:agent'];
        if (act && ag && nodeMap[act] && nodeMap[ag])
          links.push({source: ag, target: act, rel: 'wasAssociatedWith'});
      }
      for (const r of Object.values(data.wasInfluencedBy  || {})) {
        const ee = r['prov:influencee'], er = r['prov:influencer'];
        if (ee && er && nodeMap[ee] && nodeMap[er])
          links.push({source: er, target: ee, rel: 'wasInfluencedBy'});
      }

      renderProv(nodes, links);
    })
    .catch(err => console.error('Failed to load PROV JSON', err));
}

// ---- Visual encoding -------------------------------------------------------

const REL_COLOR = {
  wasGeneratedBy:    '#2ca02c',
  used:              '#98df8a',
  wasDerivedFrom:    '#1f77b4',
  wasAttributedTo:   '#d62728',
  wasAssociatedWith: '#ff7f0e',
  wasInfluencedBy:   '#9467bd',
};

function nodeColor(d) {
  if (d.provType === 'agent')    return '#f6ae2d';
  if (d.provType === 'activity') return '#74c476';
  switch (d.cfType) {
    case 'cf:Flag':           return '#08306b';
    case 'cf:Attribute':      return '#2171b5';
    case 'cf:SourceDocument': return '#6baed6';
    case 'cf:Screenshot':     return '#c6dbef';
    case 'cf:Translation':    return '#9ecae1';
    default:                  return '#4292c6';
  }
}

function nodeRadius(d) {
  if (d.cfType === 'cf:Flag')      return 12;
  if (d.cfType === 'cf:Attribute') return 9;
  return 7;
}

// Approximate half-size for edge endpoint trimming
function nodeHalfSize(n) {
  if (n.provType === 'agent')    return 11; // diamond half
  if (n.provType === 'activity') return 10; // rect half-height
  return nodeRadius(n);
}

// ---- Renderer --------------------------------------------------------------

function renderProv(nodes, links) {
  const svg = d3.select('#prov-hier');
  const width  = +svg.attr('width')  || 880;
  const height = +svg.attr('height') || 500;

  // Arrow markers, one per relation type
  const defs = svg.append('defs');
  for (const [rel, color] of Object.entries(REL_COLOR)) {
    defs.append('marker')
      .attr('id', 'arr-' + rel)
      .attr('viewBox', '0 0 10 10')
      .attr('refX', 10).attr('refY', 5)
      .attr('markerWidth', 6).attr('markerHeight', 6)
      .attr('orient', 'auto')
      .append('path').attr('d', 'M0,0 L10,5 L0,10 Z').attr('fill', color);
  }

  const g = svg.append('g');
  svg.call(d3.zoom().scaleExtent([0.2, 4]).on('zoom', e => g.attr('transform', e.transform)));

  const simulation = d3.forceSimulation(nodes)
    .force('link',    d3.forceLink(links).id(d => d.id).distance(90))
    .force('charge',  d3.forceManyBody().strength(-280))
    .force('center',  d3.forceCenter(width / 2, height / 2))
    .force('collide', d3.forceCollide(22));

  // Edges
  const link = g.append('g').attr('fill', 'none')
    .selectAll('line').data(links).join('line')
    .attr('stroke',           d => REL_COLOR[d.rel] || '#999')
    .attr('stroke-width',     1.5)
    .attr('stroke-dasharray', d => d.rel === 'wasInfluencedBy' ? '5,3' : null)
    .attr('marker-end',       d => `url(#arr-${d.rel})`);

  // Nodes
  const node = g.append('g')
    .selectAll('g').data(nodes).join('g')
    .call(d3.drag()
      .on('start', (ev, d) => { if (!ev.active) simulation.alphaTarget(0.3).restart(); d.fx = d.x; d.fy = d.y; })
      .on('drag',  (ev, d) => { d.fx = ev.x; d.fy = ev.y; })
      .on('end',   (ev, d) => { if (!ev.active) simulation.alphaTarget(0); d.fx = null; d.fy = null; }));

  // Shape per provenance type
  node.each(function(d) {
    const sel = d3.select(this);
    const fill   = nodeColor(d);
    const stroke = '#555';
    if (d.provType === 'agent') {
      const s = 11;
      sel.append('polygon')
        .attr('points', `0,${-s} ${s},0 0,${s} ${-s},0`)
        .attr('fill', fill).attr('stroke', stroke).attr('stroke-width', 1.5);
    } else if (d.provType === 'activity') {
      const rh = 10, rw = 22;
      sel.append('rect')
        .attr('x', -rw).attr('y', -rh)
        .attr('width', rw * 2).attr('height', rh * 2)
        .attr('rx', 4)
        .attr('fill', fill).attr('stroke', stroke).attr('stroke-width', 1.5);
    } else {
      sel.append('circle')
        .attr('r', nodeRadius(d))
        .attr('fill', fill).attr('stroke', stroke).attr('stroke-width', 1.5);
    }
  });

  // Tooltip showing full ID on hover
  node.append('title').text(d => d.id);

  // Labels below each node
  node.append('text')
    .attr('dy', d => nodeHalfSize(d) + 12)
    .attr('text-anchor', 'middle')
    .style('font-size', '10px')
    .style('pointer-events', 'none')
    .style('fill', '#333')
    .text(d => d.label);

  // Tick: recompute edge endpoints to terminate at node boundary
  simulation.on('tick', () => {
    link.each(function(d) {
      const dx = d.target.x - d.source.x;
      const dy = d.target.y - d.source.y;
      const len = Math.sqrt(dx * dx + dy * dy);
      if (len === 0) return;
      const rs = nodeHalfSize(d.source) + 2;
      const rt = nodeHalfSize(d.target) + 2;
      d3.select(this)
        .attr('x1', d.source.x + dx / len * rs)
        .attr('y1', d.source.y + dy / len * rs)
        .attr('x2', d.target.x - dx / len * rt)
        .attr('y2', d.target.y - dy / len * rt);
    });
    node.attr('transform', d => `translate(${d.x},${d.y})`);
  });
}
