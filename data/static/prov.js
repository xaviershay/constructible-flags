function initProv(path) {
  fetch(path)
    .then(r => r.text())
    .then(xml => {
      const parser = new DOMParser();
      const doc = parser.parseFromString(xml, 'application/xml');

      const allNodes = [];
      const nodeMap = {};
      function addNode(id, label, provType, cfType) {
        if (nodeMap[id]) return;
        const n = {id, label, provType, cfType};
        nodeMap[id] = n;
        allNodes.push(n);
      }
      function getCfType(el) {
        const t = el.querySelector('type');
        return t ? t.textContent.trim() : null;
      }

      doc.querySelectorAll('entity').forEach(el => {
        const id = el.getAttribute('prov:id');
        const label = el.querySelector('label')?.textContent || id;
        if (id) addNode(id, label, 'entity', getCfType(el));
      });
      doc.querySelectorAll('activity').forEach(el => {
        const id = el.getAttribute('prov:id');
        const label = el.querySelector('label')?.textContent || id;
        if (id) addNode(id, label, 'activity', getCfType(el));
      });
      doc.querySelectorAll('agent').forEach(el => {
        const id = el.getAttribute('prov:id');
        const label = el.querySelector('label')?.textContent || id;
        if (id) addNode(id, label, 'agent', getCfType(el));
      });

      const allLinks = [];
      function ref(el, tag) {
        const c = el.querySelector(tag);
        return c ? c.getAttribute('prov:ref') : null;
      }

      doc.querySelectorAll('wasGeneratedBy').forEach(el => {
        const e = ref(el,'entity'); const a = ref(el,'activity');
        if (e && a) allLinks.push({source:a, target:e, label:'wasGeneratedBy'});
      });
      doc.querySelectorAll('used').forEach(el => {
        const a = ref(el,'activity'); const e = ref(el,'entity');
        if (a && e) allLinks.push({source:a, target:e, label:'used'});
      });
      doc.querySelectorAll('wasDerivedFrom').forEach(el => {
        const gen = ref(el,'generatedEntity'); const used = ref(el,'usedEntity');
        if (gen && used) allLinks.push({source:used, target:gen, label:'wasDerivedFrom'});
      });
      doc.querySelectorAll('wasAttributedTo').forEach(el => {
        const e = ref(el,'entity'); const a = ref(el,'agent');
        if (e && a) allLinks.push({source:a, target:e, label:'wasAttributedTo'});
      });
      doc.querySelectorAll('wasAssociatedWith').forEach(el => {
        const act = ref(el,'activity'); const ag = ref(el,'agent');
        if (act && ag) allLinks.push({source:ag, target:act, label:'wasAssociatedWith'});
      });
      doc.querySelectorAll('wasInfluencedBy').forEach(el => {
        const ee = ref(el,'influencee'); const er = ref(el,'influencer');
        if (ee && er) allLinks.push({source:er, target:ee, label:'wasInfluencedBy'});
      });

      // Filter out Flag, Construction, Screenshot, activity, and agent nodes
      // but bridge their inputs to their outputs to preserve connectivity
      const excludeTypes = new Set(['cf:Flag', 'cf:Construction', 'cf:Screenshot']);
      const excludeIds = new Set(allNodes.filter(n => excludeTypes.has(n.cfType) || n.provType === 'activity' || n.provType === 'agent').map(n => n.id));
      excludeIds.forEach(id => {
        const incoming = allLinks.filter(l => l.target === id);
        const outgoing = allLinks.filter(l => l.source === id);
        for (const i of incoming) {
          for (const o of outgoing) {
            allLinks.push({source: i.source, target: o.target, label: o.label});
          }
        }
      });
      const nodes = allNodes.filter(n => !excludeIds.has(n.id));
      const filteredNodeMap = {};
      nodes.forEach(n => filteredNodeMap[n.id] = n);
      const links = allLinks.filter(l => filteredNodeMap[l.source] && filteredNodeMap[l.target]);

      renderProvHierarchy(nodes, links);
    }).catch(err => console.error('Failed to load prov xml', err));
}

function nodeColor(d) {
  if (d.cfType === 'cf:SourcedValue') return '#2171b5';
  return '#9ecae1';
}

function nodeRadius(d) {
  return d.cfType === 'cf:SourcedValue' ? 8 : 6;
}

function nodeLabel(d) {
  if (d.cfType === 'cf:Translation') return 'Translation';
  return d.label;
}

function renderProvHierarchy(nodes, links) {
  const svg = d3.select('#prov-hier');
  const pad = {top: 10, bottom: 10};

  nodes.forEach(n => {
    const ct = n.cfType;
    if (ct === 'cf:SourcedValue') {
      n.col = 2;
    } else if (ct === 'cf:Translation') {
      n.col = 1;
    } else {
      n.col = 0;
    }
  });

  const cols = [[], [], []];
  nodes.forEach(n => cols[n.col].push(n));

  const usedCols = cols.map((_,i) => i).filter(i => cols[i].length > 0);
  const colCount = usedCols.length;
  const maxRows = Math.max(...cols.map(c => c.length), 1);
  const rowH = 22;
  const height = Math.max(100, maxRows * rowH + pad.top + pad.bottom);
  const width = +svg.attr('width');
  const midX = width / 2;
  svg.attr('height', height);

  const spread = width * 0.18;
  const colX = {};
  if (colCount === 1) {
    colX[usedCols[0]] = midX;
  } else {
    usedCols.forEach((ci, idx) => {
      colX[ci] = midX - spread + idx * (spread * 2 / Math.max(1, colCount - 1));
    });
  }

  nodes.forEach(n => {
    n.x = colX[n.col];
    const colNodes = cols[n.col];
    const idx = colNodes.indexOf(n);
    const spacing = (height - pad.top - pad.bottom) / Math.max(1, colNodes.length);
    n.y = pad.top + spacing * (idx + 0.5);
  });

  const nodeById = {};
  nodes.forEach(n => nodeById[n.id] = n);
  nodes.filter(n => n.cfType === 'cf:Translation').forEach(n => {
    const src = links.find(l => l.target === n.id && nodeById[l.source] && nodeById[l.source].col === 0);
    if (src) n.y = nodeById[src.source].y;
  });

  const g = svg.append('g');

  const zoom = d3.zoom()
    .scaleExtent([1, 1])
    .on('zoom', e => g.attr('transform', e.transform));
  svg.call(zoom).on('wheel.zoom', null);

  g.append('defs').append('marker')
    .attr('id','hier-arrow').attr('viewBox','0 0 10 10')
    .attr('refX',10).attr('refY',5)
    .attr('markerWidth',6).attr('markerHeight',6)
    .attr('orient','auto')
    .append('path').attr('d','M0,0 L10,5 L0,10 Z').attr('fill','#999');

  const hierNodeMap = {};
  nodes.forEach(n => hierNodeMap[n.id] = n);

  g.append('g').selectAll('path')
    .data(links).join('path')
    .attr('class','hier-link')
    .attr('d', d => {
      const s = hierNodeMap[d.source.id || d.source];
      const t = hierNodeMap[d.target.id || d.target];
      if (!s || !t) return '';
      const dx = t.x - s.x;
      return `M${s.x},${s.y} C${s.x + dx*0.5},${s.y} ${t.x - dx*0.5},${t.y} ${t.x},${t.y}`;
    })
    .attr('stroke','#999')
    .attr('stroke-dasharray', d => d.label === 'wasInfluencedBy' ? '1,3' : null)
    .attr('stroke-linecap', d => d.label === 'wasInfluencedBy' ? 'round' : null)
    .attr('marker-end','url(#hier-arrow)');

  const node = g.append('g').selectAll('g')
    .data(nodes).join('g')
    .attr('class','hier-node')
    .attr('transform', d => `translate(${d.x},${d.y})`);

  node.append('circle')
    .attr('r', d => nodeRadius(d))
    .attr('fill', d => nodeColor(d))
    .attr('stroke', '#fff')
    .attr('stroke-width', d => d.cfType === 'cf:SourcedValue' ? 2 : 1);

  node.append('text')
    .attr('dx', d => d.col === 0 ? -10 : 10)
    .attr('dy', 4)
    .attr('text-anchor', d => d.col === 0 ? 'end' : 'start')
    .style('font-weight', d => d.cfType === 'cf:SourcedValue' ? 'bold' : 'normal')
    .text(d => nodeLabel(d));
}
// PROV rendering script (extracted from inline HTML)
// Exposes initProv() which fetches the -prov.xml and renders into #prov-hier
// (duplicate initProv removed) Use initProv(path) defined above instead.

function nodeColor(d) {
  if (d.cfType === 'cf:SourcedValue') return '#2171b5';
  return '#9ecae1';
}

function nodeRadius(d) {
  return d.cfType === 'cf:SourcedValue' ? 8 : 6;
}

function nodeLabel(d) {
  if (d.cfType === 'cf:Translation') return 'Translation';
  return d.label;
}

function renderProvHierarchy(nodes, links) {
  const svg = d3.select('#prov-hier');
  const pad = {top: 10, bottom: 10};

  nodes.forEach(n => {
    const ct = n.cfType;
    if (ct === 'cf:SourcedValue') {
      n.col = 2;
    } else if (ct === 'cf:Translation') {
      n.col = 1;
    } else {
      n.col = 0;
    }
  });

  const cols = [[], [], []];
  nodes.forEach(n => cols[n.col].push(n));

  const usedCols = cols.map((_,i) => i).filter(i => cols[i].length > 0);
  const colCount = usedCols.length;
  const maxRows = Math.max(...cols.map(c => c.length), 1);
  const rowH = 22;
  const height = Math.max(100, maxRows * rowH + pad.top + pad.bottom);
  const width = +svg.attr('width');
  const midX = width / 2;
  svg.attr('height', height);

  const spread = width * 0.18;
  const colX = {};
  if (colCount === 1) {
    colX[usedCols[0]] = midX;
  } else {
    usedCols.forEach((ci, idx) => {
      colX[ci] = midX - spread + idx * (spread * 2 / Math.max(1, colCount - 1));
    });
  }

  nodes.forEach(n => {
    n.x = colX[n.col];
    const colNodes = cols[n.col];
    const idx = colNodes.indexOf(n);
    const spacing = (height - pad.top - pad.bottom) / Math.max(1, colNodes.length);
    n.y = pad.top + spacing * (idx + 0.5);
  });

  const nodeById = {};
  nodes.forEach(n => nodeById[n.id] = n);
  nodes.filter(n => n.cfType === 'cf:Translation').forEach(n => {
    const src = links.find(l => l.target === n.id && nodeById[l.source] && nodeById[l.source].col === 0);
    if (src) n.y = nodeById[src.source].y;
  });

  const g = svg.append('g');

  const zoom = d3.zoom()
    .scaleExtent([1, 1])
    .on('zoom', e => g.attr('transform', e.transform));
  svg.call(zoom).on('wheel.zoom', null);

  g.append('defs').append('marker')
    .attr('id','hier-arrow').attr('viewBox','0 0 10 10')
    .attr('refX',10).attr('refY',5)
    .attr('markerWidth',6).attr('markerHeight',6)
    .attr('orient','auto')
    .append('path').attr('d','M0,0 L10,5 L0,10 Z').attr('fill','#999');

  const hierNodeMap = {};
  nodes.forEach(n => hierNodeMap[n.id] = n);

  g.append('g').selectAll('path')
    .data(links).join('path')
    .attr('class','hier-link')
    .attr('d', d => {
      const s = hierNodeMap[d.source.id || d.source];
      const t = hierNodeMap[d.target.id || d.target];
      if (!s || !t) return '';
      const dx = t.x - s.x;
      return `M${s.x},${s.y} C${s.x + dx*0.5},${s.y} ${t.x - dx*0.5},${t.y} ${t.x},${t.y}`;
    })
    .attr('stroke','#999')
    .attr('stroke-dasharray', d => d.label === 'wasInfluencedBy' ? '1,3' : null)
    .attr('stroke-linecap', d => d.label === 'wasInfluencedBy' ? 'round' : null)
    .attr('marker-end','url(#hier-arrow)');

  const node = g.append('g').selectAll('g')
    .data(nodes).join('g')
    .attr('class','hier-node')
    .attr('transform', d => `translate(${d.x},${d.y})`);

  node.append('circle')
    .attr('r', d => nodeRadius(d))
    .attr('fill', d => nodeColor(d))
    .attr('stroke', '#fff')
    .attr('stroke-width', d => d.cfType === 'cf:SourcedValue' ? 2 : 1);

  node.append('text')
    .attr('dx', d => d.col === 0 ? -10 : 10)
    .attr('dy', 4)
    .attr('text-anchor', d => d.col === 0 ? 'end' : 'start')
    .style('font-weight', d => d.cfType === 'cf:SourcedValue' ? 'bold' : 'normal')
    .text(d => nodeLabel(d));
}
