// Debug V2 - Interactive Construction Viewer
// Uses Preact + HTM (loaded from CDN) for a zero-build-step React-like app.

import { h, render, Component } from 'https://esm.sh/preact@10.19.3';
import { useState, useEffect, useRef, useMemo, useCallback } from 'https://esm.sh/preact@10.19.3/hooks';
import htm from 'https://esm.sh/htm@3.1.1';

const html = htm.bind(h);

// ---------------------------------------------------------------------------
// Data loading (fetched from URL param ?flag=<iso>)
// ---------------------------------------------------------------------------

let DATA = null;

// ---------------------------------------------------------------------------
// Utility: collect all leaf indices from a tree node
// ---------------------------------------------------------------------------

function allLeafIndices(node) {
  if (node.type === 'leaf') return [node.index];
  return node.children.flatMap(allLeafIndices);
}

// Count total leaves
function countLeaves(tree) {
  return tree.reduce((n, node) => {
    if (node.type === 'leaf') return n + 1;
    return n + countLeaves(node.children);
  }, 0);
}

// Collect all leaves in order
function collectLeaves(tree) {
  const result = [];
  for (const node of tree) {
    if (node.type === 'leaf') result.push(node);
    else result.push(...collectLeaves(node.children));
  }
  return result;
}

// Collect all group labels (for initial all-expanded state)
function collectGroupLabels(tree) {
  const labels = new Set();
  for (const node of tree) {
    if (node.type === 'group') {
      labels.add(node.label);
      collectGroupLabels(node.children).forEach(l => labels.add(l));
    }
  }
  return labels;
}

// ---------------------------------------------------------------------------
// Virtual layer mapping
// ---------------------------------------------------------------------------

// Build a mapping from virtual slider positions to sets of leaf indices.
// A collapsed group maps to a single virtual layer containing all its leaves.
// An expanded group maps each child to its own virtual layer(s).
function buildVirtualLayers(tree, expandedGroups) {
  const layers = [];
  for (const node of tree) {
    if (node.type === 'leaf') {
      layers.push([node.index]);
    } else if (expandedGroups.has(node.label)) {
      layers.push(...buildVirtualLayers(node.children, expandedGroups));
    } else {
      // Collapsed group: all leaves as one virtual layer
      const indices = allLeafIndices(node);
      if (indices.length > 0) layers.push(indices);
    }
  }
  return layers;
}

// Given a virtual position range [lo, hi], return the set of visible leaf indices
function visibleLeafIndices(virtualLayers, lo, hi) {
  const indices = new Set();
  for (let i = lo; i <= hi && i < virtualLayers.length; i++) {
    for (const idx of virtualLayers[i]) {
      indices.add(idx);
    }
  }
  return indices;
}

// Find which virtual layer a leaf index belongs to
function leafToVirtual(virtualLayers, leafIndex) {
  for (let i = 0; i < virtualLayers.length; i++) {
    if (virtualLayers[i].includes(leafIndex)) return i;
  }
  return 0;
}

// ---------------------------------------------------------------------------
// Geometry rendering from structured instructions
// ---------------------------------------------------------------------------

// Render construction geometry (dashed lines/circles) from a geom instruction.
function renderGeom(geom) {
  if (!geom) return null;
  switch (geom.type) {
    case 'intersectLL':
      return html`
        <line key="l1" x1=${geom.l1[0][0]} y1=${geom.l1[0][1]} x2=${geom.l1[1][0]} y2=${geom.l1[1][1]} stroke="#999" fill="none" />
        <line key="l2" x1=${geom.l2[0][0]} y1=${geom.l2[0][1]} x2=${geom.l2[1][0]} y2=${geom.l2[1][1]} stroke="#999" fill="none" />
      `;
    case 'intersectLC':
      return html`
        <line key="line" x1=${geom.line[0][0]} y1=${geom.line[0][1]} x2=${geom.line[1][0]} y2=${geom.line[1][1]} stroke="#999" fill="none" />
        <circle key="circ" cx=${geom.cx} cy=${geom.cy} r=${geom.r} stroke="#999" fill="none" />
      `;
    case 'intersectCC':
      return html`
        <circle key="c1" cx=${geom.cx1} cy=${geom.cy1} r=${geom.r1} stroke="#999" fill="none" />
        <circle key="c2" cx=${geom.cx2} cy=${geom.cy2} r=${geom.r2} stroke="#999" fill="none" />
      `;
    case 'circle':
      return html`<circle cx=${geom.cx} cy=${geom.cy} r=${geom.r} stroke="#999" fill="none" />`;
    default:
      return null;
  }
}

// Render a persistent filled shape from a fill instruction.
function renderFill(fill) {
  if (!fill) return null;
  switch (fill.type) {
    case 'triangle': {
      const pts = fill.pts.map(([x, y]) => `${x},${y}`).join(' ');
      return html`<polygon points=${pts} fill=${fill.color} fillOpacity="0.7" stroke=${fill.color} />`;
    }
    case 'circle':
      return html`<circle cx=${fill.cx} cy=${fill.cy} r=${fill.r} fill=${fill.color} fillOpacity="0.7" stroke=${fill.color} />`;
    default:
      return null;
  }
}

// ---------------------------------------------------------------------------
// SVG Viewer Component
// ---------------------------------------------------------------------------

function SvgViewer({ leaves, visibleSet, lo, hi, virtualLayers, hoveredPoint, setHoveredPoint }) {
  const svgRef = useRef(null);

  // Parse the fill viewBox for initial centering on the flag's filled area.
  // The fillViewBox is in original coordinates, but the SVG content is y-flipped
  // by scale(1,-1) translate(0, -T) where T = getSvgTranslateY(), so we must
  // transform the y-range to match.
  const originalVB = useMemo(() => {
    const [x, y, w, h] = DATA.fillViewBox.split(' ').map(Number);
    const T = getSvgTranslateY();
    return { x, y: T - y - h, w, h };
  }, []);

  // Pan/zoom state: store the current viewBox, initialised from URL if present
  const [vb, setVb] = useState(() => {
    const p = new URLSearchParams(window.location.search);
    const vx = parseFloat(p.get('vx'));
    const vy = parseFloat(p.get('vy'));
    const vw = parseFloat(p.get('vw'));
    const vh = parseFloat(p.get('vh'));
    if ([vx, vy, vw, vh].every(n => !isNaN(n) && isFinite(n))) {
      return { x: vx, y: vy, w: vw, h: vh };
    }
    return originalVB;
  });
  const panRef = useRef({ isPanning: false, startX: 0, startY: 0, startVb: null });

  // Sync viewBox to URL params
  useEffect(() => {
    const p = new URLSearchParams(window.location.search);
    p.set('vx', vb.x);
    p.set('vy', vb.y);
    p.set('vw', vb.w);
    p.set('vh', vb.h);
    history.replaceState(null, '', '?' + p.toString());
  }, [vb]);

  // Convert a screen-space point to SVG viewBox coordinates
  const screenToSvg = useCallback((clientX, clientY) => {
    const svg = svgRef.current;
    if (!svg) return { x: 0, y: 0 };
    const ctm = svg.getScreenCTM();
    if (!ctm) return { x: 0, y: 0 };
    const inv = ctm.inverse();
    return {
      x: inv.a * clientX + inv.c * clientY + inv.e,
      y: inv.b * clientX + inv.d * clientY + inv.f
    };
  }, []);

  // Zoom with mouse wheel
  const onWheel = useCallback((e) => {
    e.preventDefault();
    const svg = svgRef.current;
    if (!svg) return;

    // Get the SVG-space point under the cursor
    const pt = screenToSvg(e.clientX, e.clientY);
    const zoomFactor = e.deltaY > 0 ? 1.1 : 1 / 1.1;

    setVb(prev => {
      const newW = prev.w * zoomFactor;
      const newH = prev.h * zoomFactor;
      // Keep the point under the cursor fixed
      const fx = (pt.x - prev.x) / prev.w;
      const fy = (pt.y - prev.y) / prev.h;
      const newX = pt.x - fx * newW;
      const newY = pt.y - fy * newH;
      return { x: newX, y: newY, w: newW, h: newH };
    });
  }, [screenToSvg]);

  // Pan with middle-click or left-click drag
  const onPointerDown = useCallback((e) => {
    // Pan on middle button, or left button with no target dot
    if (e.button === 1 || (e.button === 0 && !e.target.classList.contains('dot'))) {
      e.preventDefault();
      const svg = svgRef.current;
      if (!svg) return;
      svg.setPointerCapture(e.pointerId);
      // Capture the CTM scale at drag start so it stays fixed during the drag
      const ctm = svg.getScreenCTM();
      panRef.current = {
        isPanning: true,
        startX: e.clientX,
        startY: e.clientY,
        startVb: { ...vb },
        scaleX: ctm ? ctm.a : 1,
        scaleY: ctm ? ctm.d : 1
      };
    }
  }, [vb]);

  const onPointerMove = useCallback((e) => {
    const pan = panRef.current;
    if (!pan.isPanning) return;

    // Convert pixel delta to viewBox units using the fixed scale from drag start
    const dx = (e.clientX - pan.startX) / pan.scaleX;
    const dy = (e.clientY - pan.startY) / pan.scaleY;

    setVb({
      x: pan.startVb.x - dx,
      y: pan.startVb.y - dy,
      w: pan.startVb.w,
      h: pan.startVb.h
    });
  }, []);

  const onPointerUp = useCallback((e) => {
    if (panRef.current.isPanning) {
      panRef.current.isPanning = false;
      const svg = svgRef.current;
      if (svg) svg.releasePointerCapture(e.pointerId);
    }
  }, []);

  // Reset view on double-click
  const onDblClick = useCallback((e) => {
    if (!e.target.classList.contains('dot')) {
      setVb(originalVB);
    }
  }, [originalVB]);

  // Attach wheel listener with { passive: false } so preventDefault works
  useEffect(() => {
    const svg = svgRef.current;
    if (!svg) return;
    svg.addEventListener('wheel', onWheel, { passive: false });
    return () => svg.removeEventListener('wheel', onWheel);
  }, [onWheel]);

  const allLeaves = leaves;
  const maxVisibleIndex = Math.max(0, ...visibleSet);
  const viewBoxStr = `${vb.x} ${vb.y} ${vb.w} ${vb.h}`;

  // Fixed screen-size radii: convert target pixel sizes to viewBox units
  const svgEl = svgRef.current;
  const svgScreenW = svgEl ? svgEl.clientWidth || 600 : 600;
  const pxToVb = vb.w / svgScreenW;
  const hitRadius = pxToVb * 30;
  const dotRadiusOuter = pxToVb * 5;
  const dotRadiusInner = pxToVb * 4;

  return html`
    <svg ref=${svgRef}
         viewBox=${viewBoxStr}
         class="construction-svg"
         xmlns="http://www.w3.org/2000/svg"
         onPointerDown=${onPointerDown}
         onPointerMove=${onPointerMove}
         onPointerUp=${onPointerUp}
         onDblClick=${onDblClick}>
      <g transform="scale(1,-1) translate(0, ${-getSvgTranslateY()})">
        ${allLeaves.map(leaf => {
          const isVisible = visibleSet.has(leaf.index);
          const showFill = leaf.index <= maxVisibleIndex;
          return html`
            <g key=${leaf.index}>
              ${showFill && leaf.fill && html`
                <g class="fill-layer">${renderFill(leaf.fill)}</g>
              `}
              <g>
                ${isVisible && leaf.geom && html`
                  <g class="geom-layer">${renderGeom(leaf.geom)}</g>
                `}
                ${isVisible && html`
                  <g class="dots-layer">
                    ${(leaf.points || []).map((pt, i) => html`
                      <circle key=${'hit' + i}
                              cx=${pt.x} cy=${pt.y} r=${hitRadius}
                              fill="transparent" class="dot-hitbox"
                              onMouseEnter=${() => setHoveredPoint(pt)}
                              onMouseLeave=${() => setHoveredPoint(null)} />
                      <circle key=${i}
                              cx=${pt.x} cy=${pt.y} r=${dotRadiusOuter}
                              fill="black" class="dot"
                              pointer-events="none" />
                    `)}
                    ${(leaf.inputPoints || []).map((pt, i) => html`
                      <circle key=${'inhit' + i}
                              cx=${pt.x} cy=${pt.y} r=${hitRadius}
                              fill="transparent" class="dot-hitbox"
                              onMouseEnter=${() => setHoveredPoint(pt)}
                              onMouseLeave=${() => setHoveredPoint(null)} />
                      <circle key=${'in' + i}
                              cx=${pt.x} cy=${pt.y} r=${dotRadiusInner}
                              fill="#666" class="dot"
                              pointer-events="none" />
                    `)}
                  </g>
                `}
              </g>
            </g>
          `;
        })}
        ${/* Initial points always visible */ ''}
        ${DATA.initialPoints.map((pt, i) => html`
          <circle key=${'inithit' + i}
                  cx=${pt.x} cy=${pt.y} r=${hitRadius}
                  fill="transparent" class="dot-hitbox"
                  onMouseEnter=${() => setHoveredPoint(pt)}
                  onMouseLeave=${() => setHoveredPoint(null)} />
          <circle key=${'init' + i}
                  cx=${pt.x} cy=${pt.y} r=${dotRadiusOuter}
                  fill="blue" class="dot"
                  pointer-events="none" />
        `)}
      </g>
    </svg>
  `;
}

// Parse the viewBox to get the Y translation for the y-flip
function getSvgTranslateY() {
  const parts = DATA.viewBox.split(' ').map(Number);
  // viewBox: minX minY width height
  // After scale(1,-1), we need to translate by -(minY + height) to flip correctly
  return parts[1] + parts[3];
}

// ---------------------------------------------------------------------------
// Range Slider Component (dual-thumb with midpoint)
// ---------------------------------------------------------------------------

function RangeSlider({ lo, hi, max: maxVal, onLoChange, onHiChange, onMidDrag }) {
  const trackRef = useRef(null);
  const [dragging, setDragging] = useState(null); // 'lo' | 'hi' | 'mid'
  const dragStartRef = useRef({ startX: 0, startLo: 0, startHi: 0 });

  const toPosition = useCallback((val) => {
    return maxVal === 0 ? 0 : (val / maxVal) * 100;
  }, [maxVal]);

  const fromPosition = useCallback((pct) => {
    return Math.round((pct / 100) * maxVal);
  }, [maxVal]);

  const getTrackPct = useCallback((clientX) => {
    const rect = trackRef.current.getBoundingClientRect();
    return Math.max(0, Math.min(100, ((clientX - rect.left) / rect.width) * 100));
  }, []);

  useEffect(() => {
    if (!dragging) return;

    const onMove = (e) => {
      const pct = getTrackPct(e.clientX);
      const val = fromPosition(pct);

      if (dragging === 'lo') {
        onLoChange(Math.min(val, hi));
      } else if (dragging === 'hi') {
        onHiChange(Math.max(val, lo));
      } else if (dragging === 'mid') {
        const { startX, startLo, startHi } = dragStartRef.current;
        const deltaPct = ((e.clientX - startX) / trackRef.current.getBoundingClientRect().width) * 100;
        const deltaVal = Math.round((deltaPct / 100) * maxVal);
        const width = startHi - startLo;
        let newLo = startLo + deltaVal;
        let newHi = startHi + deltaVal;
        if (newLo < 0) { newLo = 0; newHi = width; }
        if (newHi > maxVal) { newHi = maxVal; newLo = maxVal - width; }
        onMidDrag(newLo, newHi);
      }
    };

    const onUp = () => setDragging(null);
    window.addEventListener('mousemove', onMove);
    window.addEventListener('mouseup', onUp);
    return () => {
      window.removeEventListener('mousemove', onMove);
      window.removeEventListener('mouseup', onUp);
    };
  }, [dragging, lo, hi, maxVal]);

  const loPos = toPosition(lo);
  const hiPos = toPosition(hi);
  const midPos = (loPos + hiPos) / 2;

  const startDrag = (which, e) => {
    e.preventDefault();
    dragStartRef.current = { startX: e.clientX, startLo: lo, startHi: hi };
    setDragging(which);
  };

  return html`
    <div class="range-slider">
      <div class="range-slider-track" ref=${trackRef}>
        <div class="range-slider-fill"
             style=${{ left: loPos + '%', width: (hiPos - loPos) + '%' }} />
        <div class="range-slider-thumb range-slider-thumb-lo"
             style=${{ left: loPos + '%' }}
             onMouseDown=${(e) => startDrag('lo', e)}>
          <span class="thumb-label">${lo}</span>
        </div>
        <div class="range-slider-thumb range-slider-thumb-mid"
             style=${{ left: midPos + '%' }}
             onMouseDown=${(e) => startDrag('mid', e)}>
          <span class="thumb-label">⬥</span>
        </div>
        <div class="range-slider-thumb range-slider-thumb-hi"
             style=${{ left: hiPos + '%' }}
             onMouseDown=${(e) => startDrag('hi', e)}>
          <span class="thumb-label">${hi}</span>
        </div>
      </div>
      <div class="range-slider-labels">
        <span>0</span>
        <span>${maxVal}</span>
      </div>
    </div>
  `;
}

// ---------------------------------------------------------------------------
// Step Tree Component (collapsible sidebar)
// ---------------------------------------------------------------------------

function StepTree({ tree, visibleSet, midpointIndex, expandedGroups, onToggleGroup }) {
  return html`
    <div class="step-tree">
      ${tree.map((node, i) => html`
        <${StepNode} key=${i}
                     node=${node}
                     visibleSet=${visibleSet}
                     midpointIndex=${midpointIndex}
                     expandedGroups=${expandedGroups}
                     onToggleGroup=${onToggleGroup}
                     depth=${0} />
      `)}
    </div>
  `;
}

function StepNode({ node, visibleSet, midpointIndex, expandedGroups, onToggleGroup, depth }) {
  if (node.type === 'leaf') {
    const isActive = visibleSet.has(node.index);
    const isMidpoint = midpointIndex === node.index;
    return html`
      <div class=${'step-leaf' + (isActive ? ' active' : '') + (isMidpoint ? ' midpoint' : '')}
           style=${{ paddingLeft: (depth * 16 + 8) + 'px' }}>
        <span class="step-number">${node.index}.</span>
        <span class="step-label">${node.label}</span>
      </div>
    `;
  }

  // Group node
  const isExpanded = expandedGroups.has(node.label);
  const groupLeaves = allLeafIndices(node);
  const anyActive = groupLeaves.some(idx => visibleSet.has(idx));
  const anyMidpoint = groupLeaves.includes(midpointIndex);

  return html`
    <div class=${'step-group' + (anyActive ? ' active' : '') + (anyMidpoint ? ' midpoint' : '')}>
      <div class="step-group-header"
           style=${{ paddingLeft: (depth * 16 + 8) + 'px' }}
           onClick=${() => onToggleGroup(node.label)}>
        <span class="step-group-toggle">${isExpanded ? '▼' : '▶'}</span>
        <span class="step-group-label">${node.label}</span>
        <span class="step-group-count">(${groupLeaves.length})</span>
      </div>
      ${isExpanded && html`
        <div class="step-group-children">
          ${node.children.map((child, i) => html`
            <${StepNode} key=${i}
                         node=${child}
                         visibleSet=${visibleSet}
                         midpointIndex=${midpointIndex}
                         expandedGroups=${expandedGroups}
                         onToggleGroup=${onToggleGroup}
                         depth=${depth + 1} />
          `)}
        </div>
      `}
    </div>
  `;
}

// ---------------------------------------------------------------------------
// Coordinate Tooltip
// ---------------------------------------------------------------------------

function CoordTooltip({ point }) {
  const ref = useRef(null);

  useEffect(() => {
    if (!ref.current || !point || typeof katex === 'undefined') return;
    const exactX = point.exactX || String(Number(point.x).toFixed(4));
    const exactY = point.exactY || String(Number(point.y).toFixed(4));
    const tex = `\\left(\\, ${exactX} \\,,\\; ${exactY} \\,\\right)`;
    katex.render(tex, ref.current, { throwOnError: false, displayMode: false });
  }, [point]);

  if (!point) return null;

  return html`
    <div class="coord-tooltip" ref=${ref} />
  `;
}

// ---------------------------------------------------------------------------
// App Component
// ---------------------------------------------------------------------------

function App() {
  const allLeaves = useMemo(() => collectLeaves(DATA.tree), []);
  const totalLeaves = allLeaves.length;

  const [expandedGroups, setExpandedGroups] = useState(() => collectGroupLabels(DATA.tree));

  const [lo, setLo] = useState(() => {
    const p = new URLSearchParams(window.location.search);
    const v = parseInt(p.get('lo'), 10);
    return isNaN(v) ? 0 : Math.max(0, Math.min(v, totalLeaves));
  });
  const [hi, setHi] = useState(() => {
    const p = new URLSearchParams(window.location.search);
    const v = parseInt(p.get('hi'), 10);
    return isNaN(v) ? totalLeaves : Math.max(0, Math.min(v, totalLeaves));
  });

  // Sync lo/hi to URL params without adding history entries
  useEffect(() => {
    const p = new URLSearchParams(window.location.search);
    p.set('lo', lo);
    p.set('hi', hi);
    history.replaceState(null, '', '?' + p.toString());
  }, [lo, hi]);
  const [hoveredPoint, setHoveredPoint] = useState(null);

  // Build virtual layers based on expanded groups
  const virtualLayers = useMemo(
    () => buildVirtualLayers(DATA.tree, expandedGroups),
    [expandedGroups]
  );

  const maxVirtual = virtualLayers.length;

  // Clamp lo/hi when virtual layers change
  useEffect(() => {
    if (lo > maxVirtual) setLo(maxVirtual);
    if (hi > maxVirtual) setHi(maxVirtual);
  }, [maxVirtual]);

  // Compute which leaf indices are visible
  const visibleSet = useMemo(
    () => visibleLeafIndices(virtualLayers, lo, hi),
    [virtualLayers, lo, hi]
  );

  // Compute the midpoint virtual position and its "representative" leaf index
  const midVirtual = Math.floor((lo + hi) / 2);
  const midpointLeafIndex = midVirtual < virtualLayers.length
    ? virtualLayers[midVirtual][0]
    : null;

  // Toggle group expansion
  const toggleGroup = useCallback((label) => {
    setExpandedGroups(prev => {
      const next = new Set(prev);
      if (next.has(label)) {
        next.delete(label);
      } else {
        next.add(label);
      }
      return next;
    });
  }, []);

  // Midpoint drag handler
  const onMidDrag = useCallback((newLo, newHi) => {
    setLo(newLo);
    setHi(newHi);
  }, []);

  // Auto-scroll the midpoint step into view
  const treeRef = useRef(null);
  useEffect(() => {
    if (!treeRef.current || midpointLeafIndex == null) return;
    const el = treeRef.current.querySelector('.step-leaf.midpoint');
    if (el) {
      el.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
    }
  }, [midpointLeafIndex]);

  return html`
    <div class="app">
      <header class="app-header">
        <h1>Construction Debug — ${DATA.flagName}</h1>
      </header>
      <div class="app-body">
        <div class="svg-panel">
          <${SvgViewer}
            leaves=${allLeaves}
            visibleSet=${visibleSet}
            lo=${lo}
            hi=${hi}
            virtualLayers=${virtualLayers}
            hoveredPoint=${hoveredPoint}
            setHoveredPoint=${setHoveredPoint} />
          <${CoordTooltip} point=${hoveredPoint} />
          <${RangeSlider}
            lo=${lo}
            hi=${hi}
            max=${maxVirtual}
            onLoChange=${setLo}
            onHiChange=${setHi}
            onMidDrag=${onMidDrag} />
        </div>
        <div class="tree-panel" ref=${treeRef}>
          <h2>Steps</h2>
          <${StepTree}
            tree=${DATA.tree}
            visibleSet=${visibleSet}
            midpointIndex=${midpointLeafIndex}
            expandedGroups=${expandedGroups}
            onToggleGroup=${toggleGroup} />
        </div>
      </div>
    </div>
  `;
}

// ---------------------------------------------------------------------------
// Styles (injected into <head>)
// ---------------------------------------------------------------------------

const styles = `
  * { box-sizing: border-box; margin: 0; padding: 0; }

  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    background: #f5f5f5;
    color: #333;
  }

  .app-header {
    background: #2c3e50;
    color: white;
    padding: 12px 24px;
  }

  .app-header h1 {
    font-size: 18px;
    font-weight: 500;
  }

  .app-body {
    display: flex;
    height: calc(100vh - 48px);
  }

  .svg-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    padding: 16px;
    position: relative;
  }

  .construction-svg {
    flex: 1;
    width: 100%;
    background: white;
    border: 1px solid #ddd;
    border-radius: 4px;
    cursor: grab;
    touch-action: none;
  }

  .construction-svg:active {
    cursor: grabbing;
  }

  .tree-panel {
    width: 300px;
    border-left: 1px solid #ddd;
    background: white;
    overflow-y: auto;
    padding: 16px 0;
  }

  .tree-panel h2 {
    font-size: 14px;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    color: #888;
    padding: 0 16px 8px;
    border-bottom: 1px solid #eee;
    margin-bottom: 4px;
  }

  /* Step tree */
  .step-leaf {
    padding: 4px 8px;
    font-size: 13px;
    cursor: default;
    display: flex;
    align-items: center;
    gap: 6px;
    border-left: 3px solid transparent;
    transition: background 0.1s;
  }

  .step-leaf.active {
    background: #e8f4fd;
    border-left-color: #3498db;
  }

  .step-leaf.midpoint {
    background: #d5f0d5;
    border-left-color: #27ae60;
    font-weight: 600;
  }

  .step-number {
    color: #999;
    font-size: 11px;
    min-width: 24px;
  }

  .step-group-header {
    padding: 6px 8px;
    font-size: 13px;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 6px;
    user-select: none;
    transition: background 0.1s;
  }

  .step-group-header:hover {
    background: #f0f0f0;
  }

  .step-group.active > .step-group-header {
    background: #f0f8ff;
  }

  .step-group.midpoint > .step-group-header {
    background: #eef9ee;
  }

  .step-group-toggle {
    font-size: 10px;
    color: #999;
    width: 12px;
  }

  .step-group-label {
    font-weight: 500;
  }

  .step-group-count {
    color: #aaa;
    font-size: 11px;
  }

  /* Range slider */
  .range-slider {
    padding: 16px 8px 8px;
  }

  .range-slider-track {
    position: relative;
    height: 6px;
    background: #ddd;
    border-radius: 3px;
    margin: 12px 0;
  }

  .range-slider-fill {
    position: absolute;
    height: 100%;
    background: #3498db;
    border-radius: 3px;
  }

  .range-slider-thumb {
    position: absolute;
    top: 50%;
    transform: translate(-50%, -50%);
    width: 20px;
    height: 20px;
    background: white;
    border: 2px solid #3498db;
    border-radius: 50%;
    cursor: grab;
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 2;
    user-select: none;
  }

  .range-slider-thumb:active {
    cursor: grabbing;
  }

  .range-slider-thumb-mid {
    background: #3498db;
    color: white;
    z-index: 3;
  }

  .thumb-label {
    font-size: 9px;
    font-weight: 600;
    pointer-events: none;
  }

  .range-slider-thumb-mid .thumb-label {
    color: white;
  }

  .range-slider-labels {
    display: flex;
    justify-content: space-between;
    font-size: 11px;
    color: #999;
  }

  /* Coordinate tooltip */
  .coord-tooltip {
    position: absolute;
    top: 24px;
    right: 24px;
    background: rgba(0, 0, 0, 0.8);
    color: white;
    padding: 4px 10px;
    border-radius: 4px;
    font-size: 13px;
    font-family: 'SF Mono', 'Fira Code', monospace;
    pointer-events: none;
    z-index: 10;
  }

  /* Construction geometry: scale-invariant strokes */
  .geom-layer line,
  .geom-layer circle {
    vector-effect: non-scaling-stroke;
    stroke-width: 1.5;
  }

  .geom-layer line {
    stroke-dasharray: 5 4;
  }

  .geom-layer circle {
    stroke-dasharray: 8 5;
  }

  /* Fill layer outlines: scale-invariant */
  .fill-layer polygon,
  .fill-layer circle {
    vector-effect: non-scaling-stroke;
    stroke-width: 1;
  }

  /* Dot hover */
  .dot {
    cursor: crosshair;
  }

  .dot:hover {
    r: 0.06;
    fill: #e74c3c;
  }
`;

// Inject styles
const styleEl = document.createElement('style');
styleEl.textContent = styles;
document.head.appendChild(styleEl);

// ---------------------------------------------------------------------------
// Mount (async: fetch JSON then render)
// ---------------------------------------------------------------------------

async function main() {
  const params = new URLSearchParams(window.location.search);
  const flagCode = params.get('flag');
  const app = document.getElementById('app');

  if (!flagCode) {
    app.textContent = 'No ?flag= parameter provided.';
    return;
  }

  try {
    const resp = await fetch(`../construction/${flagCode}.json`);
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    DATA = await resp.json();
  } catch (e) {
    app.textContent = `Failed to load ${flagCode}: ${e.message}`;
    return;
  }

  render(html`<${App} />`, app);
}

main();
