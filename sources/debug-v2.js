// Debug V2 - Interactive Construction Viewer
// Uses Preact + HTM (loaded from CDN) for a zero-build-step React-like app.

import { h, render, Component } from 'https://esm.sh/preact@10.19.3';
import { useState, useEffect, useRef, useMemo, useCallback } from 'https://esm.sh/preact@10.19.3/hooks';
import htm from 'https://esm.sh/htm@3.1.1';

const html = htm.bind(h);

// ---------------------------------------------------------------------------
// Data loading
// ---------------------------------------------------------------------------

const DATA = window.__DEBUG_DATA__;

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
// SVG Viewer Component
// ---------------------------------------------------------------------------

function SvgViewer({ leaves, visibleSet, hoveredPoint, setHoveredPoint }) {
  const svgRef = useRef(null);

  // Build cumulative fills: for each visible leaf, show its fill
  // and all fills from leaves with lower indices that are also visible or earlier
  const allLeaves = leaves;

  // We want: for a leaf to show its fill, all leaves with index <= that leaf's index
  // should show their fill (cumulative). Construction geom only for leaves in visibleSet.
  const maxVisibleIndex = Math.max(0, ...visibleSet);

  return html`
    <svg ref=${svgRef}
         viewBox=${DATA.viewBox}
         class="construction-svg"
         xmlns="http://www.w3.org/2000/svg">
      <g transform="scale(1,-1) translate(0, ${-getSvgTranslateY()})">
        ${allLeaves.map(leaf => {
          const isVisible = visibleSet.has(leaf.index);
          const showFill = leaf.index <= maxVisibleIndex;
          return html`
            <g key=${leaf.index}>
              ${showFill && leaf.fillSvg && html`
                <g dangerouslySetInnerHTML=${{ __html: leaf.fillSvg }} />
              `}
              ${isVisible && leaf.geomSvg && html`
                <g class="geom-layer" dangerouslySetInnerHTML=${{ __html: leaf.geomSvg }} />
              `}
              ${isVisible && html`
                <g class="dots-layer">
                  ${(leaf.points || []).map((pt, i) => html`
                    <circle key=${i}
                            cx=${pt.x} cy=${pt.y} r="0.04"
                            fill="black" class="dot"
                            onMouseEnter=${() => setHoveredPoint(pt)}
                            onMouseLeave=${() => setHoveredPoint(null)} />
                  `)}
                  ${(leaf.inputPoints || []).map((pt, i) => html`
                    <circle key=${'in' + i}
                            cx=${pt.x} cy=${pt.y} r="0.03"
                            fill="#666" class="dot"
                            onMouseEnter=${() => setHoveredPoint(pt)}
                            onMouseLeave=${() => setHoveredPoint(null)} />
                  `)}
                </g>
              `}
            </g>
          `;
        })}
        ${/* Initial points always visible */ ''}
        ${DATA.initialPoints.map((pt, i) => html`
          <circle key=${'init' + i}
                  cx=${pt.x} cy=${pt.y} r="0.04"
                  fill="blue" class="dot"
                  onMouseEnter=${() => setHoveredPoint(pt)}
                  onMouseLeave=${() => setHoveredPoint(null)} />
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
  if (!point) return null;
  const ref = useRef(null);

  useEffect(() => {
    if (!ref.current || typeof katex === 'undefined') return;
    const exactX = point.exactX || String(Number(point.x).toFixed(4));
    const exactY = point.exactY || String(Number(point.y).toFixed(4));
    const tex = `\\left(\\, ${exactX} \\,,\\; ${exactY} \\,\\right)`;
    katex.render(tex, ref.current, { throwOnError: false, displayMode: false });
  }, [point]);

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

  const [expandedGroups, setExpandedGroups] = useState(new Set());
  const [lo, setLo] = useState(0);
  const [hi, setHi] = useState(totalLeaves);
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
// Mount
// ---------------------------------------------------------------------------

render(html`<${App} />`, document.getElementById('app'));
