// Wait for KaTeX auto-render to be available then render maths in the body
(function waitAndRender() {
  function tryRender() {
    if (typeof renderMathInElement === 'function') {
      try {
        renderMathInElement(document.body, {delimiters: [{left: '$$', right: '$$', display: true}, {left: '$', right: '$', display: false}]});
      } catch (e) {
        console.error('KaTeX render failed', e);
      }
    } else {
      setTimeout(tryRender, 50);
    }
  }
  if (document.readyState === 'complete' || document.readyState === 'interactive') {
    tryRender();
  } else {
    window.addEventListener('DOMContentLoaded', tryRender);
  }
})();
// Initialize KaTeX rendering and call initProv if available
if (typeof renderMathInElement === 'function') {
  document.addEventListener('DOMContentLoaded', () => {
    try {
      renderMathInElement(document.body, {delimiters: [{left: '$$', right: '$$', display: true}, {left: '$', right: '$', display: false}]});
    } catch (e) {
      // ignore
    }
    if (typeof initProv === 'function') {
      try { initProv(); } catch(e) { }
    }
  });
}
