/* custom.js — Data Harmonisation Assistant (construct-first) */

// ── Domain group collapse / expand ────────────────────────────
function toggleDomain(evt, domId) {
  evt.stopPropagation();
  var rows = document.getElementById('dr-' + domId);
  var hdr  = evt.currentTarget;
  if (!rows) return;
  var collapsed = rows.style.display === 'none';
  rows.style.display = collapsed ? '' : 'none';
  var span = hdr.querySelector('span');
  if (span) span.innerHTML = span.innerHTML.replace(/^[▼▶] /, collapsed ? '▼ ' : '▶ ');
}

// ── Checkbox selection sync ───────────────────────────────────
function syncConstructSelections() {
  var cbs = document.querySelectorAll('.construct-cb:checked');
  var ids = [];
  cbs.forEach(function(cb) { ids.push(cb.dataset.hn); });
  Shiny.setInputValue('construct_selections', ids, {priority: 'event'});
}

// ── Code drawer ───────────────────────────────────────────────
function toggleCodeDrawer() {
  var drawer  = document.getElementById('code-drawer');
  var content = document.getElementById('app-main-content');
  if (!drawer) return;
  var open = drawer.classList.toggle('open');
  if (content) content.classList.toggle('drawer-open', open);
  Shiny.setInputValue('code_drawer_open', open, {priority: 'event'});
}

// ── Detail panel ──────────────────────────────────────────────
function openDetailPanel() {
  var panel   = document.getElementById('detail-panel');
  var content = document.getElementById('app-main-content');
  if (panel) {
    panel.classList.add('open');
    // Brief flash animation to draw attention to the newly opened panel
    panel.classList.remove('dp-just-opened');
    void panel.offsetWidth; // force reflow
    panel.classList.add('dp-just-opened');
    setTimeout(function() { panel.classList.remove('dp-just-opened'); }, 1800);
  }
  if (content) content.classList.add('panel-open');
}

function closeDetailPanel() {
  var panel   = document.getElementById('detail-panel');
  var content = document.getElementById('app-main-content');
  if (panel)   panel.classList.remove('open');
  if (content) content.classList.remove('panel-open');
  Shiny.setInputValue('close_detail_panel', Math.random(), {priority: 'event'});
}

// Visually hide panel WITHOUT clearing rv$active_construct in Shiny.
// Used when navigating to search from a construct's detail panel.
function hideDetailPanel() {
  var panel   = document.getElementById('detail-panel');
  var content = document.getElementById('app-main-content');
  if (panel)   panel.classList.remove('open');
  if (content) content.classList.remove('panel-open');
  // Does NOT fire close_detail_panel input — active_construct stays set
}

// ── Nav view switching ────────────────────────────────────────
function switchView(view) {
  var vC = document.getElementById('view-constructs');
  var vS = document.getElementById('view-search');
  var bC = document.getElementById('btn-nav-constructs');
  var bS = document.getElementById('btn-nav-search');
  if (!vC || !vS) return;
  if (view === 'constructs') {
    vC.classList.remove('d-none');
    vS.classList.add('d-none');
    if (bC) { bC.classList.add('active'); }
    if (bS) { bS.classList.remove('active'); }
  } else {
    vS.classList.remove('d-none');
    vC.classList.add('d-none');
    if (bS) { bS.classList.add('active'); }
    if (bC) { bC.classList.remove('active'); }
  }
}

// ── Pending queue ─────────────────────────────────────────────
function updatePendingQueue(n) {
  var q = document.getElementById('pending-queue');
  if (q) q.classList.toggle('pq-open', n > 0);
}

// ── Detail panel drag-resize ──────────────────────────────────
(function() {
  var resizing = false, startY = 0, startH = 0;
  document.addEventListener('mousedown', function(e) {
    if (e.target && e.target.classList.contains('dp-handle')) {
      resizing = true;
      startY = e.clientY;
      var p = document.getElementById('detail-panel');
      startH = p ? p.offsetHeight : 0;
      e.preventDefault();
    }
  });
  document.addEventListener('mousemove', function(e) {
    if (!resizing) return;
    var p = document.getElementById('detail-panel');
    var c = document.getElementById('app-main-content');
    if (!p) return;
    var delta = startY - e.clientY;
    var newH  = Math.max(180, Math.min(window.innerHeight - 80, startH + delta));
    p.style.height = newH + 'px';
    if (c) c.style.paddingBottom = newH + 'px';
  });
  document.addEventListener('mouseup', function() { resizing = false; });
})();

// ── Escape to close detail panel ─────────────────────────────
document.addEventListener('keydown', function(e) {
  if (e.key === 'Escape') closeDetailPanel();
});

// ── Inline name / label editing in construct rows ─────────────
document.addEventListener('dblclick', function(e) {
  var el = e.target;
  if (!el.classList.contains('cr-name') && !el.classList.contains('cr-label')) return;
  var field = el.dataset.field;
  var hn    = el.dataset.hn;
  var orig  = el.textContent;

  el.setAttribute('contenteditable', 'true');
  el.focus();
  var r = document.createRange();
  r.selectNodeContents(el);
  var sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(r);

  function finish(save) {
    el.setAttribute('contenteditable', 'false');
    var val = el.textContent.trim();
    if (save && val && val !== orig) {
      Shiny.setInputValue('inline_edit', {hn: hn, field: field, value: val}, {priority: 'event'});
    } else {
      el.textContent = orig;
    }
  }
  el.addEventListener('blur',    function()  { finish(true);  }, {once: true});
  el.addEventListener('keydown', function(ev) {
    if (ev.key === 'Enter')  { ev.preventDefault(); finish(true);  el.blur(); }
    if (ev.key === 'Escape') { finish(false); el.blur(); }
  }, {once: false});
});

// ── Variable card expand / collapse + lazy label loading ─────
document.addEventListener('click', function(e) {
  var card = e.target.closest('.var-card');
  if (!card) return;
  // Don't toggle if clicking a button inside the card
  if (e.target.closest('button') || e.target.closest('a')) return;
  var body = card.querySelector('.vc-expand-body');
  if (!body) return;
  var isOpen = body.style.display !== 'none' && body.style.display !== '';
  body.style.display = isOpen ? 'none' : 'block';
  card.classList.toggle('vc-exp', !isOpen);
  // On first expand, trigger lazy load of value labels via Shiny
  if (!isOpen) {
    var ph = body.querySelector('.vc-expand-placeholder');
    if (ph && !ph.dataset.loaded) {
      ph.dataset.loaded = 'true';
      var ds   = ph.dataset.ds;
      var lset = ph.dataset.lset;
      if (!ds || !lset) {
        ph.innerHTML = '<em class="small text-muted">No response codes</em>';
      } else {
        ph.innerHTML = '<em class="small text-muted">Loading…</em>';
        Shiny.setInputValue('load_value_labels',
          {ds: ds, lset: lset, card_id: card.id},
          {priority: 'event'});
      }
    }
  }
});

// ── Shiny custom message handlers ────────────────────────────
Shiny.addCustomMessageHandler('openDetailPanel',   function(_) { openDetailPanel(); });
Shiny.addCustomMessageHandler('closeDetailPanel',  function(_) { closeDetailPanel(); });
Shiny.addCustomMessageHandler('hideDetailPanel',   function(_) { hideDetailPanel(); });
Shiny.addCustomMessageHandler('switchView',        function(v) { switchView(v); });
Shiny.addCustomMessageHandler('updatePendingQueue',function(n) { updatePendingQueue(n); });
Shiny.addCustomMessageHandler('scrollToTop',       function(_) { window.scrollTo(0, 0); });
Shiny.addCustomMessageHandler('scrollToConstruct', function(hn) {
  var el = document.getElementById('cr-' + hn);
  if (el) {
    el.scrollIntoView({behavior: 'smooth', block: 'center'});
    el.style.outline = '2px solid #0d6efd';
    el.style.outlineOffset = '2px';
    setTimeout(function() { el.style.outline = ''; el.style.outlineOffset = ''; }, 2500);
  }
});
// FIX 5: atomic view switch — sets dataset, pre-fills search hint, switches view, hides panel
Shiny.addCustomMessageHandler('openSearchForAssignment', function(msg) {

  // Step 1: Switch view first so elements are visible before we touch them
  switchView('search');

  // Step 2: Hide the detail panel without clearing active construct
  hideDetailPanel();

  // Step 3: Set dataset dropdown
  // Use a short delay to ensure the renderUI search_ds_ui has rendered
  // into the DOM (it uses outputOptions suspendWhenHidden=FALSE but
  // Select2 binding may not be ready if view was previously hidden)
  setTimeout(function() {
    var dsEl = document.getElementById('search_ds');
    if (dsEl) {
      dsEl.value = msg.dataset;
      // Try Select2 first, fall back to native Shiny binding
      if (typeof $ !== 'undefined' && $(dsEl).data('select2')) {
        $(dsEl).val(msg.dataset).trigger('change');
      } else {
        $(dsEl).val(msg.dataset).trigger('change.select2').trigger('change');
      }
      Shiny.setInputValue('search_ds', msg.dataset);
    }

    // Step 4: Set search hint in the query box
    var qEl = document.getElementById('search_q');
    if (qEl) {
      var hint = (msg.search_hint && msg.search_hint.length > 0)
                 ? msg.search_hint : '';
      qEl.value = hint;
      // Trigger Shiny's input event (not change — textInput uses input)
      $(qEl).trigger('input');
    }
  }, 50);
});
// FIX 3: inject lazily loaded value label HTML into the placeholder div
Shiny.addCustomMessageHandler('injectLabelHTML', function(msg) {
  var card = document.getElementById(msg.card_id);
  if (!card) return;
  var ph = card.querySelector('.vc-expand-placeholder');
  if (ph) ph.innerHTML = msg.html;
});
