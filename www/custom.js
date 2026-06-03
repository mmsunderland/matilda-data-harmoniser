// Reset domain tree selection when dataset changes.
Shiny.addCustomMessageHandler('resetTreeSelect', function(_) {
  Shiny.setInputValue('tree_select', {domain: '', subdomain: '', nonce: Math.random()});
});

// Copy generated code to clipboard.
function copyGeneratedCode() {
  var text = document.getElementById('generated_code').innerText;
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text).then(function () {
      setCopySuccess();
    }).catch(function () {
      fallbackCopy(text);
    });
  } else {
    fallbackCopy(text);
  }
}

function fallbackCopy(text) {
  var ta = document.createElement('textarea');
  ta.value = text;
  ta.style.position = 'fixed';
  ta.style.opacity  = '0';
  document.body.appendChild(ta);
  ta.focus();
  ta.select();
  try { document.execCommand('copy'); setCopySuccess(); } catch (e) {}
  document.body.removeChild(ta);
}

function setCopySuccess() {
  var btn = document.getElementById('copy_code_btn');
  if (!btn) return;
  var orig = btn.innerHTML;
  btn.innerHTML = '&#10003; Copied!';
  btn.disabled  = true;
  setTimeout(function () {
    btn.innerHTML = orig;
    btn.disabled  = false;
  }, 2000);
}

// ---------------------------------------------------------------------------
// Plan table — checkbox selection
// ---------------------------------------------------------------------------

var planSelectedIds = [];

// Re-apply checkbox states after DT draws (called from drawCallback)
function planTableDrawn() {
  $('#plan_table .plan-row-cb').each(function () {
    var rid = parseInt($(this).data('rid'));
    $(this).prop('checked', planSelectedIds.indexOf(rid) >= 0);
  });
  _updatePlanSelectAllState();
}

// Checkbox click handler (event delegation — survives DT re-renders)
$(document).on('change', '#plan_table .plan-row-cb', function () {
  var rid = parseInt($(this).data('rid'));
  if (this.checked) {
    if (planSelectedIds.indexOf(rid) < 0) planSelectedIds.push(rid);
  } else {
    planSelectedIds = planSelectedIds.filter(function (id) { return id !== rid; });
  }
  Shiny.setInputValue('plan_selected_ids', planSelectedIds, { priority: 'event' });
  _updatePlanSelectAllState();
});

// Called by the select-all checkbox in the toolbar
function planSelectAll(checked) {
  if (checked) {
    planSelectedIds = [];
    $('#plan_table .plan-row-cb').each(function () {
      planSelectedIds.push(parseInt($(this).data('rid')));
    });
  } else {
    planSelectedIds = [];
  }
  $('#plan_table .plan-row-cb').prop('checked', checked);
  Shiny.setInputValue('plan_selected_ids', planSelectedIds, { priority: 'event' });
  _updatePlanSelectAllState();
}

function _updatePlanSelectAllState() {
  var total   = $('#plan_table .plan-row-cb').length;
  var checked = planSelectedIds.length;
  var allCb   = document.getElementById('plan_select_all_cb');
  if (!allCb) return;
  allCb.indeterminate = checked > 0 && checked < total;
  allCb.checked       = total > 0 && checked === total;
}

// Server → client: force-update selection (e.g. deselect all)
Shiny.addCustomMessageHandler('setPlanSelection', function (ids) {
  planSelectedIds = ids.map(function (x) { return parseInt(x); });
  planTableDrawn();
  Shiny.setInputValue('plan_selected_ids', planSelectedIds, { priority: 'event' });
});

// Recode and remove button handlers (event delegation)
$(document).on('click', '.plan-recode-btn', function () {
  Shiny.setInputValue('plan_recode_click', $(this).data('key'), { priority: 'event' });
});

$(document).on('click', '.plan-remove-btn', function () {
  Shiny.setInputValue('plan_remove_click', $(this).data('key'), { priority: 'event' });
});
