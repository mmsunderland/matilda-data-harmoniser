// Remove-from-plan button handler.
// Uses event delegation so it works after DT re-renders the table.
$(document).on('click', '.rm-plan-btn', function () {
  Shiny.setInputValue('remove_plan_var', {
    ds: $(this).data('ds'),
    vn: $(this).data('vn')
  }, { priority: 'event' });
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
