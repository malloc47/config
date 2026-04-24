// ==UserScript==
// @name         Ctrl+J -> newline
// @namespace    https://github.com/malloc47/config
// @version      1.0
// @description  Bind Ctrl+J to insert a newline in textareas and contenteditable elements, matching Emacs/Ghostty behavior. Install via Tampermonkey.
// @match        *://*/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

// Paired with ~/Library/KeyBindings/DefaultKeyBinding.dict on macOS, which
// handles the same binding for native Cocoa text fields. This script covers
// Chrome / web pages, where the Cocoa text binding system does not apply.
//
// Known gaps:
//   - Rich editors (Google Docs, Notion, CodeMirror, Monaco) install their own
//     capture-phase handlers and/or treat the editor as a canvas; Ctrl+J may be
//     swallowed or produce no visible change. Per-site handlers would be needed.
//   - React-controlled inputs that read from internal state (not .value) may
//     need the native setter trick: pass the new value through the property
//     descriptor setter, then dispatch an 'input' event. Add per-site only if
//     you hit a case where the newline appears but the app's model ignores it.

document.addEventListener('keydown', (e) => {
  if (!e.ctrlKey || e.key !== 'j' || e.metaKey || e.altKey) return;
  const el = document.activeElement;
  if (!el) return;

  if (el.tagName === 'TEXTAREA') {
    e.preventDefault();
    e.stopImmediatePropagation();
    const { selectionStart: s, selectionEnd: end, value: v } = el;
    el.value = v.slice(0, s) + '\n' + v.slice(end);
    el.selectionStart = el.selectionEnd = s + 1;
    el.dispatchEvent(new Event('input', { bubbles: true }));
  } else if (el.isContentEditable) {
    e.preventDefault();
    e.stopImmediatePropagation();
    document.execCommand('insertLineBreak');
  }
}, true);
