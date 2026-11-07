// stamp.js — an "Updated HH:MM" for Puppet-rendered e-ink cards that only
// moves when the rest of the screen does.
//
// The panels fetch their images with ETags, so an unchanged render costs
// nothing (Puppet answers 304 and the panel doesn't redraw); a clock time in
// the render would change the image on every fetch. Instead the stamp is the
// time the card's content last changed: the card's markup (with the stamp
// blanked) is hashed, and the hash and the time it first appeared are kept
// per page in localStorage, so the time moves only together with something
// else on screen. When fetching breaks, the stamp visibly stops moving.
//
// A card's first renders after (re)creation come before its forecasts and
// calendar alerts arrive; Puppet recreates the card on most requests, so those
// partial renders must not count: until the card says it's ready, the stored
// time is shown and nothing is recorded.

const memory = new Map(); // fallback when localStorage is unavailable

// FNV-1a, 32-bit: enough to tell one render from the next.
const fnv = (s) => {
  let h = 0x811c9dc5;
  for (let i = 0; i < s.length; i++) h = Math.imul(h ^ s.charCodeAt(i), 0x01000193);
  return (h >>> 0).toString(16);
};

// The time (ms) root's content last changed, for this page (key), or null
// before anything was recorded. stampEl, inside root, is emptied first so the
// stamp never counts as content; the caller writes the time back into it.
// ready: whether the card's data has all arrived (see above).
export function contentStamp(key, root, stampEl, ready = true, now = Date.now()) {
  const k = `eink-updated:${key}`;
  let saved = memory.get(k) ?? null;
  try {
    saved = JSON.parse(localStorage.getItem(k) || "null") ?? saved;
  } catch (_) {
    // Blocked storage: the in-memory copy holds until a reload.
  }
  if (!ready) return saved?.at ?? null;
  const shown = stampEl.textContent;
  stampEl.textContent = "";
  const sig = fnv(root.innerHTML);
  stampEl.textContent = shown;
  if (!saved || saved.sig !== sig || !saved.at) {
    saved = { sig, at: now };
    memory.set(k, saved);
    try {
      localStorage.setItem(k, JSON.stringify(saved));
    } catch (_) {
      // As above.
    }
  }
  return saved.at;
}
