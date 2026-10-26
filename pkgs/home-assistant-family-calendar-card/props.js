// props.js — holiday and seasonal props for family-calendar-card's sky theme
// (docs/family-calendar-scenes.md, task 2+).
//
// A prop is a small drawing at an anchor that reacts to the scene through the
// shared context: --lit (darkness), --wind, and the weather group on .screen
// (data-wx: rain, pouring, storm, snow, sleet...), plus .flash during
// lightning. Accents say which prop shows when: by date range (MM-DD..MM-DD,
// may wrap the year), by an event on a calendar (match, with days_before /
// days_after), and only after dark if `dark`. Props are not tied to scenes,
// so any of them works in any scene.
//
// Anchors: edge (sitting on the calendar sheet's top edge, in front of it:
// the edge host; params.side picks left or right), ledge (on the same edge
// but behind the header text and the glass: the back host), sky (the sky
// above the sheet, back host). A prop with an `event` appears only now and
// then as a timed event (the Headless Horseman's ride along the ledge), at
// sky_scenes.rates[event].

const mix = (a, b, t) => {
  const p = (h) => [1, 3, 5].map((i) => parseInt(h.slice(i, i + 2), 16));
  const [x, y] = [p(a), p(b)];
  return `rgb(${x.map((v, i) => Math.round(v + (y[i] - v) * t)).join(", ")})`;
};

// A jack-o'-lantern: ribbed pumpkin, curled stem, carved face whose colour
// and glow follow --lit (set from darkness). Optional snow cap.
const pumpkin = (size, lit, delay) => {
  const face = mix("#3b1c08", "#ffcf4a", lit);
  return `<span class="p-pk" style="width: ${size}px; --lit: ${lit.toFixed(2)}; animation-delay: ${delay}s">
    <span class="p-pk-glow"></span>
    <svg viewBox="0 0 64 58" aria-hidden="true">
      <path d="M31 13 C31 8 33 5 37 3" stroke="#4d5a22" stroke-width="3.5" fill="none" stroke-linecap="round"></path>
      <path d="M37 3 C42 1 45 4 43 7" stroke="#6f8a2c" stroke-width="2" fill="none" stroke-linecap="round"></path>
      <ellipse cx="14" cy="36" rx="10" ry="17" fill="#c85a10"></ellipse>
      <ellipse cx="50" cy="36" rx="10" ry="17" fill="#c85a10"></ellipse>
      <ellipse cx="21" cy="35" rx="14" ry="20" fill="#e2701b"></ellipse>
      <ellipse cx="43" cy="35" rx="14" ry="20" fill="#e2701b"></ellipse>
      <ellipse cx="32" cy="35" rx="13" ry="21.5" fill="#f08a2a"></ellipse>
      <path class="p-pk-face" fill="${face}" d="M18 30 l6-8 l6 8z M34 30 l6-8 l6 8z M29 36 l3-4 l3 4z
        M14 41 q18 13 36 0 l-4 1 l-2 4 l-3-3 l-4 4 l-3-4 l-4 4 l-3-4 l-2 3 z"></path>
      <ellipse class="p-cap" cx="32" cy="15.5" rx="19" ry="4.5" fill="#f4f7fb"></ellipse>
    </svg>
  </span>`;
};

export const PROPS = {
  "jack-o-lantern": {
    css: `
      .p-pumpkins { position: absolute; bottom: -4px; display: flex; align-items: flex-end; gap: 2px; }
      .p-pk { position: relative; display: block; }
      .p-pk svg { position: relative; display: block; width: 100%; filter: drop-shadow(0 2px 2px rgba(0, 0, 0, 0.35)); }
      .p-pk-glow { position: absolute; left: -40%; right: -40%; top: -10%; bottom: -30%; border-radius: 50%;
        background: radial-gradient(closest-side, rgba(255, 170, 60, calc(var(--lit) * 0.55)), transparent);
        animation: p-flicker var(--flick, 2.4s) ease-in-out infinite alternate; animation-delay: inherit; }
      .p-pk-face { animation: p-face var(--flick, 2.4s) steps(6) infinite alternate; animation-delay: inherit; }
      .p-cap { display: none; }
      .screen[data-wx="snow"] .p-cap, .screen[data-wx="sleet"] .p-cap { display: inline; }
      .screen[data-wx="rain"] .p-pk-face, .screen[data-wx="pouring"] .p-pk-face, .screen[data-wx="storm"] .p-pk-face,
      .screen[data-wx="rain"] .p-pk-glow, .screen[data-wx="pouring"] .p-pk-glow, .screen[data-wx="storm"] .p-pk-glow { animation-name: p-sputter; }
      @keyframes p-flicker { 0% { opacity: 0.75; transform: scale(0.96); } 50% { opacity: 1; } 100% { opacity: 0.85; transform: scale(1.04); } }
      @keyframes p-face { 0% { opacity: 0.82; } 40% { opacity: 1; } 70% { opacity: 0.9; } 100% { opacity: 1; } }
      @keyframes p-sputter { 0%, 100% { opacity: 1; } 18% { opacity: 0.35; } 22% { opacity: 0.9; } 61% { opacity: 0.55; } 64% { opacity: 1; } }`,
    render: (ctx, p) => {
      const lit = Math.max(0.15, ctx.darkness);
      const sizes = (p.sizes || [52, 36]).slice(0, p.count || 2);
      return `<div class="p-pumpkins" style="${p.side === "right" ? "right" : "left"}: ${p.inset ?? 16}px">${sizes
        .map((s, i) => pumpkin(s, lit, -i * 0.7))
        .join("")}</div>`;
    },
  },

  crow: {
    css: `
      .p-crow { position: absolute; bottom: -2px; width: 46px; color: #16141c; }
      .p-crow svg { display: block; width: 100%; filter: drop-shadow(0 1px 1px rgba(0, 0, 0, 0.3)); }
      .p-crow-head { transform-box: view-box; transform-origin: 13px 13px; animation: p-crow-look 11s ease-in-out infinite; }
      .screen.dark .p-crow svg { filter: drop-shadow(0 0 1.5px rgba(200, 210, 255, 0.45)); }
      @keyframes p-crow-look { 0%, 62%, 100% { transform: rotate(0); } 66%, 80% { transform: rotate(-16deg); } 84% { transform: rotate(8deg); } }`,
    render: (ctx, p) => `<div class="p-crow" style="${p.side === "left" ? "left" : "right"}: ${p.inset ?? 22}px">
      <svg viewBox="0 0 44 40" aria-hidden="true" fill="currentColor">
        <path d="M14 18 C18 13 26 12 31 15 C36 18 39 23 43 29 L36 27 C33 31 26 33 19 31 C15 29 13 24 14 18 Z"></path>
        <g class="p-crow-head"><path d="M9 14 C9 9 14 7 18 9 C20 11 20 15 18 18 L13 19 Z"></path><path d="M9 12 L2 14.5 L9 15.5 Z"></path>
        <circle cx="12" cy="12" r="1.1" fill="#d8c25a"></circle></g>
        <path d="M21 31 L20 38 M25 31 L25 38" stroke="currentColor" stroke-width="1.6" fill="none"></path>
        <path d="M17 38 L23 38 M22 38 L28 38" stroke="currentColor" stroke-width="1.4"></path>
      </svg></div>`,
  },

  bats: {
    css: `
      .p-bats { position: absolute; left: 0; right: 0; top: 0; height: 210px; overflow: hidden; color: #2b2338; }
      .p-bat { position: absolute; left: -40px; width: var(--s); animation: p-bat-fly var(--dur) linear infinite; animation-delay: var(--d); }
      .p-bat svg { display: block; width: 100%; animation: p-bat-bob 1.7s ease-in-out infinite alternate; }
      .p-bat path { transform-box: fill-box; transform-origin: 50% 40%; animation: p-bat-flap 0.26s ease-in-out infinite alternate; }
      .screen.dark .p-bat { filter: drop-shadow(0 0 1.5px rgba(215, 205, 255, 0.75)); }
      @keyframes p-bat-fly { from { transform: translate(0, var(--y)); } to { transform: translate(800px, calc(var(--y) - 40px)); } }
      @keyframes p-bat-bob { from { transform: translateY(-6px); } to { transform: translateY(8px); } }
      @keyframes p-bat-flap { from { transform: scaleY(1); } to { transform: scaleY(0.35); } }`,
    render: (ctx, p) => {
      const bats = [
        [22, 120, 19, 0],
        [16, 70, 23, -7],
        [26, 160, 17, -12],
        [14, 40, 27, -3],
      ].slice(0, p.count || 4);
      return `<div class="p-bats">${bats
        .map(
          ([s, y, dur, d]) => `<span class="p-bat" style="--s: ${s}px; --y: ${y}px; --dur: ${dur}s; --d: ${d}s">
            <svg viewBox="0 0 32 14" aria-hidden="true" fill="currentColor"><path d="M16 4 C16.6 2.6 17.6 2.6 18 4 C20.5 2.5 24 0.8 30 1.5
              C28.5 3 28.5 5 29.5 7 C27 6 25 6.5 24.5 9 C22.5 7.5 20 8 19 11 C18 10 17 10 16 12 C15 10 14 10 13 11 C12 8 9.5 7.5 7.5 9
              C7 6.5 5 6 2.5 7 C3.5 5 3.5 3 2 1.5 C8 0.8 11.5 2.5 14 4 C14.4 2.6 15.4 2.6 16 4 Z"></path></svg></span>`,
        )
        .join("")}</div>`;
    },
  },

  ghost: {
    css: `
      .p-ghost { position: absolute; width: 38px; opacity: 0.72; animation: p-ghost-drift 9s ease-in-out infinite alternate; }
      .p-ghost svg { display: block; width: 100%; filter: drop-shadow(0 0 6px rgba(220, 230, 255, 0.55)); animation: p-ghost-bob 3.2s ease-in-out infinite alternate; }
      @keyframes p-ghost-drift { from { transform: translateX(0) rotate(-4deg); } to { transform: translateX(46px) rotate(5deg); } }
      @keyframes p-ghost-bob { from { transform: translateY(-5px); } to { transform: translateY(7px); } }`,
    render: (ctx, p) => `<div class="p-ghost" style="left: ${p.x ?? 262}px; top: ${p.y ?? 118}px">
      <svg viewBox="0 0 40 48" aria-hidden="true"><path fill="#f3f5ff" d="M20 2 C31 2 36 11 36 22 L36 43 L31 38.5 L26 44 L20 38.5 L14 44 L9 38.5 L4 43 L4 22 C4 11 9 2 20 2 Z"></path>
      <ellipse cx="14.5" cy="20" rx="2.6" ry="3.6" fill="#1d1b2a"></ellipse><ellipse cx="25.5" cy="20" rx="2.6" ry="3.6" fill="#1d1b2a"></ellipse>
      <ellipse cx="20" cy="29" rx="2.4" ry="3" fill="#1d1b2a"></ellipse></svg></div>`,
  },

  // Sleepy Hollow's Headless Horseman: galloping silhouette, cape streaming,
  // a jack-o'-lantern head held high (lit by darkness). Shown only as an
  // event: a ride along the calendar's top edge.
  horseman: {
    css: `
      .p-ledge { position: absolute; left: 12px; right: 12px; top: var(--edge-y, 0); height: 0; }
      .p-hh { position: absolute; bottom: -3px; left: -150px; width: 116px; color: #120f18;
        animation: p-hh-ride var(--ride, 11s) linear forwards; }
      .p-hh svg { display: block; width: 100%; overflow: visible; animation: p-hh-bob 0.42s ease-in-out infinite alternate;
        filter: drop-shadow(0 2px 2px rgba(0, 0, 0, 0.35)); }
      .screen.dark .p-hh { color: #2c2437; }
      .screen.dark .p-hh svg { filter: drop-shadow(0 0 1.5px rgba(255, 190, 130, 0.9)) drop-shadow(0 0 6px rgba(255, 150, 70, 0.35)); }
      .p-hh .leg { transform-box: view-box; animation: p-hh-gallop 0.42s ease-in-out infinite alternate; }
      .p-hh .l1 { transform-origin: 82px 50px; } .p-hh .l2 { transform-origin: 76px 52px; animation-direction: alternate-reverse; }
      .p-hh .l3 { transform-origin: 36px 50px; animation-direction: alternate-reverse; } .p-hh .l4 { transform-origin: 42px 52px; }
      .p-hh .cape { transform-box: view-box; transform-origin: 60px 20px; animation: p-hh-cape 0.3s ease-in-out infinite alternate; }
      .p-hh-head { animation: p-flicker 0.6s ease-in-out infinite alternate; }
      @keyframes p-hh-ride { from { left: -150px; } to { left: 100%; } }
      @keyframes p-hh-bob { from { transform: translateY(0); } to { transform: translateY(-4px); } }
      @keyframes p-hh-gallop { from { transform: rotate(-22deg); } to { transform: rotate(24deg); } }
      @keyframes p-hh-cape { from { transform: skewY(-4deg) scaleX(0.96); } to { transform: skewY(5deg) scaleX(1.04); } }`,
    render: (ctx) => {
      const lit = Math.max(0.35, ctx.darkness);
      const face = mix("#5a2a08", "#ffd25a", lit);
      return `<div class="p-hh"><svg viewBox="0 0 140 84" aria-hidden="true" fill="currentColor">
        <path class="cape" d="M58 20 C44 16 30 24 12 18 C20 26 18 32 8 36 C26 38 42 34 58 33 Z"></path>
        <path d="M34 45 C26 42 16 44 8 54 C17 49 24 50 31 53 Z"></path>
        <ellipse cx="60" cy="48" rx="28" ry="11.5"></ellipse>
        <path d="M80 43 C86 36 92 28 98 23 L108 25 L112 32 L105 33 L99 31 C95 38 90 46 86 52 Z"></path>
        <path d="M98 23 L100 16 L103 22 Z"></path>
        <g class="leg l1"><path d="M80 50 L91 61 L98 67 L95 70 L88 64 L76 54 Z"></path></g>
        <g class="leg l2"><path d="M73 53 L76 66 L74 76 L70.5 76 L71.5 66 L69 55 Z"></path></g>
        <g class="leg l3"><path d="M38 50 L30 61 L22 68 L19.5 65.5 L27 58 L33 48 Z"></path></g>
        <g class="leg l4"><path d="M45 54 L45 67 L48 76 L44.5 77 L41 67 L40.5 55 Z"></path></g>
        <path d="M54 37 L58 18 L67 18 L69 37 Z"></path>
        <path d="M57 37 L50 47 L54 48.5 L61 39 Z"></path>
        <path d="M65 21 L83 10 L85 13.5 L68 26 Z"></path>
        <g class="p-hh-head"><circle cx="88" cy="8" r="7.5" fill="#e8741d"></circle>
          <path fill="${face}" d="M84.5 7 l2-2.6 l2 2.6z M89.5 7 l2-2.6 l2 2.6z M84 10.5 q4 3.2 8 0 l-1.4 0.4 l-1.2 1.4 l-1.4-1.2 l-1.4 1.2 l-1.2-1.4 z"></path></g>
      </svg></div>`;
    },
  },
};

export const PROP_CSS = Object.values(PROPS)
  .map((p) => p.css || "")
  .join("\n");

// The built-in accents, by id: prop, when, place (anchor), params, event.
const HALLOWEEN = "10-24..10-31";
export const ACCENTS = {
  "halloween-pumpkins": { prop: "jack-o-lantern", when: { dates: HALLOWEEN }, place: "edge", params: { side: "left", inset: 18 } },
  "halloween-crow": { prop: "crow", when: { dates: HALLOWEEN }, place: "edge", params: { side: "right", inset: 26 } },
  "halloween-bats": { prop: "bats", when: { dates: HALLOWEEN, dark: true }, place: "sky" },
  "halloween-ghost": { prop: "ghost", when: { dates: "10-29..10-31", dark: true }, place: "sky" },
  "halloween-horseman": { prop: "horseman", when: { dates: HALLOWEEN }, place: "edge", event: "horseman" },
};
export const PROP_RATES = { horseman: [900, 1800] };

const md = (date) => date.slice(5); // "MM-DD"
const inRange = (date, range) => {
  const [a, b] = String(range).split("..");
  const d = md(date);
  return a <= b ? d >= a && d <= (b || a) : d >= a || d <= b;
};
const addDays = (date, n) => {
  const d = new Date(`${date}T12:00:00Z`);
  d.setUTCDate(d.getUTCDate() + n);
  return d.toISOString().slice(0, 10);
};

// The accents active now. cfg is the card's sky_accents: `disable` (ids),
// `define` (extra or replacement accents by id). eventsOn(calendar) lists
// that calendar's fetched events ({summary, startDay}).
export function activeAccents(ctx, cfg = {}, eventsOn = () => []) {
  const all = { ...ACCENTS, ...(cfg.define || {}) };
  const disabled = new Set(cfg.disable || []);
  return Object.entries(all)
    .filter(([id, a]) => !disabled.has(id) && PROPS[a.prop])
    .filter(([, a]) => {
      const w = a.when || {};
      if (w.dark && ctx.darkness < 0.5) return false;
      if (w.dates && ![].concat(w.dates).some((r) => inRange(ctx.date, r))) return false;
      if (w.calendar) {
        const re = new RegExp(w.match ?? "", "i");
        const from = addDays(ctx.date, -(w.days_after ?? 0));
        const to = addDays(ctx.date, w.days_before ?? 0);
        if (!eventsOn(w.calendar).some((e) => re.test(e.summary) && e.startDay >= from && e.startDay <= to)) return false;
      }
      return true;
    })
    .map(([id, a]) => ({ id, ...a }));
}

// Accent markup per host; props with an event are left to the scheduler.
export function renderAccents(accents, ctx) {
  const out = { back: "", edge: "", front: "" };
  for (const a of accents) {
    if (a.event) continue;
    const html = PROPS[a.prop].render(ctx, a.params || {});
    if (a.place === "ledge") out.back += `<div class="p-ledge">${html}</div>`;
    else out[a.place === "sky" ? "back" : a.place === "front" ? "front" : "edge"] += html;
  }
  return out;
}

// Timed prop appearances, fired like scene events: add the prop to its host
// and say when to remove it.
export const PROP_EVENTS = {
  horseman: {
    fire(hosts, ctx) {
      const node = document.createElement("div");
      node.className = "p-ledge";
      node.innerHTML = PROPS.horseman.render(ctx, {});
      hosts.back.append(node);
      return [[node], 12000];
    },
  },
};
