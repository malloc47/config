// props.js — holiday and seasonal props for family-calendar-card's sky theme
// (docs/family-calendar-scenes.md, task 2+).
//
// A prop is a small drawing at an anchor that reacts to the scene through the
// shared context: --lit (darkness), --wind, and the weather group on .screen
// (data-wx: rain, pouring, storm, snow, sleet...), plus .flash during
// lightning. Accents say which prop shows when: by date range (MM-DD..MM-DD,
// may wrap the year), by a floating date (nth_weekday: [month, weekday 0-6,
// n], with days_before / days_after), by an event on a calendar (match, with
// days_before / days_after), and only after dark if `dark`. Props are not tied to scenes,
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
  // Thanksgiving: a turkey strutting in place on the edge, tail fanned,
  // head bobbing now and then.
  turkey: {
    css: `
      .p-turkey { position: absolute; bottom: -3px; width: 48px; }
      .p-turkey svg { display: block; width: 100%; overflow: visible; filter: drop-shadow(0 2px 2px rgba(0, 0, 0, 0.3)); }
      .p-turkey .tail { transform-box: view-box; transform-origin: 30px 38px; animation: p-tk-fan 7s ease-in-out infinite; }
      .p-turkey .head { transform-box: view-box; transform-origin: 40px 34px; animation: p-tk-bob 4.5s ease-in-out infinite; }
      .screen.dark .p-turkey svg { filter: drop-shadow(0 0 1.5px rgba(255, 210, 160, 0.6)); }
      @keyframes p-tk-fan { 0%, 70%, 100% { transform: scale(1); } 76%, 90% { transform: scale(1.07) rotate(-2deg); } }
      @keyframes p-tk-bob { 0%, 40%, 100% { transform: rotate(0); } 45% { transform: translate(3px, 3px) rotate(14deg); } 50% { transform: rotate(0); }
        55% { transform: translate(3px, 3px) rotate(14deg); } 60% { transform: rotate(0); } }`,
    render: (ctx, p) => {
      const feathers = [-70, -48, -25, 0, 25, 48, 70]
        .map((a, i) => `<ellipse cx="30" cy="16" rx="6" ry="17" fill="${["#8a4b1f", "#c26a27", "#e0a040", "#b5521f", "#e0a040", "#c26a27", "#8a4b1f"][i]}"
          stroke="#5b2f12" stroke-width="0.6" transform="rotate(${a} 30 38)"></ellipse>`)
        .join("");
      return `<div class="p-turkey" style="${p.side === "right" ? "right" : "left"}: ${p.inset ?? 18}px">
        <svg viewBox="0 0 60 60" aria-hidden="true">
          <g class="tail">${feathers}<ellipse cx="30" cy="38" rx="9" ry="9" fill="#f2c45a" opacity="0.5"></ellipse></g>
          <path d="M26 51 L25 58 M33 51 L34 58 M22 58 h6 M31 58 h6" stroke="#d98a2a" stroke-width="1.8" fill="none" stroke-linecap="round"></path>
          <ellipse cx="30" cy="42" rx="13" ry="11" fill="#6b3a1a"></ellipse>
          <path d="M22 40 q8 7 16 0" stroke="#8a5428" stroke-width="2" fill="none"></path>
          <g class="head"><path d="M36 36 C38 30 40 26 42 24" stroke="#6b3a1a" stroke-width="5" fill="none" stroke-linecap="round"></path>
            <circle cx="43" cy="23" r="4.2" fill="#c9b6a0"></circle><circle cx="44.3" cy="22" r="0.9" fill="#1b1410"></circle>
            <path d="M46.8 22.6 L50 24 L46.6 25 Z" fill="#e9a52b"></path><path d="M45.6 25 C47 28 46 31 44.5 31 C44 29 44.3 27 44.6 25.5 Z" fill="#c8312b"></path></g>
        </svg></div>`;
    },
  },

  // A pile of harvest gourds and small pumpkins (uncarved).
  gourds: {
    css: `
      .p-gourds { position: absolute; bottom: -4px; display: flex; align-items: flex-end; gap: 0; }
      .p-gourds svg { display: block; filter: drop-shadow(0 2px 2px rgba(0, 0, 0, 0.3)); margin-left: -6px; }`,
    render: (ctx, p) => {
      const g = (w, c1, c2, stem) => `<svg width="${w}" viewBox="0 0 40 36" aria-hidden="true">
        <path d="M20 9 C20 5 22 3 24 2" stroke="${stem}" stroke-width="2.5" fill="none" stroke-linecap="round"></path>
        <ellipse cx="11" cy="23" rx="8" ry="11" fill="${c2}"></ellipse><ellipse cx="29" cy="23" rx="8" ry="11" fill="${c2}"></ellipse>
        <ellipse cx="20" cy="22.5" rx="10" ry="13" fill="${c1}"></ellipse></svg>`;
      return `<div class="p-gourds" style="${p.side === "left" ? "left" : "right"}: ${p.inset ?? 22}px">${[
        g(34, "#f2e6c9", "#dccfae", "#5d6a2a"),
        g(46, "#f08a2a", "#c85a10", "#4d5a22"),
        g(26, "#6f8f3a", "#4f6a26", "#3f4a1c"),
      ].join("")}</div>`;
    },
  },

  // String lights draped along the calendar sheet's top edge: scallops of
  // wire with coloured bulbs twinkling, glowing brighter after dark; snow
  // caps the bulbs when it snows. params: colors, bulbs (per scallop), sag.
  "string-lights": {
    css: `
      .p-lights { position: absolute; left: 4px; right: 4px; top: -3px; height: 20px; --lit: 0.3; }
      .p-lights svg { position: absolute; inset: 0; width: 100%; height: 100%; overflow: visible; }
      .p-lights path { fill: none; stroke: #2f3a2c; stroke-width: 1.3; vector-effect: non-scaling-stroke; }
      .p-bulb { position: absolute; left: var(--x); top: var(--y); width: 6px; height: 9px; margin-left: -3px; border-radius: 45% 45% 50% 50% / 40% 40% 60% 60%;
        background: var(--c); transform: rotate(var(--r)); transform-origin: 50% 0;
        box-shadow: 0 0 calc(3px + var(--lit) * 9px) calc(var(--lit) * 2px) var(--c); animation: p-twinkle var(--dur) ease-in-out infinite alternate; animation-delay: var(--d); }
      .p-bulb::before { content: ""; position: absolute; left: 1px; right: 1px; top: -2.5px; height: 3px; border-radius: 1px; background: #3d4636; }
      .p-bulb::after { content: ""; position: absolute; left: -1px; right: -1px; top: -3.5px; height: 3px; border-radius: 3px 3px 1px 1px; background: #f6f9fc; display: none; }
      .screen[data-wx="snow"] .p-bulb::after, .screen[data-wx="sleet"] .p-bulb::after { display: block; }
      .screen.gusty .p-lights { animation: p-lights-sway 1.8s ease-in-out infinite alternate; transform-origin: 50% 0; }
      @keyframes p-twinkle { 0% { opacity: 0.55; filter: brightness(0.8); } 60% { opacity: 1; filter: brightness(1.15); } 100% { opacity: 0.85; } }
      @keyframes p-lights-sway { from { transform: skewX(-2deg); } to { transform: skewX(2deg); } }`,
    render: (ctx, p) => {
      const colors = p.colors || ["#ff4d4d", "#ffcc33", "#3ddc84", "#4da3ff", "#ff66cc"];
      const n = p.scallops ?? 7;
      const per = p.bulbs ?? 3;
      const sag = p.sag ?? 10;
      const lit = Math.max(0.25, ctx.darkness);
      let wire = "M0 3";
      for (let k = 0; k < n; k++) wire += ` Q${((k + 0.5) / n) * 100} ${3 + sag * 2} ${((k + 1) / n) * 100} 3`;
      const bulbs = [];
      let i = 0;
      for (let k = 0; k < n; k++) {
        for (let j = 1; j <= per; j++) {
          const t = j / (per + 1);
          const y = 3 + sag * 2 * 2 * t * (1 - t);
          const slope = (1 - 2 * t) * sag * 2;
          bulbs.push(`<span class="p-bulb" style="--x: ${(((k + t) / n) * 100).toFixed(2)}%; --y: ${(y - 0.5).toFixed(1)}px; --c: ${colors[i % colors.length]};
            --r: ${(-slope * 1.5).toFixed(0)}deg; --dur: ${(1.6 + ((i * 7) % 5) * 0.4).toFixed(1)}s; --d: ${(-((i * 3) % 7) * 0.5).toFixed(1)}s"></span>`);
          i++;
        }
      }
      return `<div class="p-lights" style="--lit: ${lit.toFixed(2)}"><svg viewBox="0 0 100 20" preserveAspectRatio="none" aria-hidden="true"><path d="${wire}"></path></svg>${bulbs.join("")}</div>`;
    },
  },

  // A small decorated tree on the edge: tiers, twinkling ornaments, a
  // glowing star; snow on the boughs when it snows.
  tree: {
    css: `
      .p-tree { position: absolute; bottom: -3px; width: 40px; }
      .p-tree svg { display: block; width: 100%; overflow: visible; filter: drop-shadow(0 2px 2px rgba(0, 0, 0, 0.3)); }
      .p-tree .orn { animation: p-twinkle var(--dur) ease-in-out infinite alternate; animation-delay: var(--d); }
      .p-tree .star { transform-box: fill-box; transform-origin: 50% 50%; animation: p-star 3s ease-in-out infinite alternate;
        filter: drop-shadow(0 0 calc(2px + var(--lit) * 6px) rgba(255, 220, 110, 0.95)); }
      .p-tree .snow { display: none; }
      .screen[data-wx="snow"] .p-tree .snow, .screen[data-wx="sleet"] .p-tree .snow { display: inline; }
      .screen.gusty .p-tree svg { transform-origin: 50% 100%; animation: p-sway 2.2s ease-in-out infinite alternate; }
      @keyframes p-star { from { transform: scale(0.92) rotate(-6deg); } to { transform: scale(1.08) rotate(6deg); } }`,
    render: (ctx, p) => {
      const lit = Math.max(0.25, ctx.darkness);
      const orns = [[20, 54, "#ff4d4d"], [32, 50, "#ffcc33"], [25, 42, "#4da3ff"], [18, 36, "#ffcc33"], [30, 32, "#ff66cc"], [23, 24, "#ff4d4d"], [27, 16, "#4da3ff"]]
        .map(([x, y, c], i) => `<circle class="orn" cx="${x}" cy="${y}" r="2" fill="${c}" style="--dur: ${1.5 + (i % 3) * 0.5}s; --d: ${-i * 0.4}s; filter: drop-shadow(0 0 ${(1 + lit * 3).toFixed(1)}px ${c})"></circle>`)
        .join("");
      return `<div class="p-tree" style="${p.side === "right" ? "right" : "left"}: ${p.inset ?? 20}px; --lit: ${lit.toFixed(2)}">
        <svg viewBox="0 0 50 66" aria-hidden="true">
          <rect x="21.5" y="57" width="7" height="8" fill="#6b4423"></rect>
          <path d="M25 30 L44 58 L6 58 Z" fill="#1f6b3a"></path><path d="M25 18 L39 42 L11 42 Z" fill="#26804a"></path>
          <path d="M25 8 L34 28 L16 28 Z" fill="#2e9456"></path>
          <path class="snow" fill="#f4f7fb" d="M8 57 q5 -3 9 0 q4 -3 8 0 q4 -3 8 0 q4 -3 9 0 l0 1 l-34 0 Z M13 41 q4 -2.5 7 0 q3 -2.5 6 0 q3 -2.5 6 0 q3 -2.5 6 0 l0 1 l-25 0 Z M18 27 q3 -2 7 0 q3 -2 7 0 l0 1 l-14 0 Z"></path>
          <path d="M14 51 Q25 56 37 49 M15 37 Q25 41 34 35 M18 23 Q25 26 32 22" stroke="#e8c65a" stroke-width="0.9" fill="none" opacity="0.9"></path>
          ${orns}
          <path class="star" d="M25 0 L26.8 4.6 L31.6 4.8 L27.8 7.8 L29.2 12.4 L25 9.7 L20.8 12.4 L22.2 7.8 L18.4 4.8 L23.2 4.6 Z" fill="#ffd84a"></path>
        </svg></div>`;
    },
  },

  // Fireworks over the sky: bursts of sparks blooming and falling, each on
  // its own loop so they don't repeat in step. params: colors, count.
  fireworks: {
    css: `
      .p-fw-sky { position: absolute; left: 0; right: 0; top: 0; height: var(--edge-y, 220px); overflow: hidden; }
      .p-fw { position: absolute; left: var(--x); top: var(--y); width: 0; height: 0; animation: p-fw-sag var(--dur) linear infinite; animation-delay: var(--d); }
      .p-fw b { position: absolute; left: -1px; width: 2px; height: 26px; border-radius: 1px; background: linear-gradient(180deg, var(--c), transparent);
        opacity: 0; animation: p-fw-rise var(--dur) ease-out infinite; animation-delay: var(--d); }
      .p-fw i { position: absolute; left: 0; top: 0; width: 0; height: 0; transform: rotate(var(--a)); }
      .p-fw i::before { content: ""; position: absolute; left: 0; top: -1.5px; width: 3px; height: 3px; border-radius: 50%; background: var(--c);
        box-shadow: 0 0 4px 1px var(--c); opacity: 0; animation: p-fw-spark var(--dur) cubic-bezier(0.1, 0.7, 0.3, 1) infinite; animation-delay: var(--d); }
      .p-fw::after { content: ""; position: absolute; left: -60px; top: -60px; width: 120px; height: 120px; border-radius: 50%;
        background: radial-gradient(closest-side, var(--c), transparent); opacity: 0; animation: p-fw-glow var(--dur) ease-out infinite; animation-delay: var(--d); }
      @keyframes p-fw-rise { 0% { opacity: 0; transform: translateY(var(--rise)); } 3% { opacity: 0.9; } 22% { opacity: 0.9; transform: translateY(0); } 24%, 100% { opacity: 0; transform: translateY(0); } }
      @keyframes p-fw-spark { 0%, 23% { opacity: 0; transform: translateX(0); } 25% { opacity: 1; } 55% { opacity: 0.85; }
        75% { opacity: 0; transform: translateX(var(--r)); } 100% { opacity: 0; transform: translateX(var(--r)); } }
      @keyframes p-fw-glow { 0%, 23% { opacity: 0; } 26% { opacity: 0.22; } 50%, 100% { opacity: 0; } }
      @keyframes p-fw-sag { 0%, 30% { transform: translateY(0); } 80%, 100% { transform: translateY(14px); } }`,
    render: (ctx, p) => {
      const colors = p.colors || ["#ffd166", "#ff5d73", "#7bdff2", "#c3f584", "#f7f7ff", "#c792ea"];
      const spots = [[14, 40, 7.2, 0], [92, 105, 8.6, -2.3], [52, 28, 9.4, -5.1], [80, 30, 7.9, -3.7], [30, 175, 10.3, -7.4]].slice(0, p.count ?? 4);
      return `<div class="p-fw-sky">${spots
        .map(([x, y, dur, d], k) => {
          const c = colors[k % colors.length];
          const n = 14;
          const sparks = Array.from({ length: n }, (_, j) => `<i style="--a: ${((j / n) * 360 + k * 9).toFixed(0)}deg"></i>`).join("");
          return `<span class="p-fw" style="--x: ${x}%; --y: ${y}px; --c: ${c}; --dur: ${dur}s; --d: ${d}s; --r: ${34 + (k % 3) * 8}px; --rise: ${(210 - y).toFixed(0)}px"><b></b>${sparks}</span>`;
        })
        .join("")}</div>`;
    },
  },

  // Hearts drifting up through the sky, swaying.
  hearts: {
    css: `
      .p-hearts { position: absolute; left: 0; right: 0; top: 0; height: var(--edge-y, 220px); overflow: hidden; }
      .p-heart { position: absolute; left: var(--x); bottom: -30px; width: var(--s); animation: p-heart-rise var(--dur) linear infinite; animation-delay: var(--d); }
      .p-heart svg { display: block; width: 100%; animation: p-heart-sway 3.4s ease-in-out infinite alternate; filter: drop-shadow(0 1px 2px rgba(120, 20, 50, 0.3)); }
      @keyframes p-heart-rise { 0% { transform: translateY(0); opacity: 0; } 10% { opacity: var(--o); } 80% { opacity: var(--o); }
        100% { transform: translateY(calc(-1 * var(--edge-y, 220px) - 20px)); opacity: 0; } }
      @keyframes p-heart-sway { from { transform: translateX(-10px) rotate(-10deg); } to { transform: translateX(10px) rotate(10deg); } }`,
    render: (ctx, p) => {
      const hs = [[8, 18, 17, 0, "#ff5d8f"], [26, 13, 21, -9, "#ff8fab"], [47, 22, 19, -4, "#e63950"], [70, 15, 23, -14, "#ff8fab"], [90, 19, 18, -7, "#ff5d8f"]].slice(0, p.count ?? 5);
      return `<div class="p-hearts">${hs
        .map(([x, s, dur, d, c]) => `<span class="p-heart" style="--x: ${x}%; --s: ${s}px; --dur: ${dur}s; --d: ${d}s; --o: ${ctx.dark ? 0.75 : 0.85}">
          <svg viewBox="0 0 24 22" aria-hidden="true"><path fill="${c}" d="M12 21 C5 15 1 11.5 1 7 C1 3.5 3.7 1 7 1 C9.2 1 11 2.3 12 4 C13 2.3 14.8 1 17 1 C20.3 1 23 3.5 23 7 C23 11.5 19 15 12 21 Z"></path>
          <ellipse cx="7" cy="6" rx="2.4" ry="1.6" fill="#fff" opacity="0.45" transform="rotate(-30 7 6)"></ellipse></svg></span>`)
        .join("")}</div>`;
    },
  },

  // Bunting: pennants on a drooping string along the edge, fluttering
  // (more so in the wind). params: colors, scallops, per.
  bunting: {
    css: `
      .p-bunting { position: absolute; left: 6px; right: 6px; top: -4px; height: 24px; }
      .p-bunting svg { position: absolute; inset: 0; width: 100%; height: 8px; overflow: visible; }
      .p-bunting path { fill: none; stroke: rgba(60, 50, 40, 0.75); stroke-width: 1; vector-effect: non-scaling-stroke; }
      .p-pennant { position: absolute; left: var(--x); top: var(--y); width: 0; height: 0; margin-left: -6px; border-left: 6px solid transparent; border-right: 6px solid transparent;
        border-top: 14px solid var(--c); transform: rotate(var(--r)); transform-origin: 50% 0; filter: drop-shadow(0 1px 1px rgba(0, 0, 0, 0.25));
        animation: p-flutter var(--dur) ease-in-out infinite alternate; animation-delay: var(--d); }
      .screen.gusty .p-pennant { animation-duration: 0.5s; }
      @keyframes p-flutter { from { transform: rotate(calc(var(--r) - 5deg)) scaleY(1); } to { transform: rotate(calc(var(--r) + 5deg)) scaleY(0.92); } }`,
    render: (ctx, p) => {
      const colors = p.colors || ["#c8102e", "#ffffff", "#1f3a93"];
      const n = p.scallops ?? 6;
      const per = p.per ?? 5;
      const sag = 6;
      let wire = "M0 1";
      for (let k = 0; k < n; k++) wire += ` Q${((k + 0.5) / n) * 100} ${1 + sag * 2} ${((k + 1) / n) * 100} 1`;
      const flags = [];
      let i = 0;
      for (let k = 0; k < n; k++) {
        for (let j = 1; j <= per; j++) {
          const t = j / (per + 1);
          const y = 1 + sag * 2 * 2 * t * (1 - t);
          flags.push(`<span class="p-pennant" style="--x: ${(((k + t) / n) * 100).toFixed(2)}%; --y: ${y.toFixed(1)}px; --c: ${colors[i % colors.length]};
            --r: ${(-(1 - 2 * t) * 10).toFixed(0)}deg; --dur: ${(1.4 + (i % 4) * 0.3).toFixed(1)}s; --d: ${(-(i % 5) * 0.3).toFixed(1)}s"></span>`);
          i++;
        }
      }
      return `<div class="p-bunting"><svg viewBox="0 0 100 8" preserveAspectRatio="none" aria-hidden="true"><path d="${wire}"></path></svg>${flags.join("")}</div>`;
    },
  },

  // A flag on a short pole on the edge, waving (harder in the wind).
  // params: bands (colours, vertical bands: Peru's red-white-red by
  // default), side.
  flag: {
    css: `
      .p-flag { position: absolute; bottom: -3px; width: 64px; height: 66px; }
      .p-flag .pole { position: absolute; left: 3px; bottom: 0; width: 3px; height: 64px; border-radius: 2px; background: linear-gradient(90deg, #8a8f99, #e6e9ee, #8a8f99); }
      .p-flag .pole::before { content: ""; position: absolute; left: -2px; top: -5px; width: 7px; height: 7px; border-radius: 50%; background: #d9b44a; }
      .p-flag .cloth { position: absolute; left: 6px; top: 2px; width: 54px; height: 36px; display: flex; transform-origin: 0 50%;
        -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 54 36' preserveAspectRatio='none'%3E%3Cpath d='M0 0 C14 3 28 -3 54 2 L54 34 C28 29 14 35 0 36 Z'/%3E%3C/svg%3E");
        -webkit-mask-size: 100% 100%; animation: p-wave 2.6s ease-in-out infinite alternate; filter: drop-shadow(0 1px 2px rgba(0, 0, 0, 0.3)); }
      .p-flag .cloth span { flex: 1; }
      .p-flag .cloth::after { content: ""; position: absolute; inset: 0;
        background: repeating-linear-gradient(90deg, rgba(0, 0, 0, 0.14) 0, rgba(255, 255, 255, 0.12) 9px, rgba(0, 0, 0, 0.14) 18px);
        background-size: 36px 100%; animation: p-wave-shade 1.6s linear infinite; }
      .screen.gusty .p-flag .cloth { animation-duration: 0.9s; }
      .screen.gusty .p-flag .cloth::after { animation-duration: 0.7s; }
      @keyframes p-wave { from { transform: skewY(2deg) scaleX(0.97); } to { transform: skewY(-3deg) scaleX(1.02); } }
      @keyframes p-wave-shade { to { background-position: 36px 0; } }`,
    render: (ctx, p) => {
      const bands = p.bands || ["#d91023", "#ffffff", "#d91023"];
      return `<div class="p-flag" style="${p.side === "left" ? "left" : "right"}: ${p.inset ?? 20}px">
        <span class="pole"></span><span class="cloth">${bands.map((c) => `<span style="background: ${c}"></span>`).join("")}</span></div>`;
    },
  },
};

// In windy weather (or strong gusts) the sitting props sway.
const SWAY_CSS = `
  .screen.gusty .p-pk svg, .screen.gusty .p-crow svg { transform-origin: 50% 100%; animation: p-sway 1.6s ease-in-out infinite alternate; }
  .screen.gusty .p-pk:nth-child(2) svg { animation-duration: 1.3s; }
  @keyframes p-sway { from { transform: rotate(-3deg); } to { transform: rotate(3.5deg); } }`;

export const PROP_CSS = SWAY_CSS + Object.values(PROPS)
  .map((p) => p.css || "")
  .join("\n");

// The built-in accents, by id: prop, when, place (anchor), params, event.
const HALLOWEEN = "10-24..10-31";
const THANKSGIVING = { nth_weekday: [11, 4, 4], days_before: 3, days_after: 1 }; // 4th Thursday of November
export const ACCENTS = {
  "halloween-pumpkins": { prop: "jack-o-lantern", when: { dates: HALLOWEEN }, place: "edge", params: { side: "left", inset: 18 } },
  "halloween-crow": { prop: "crow", when: { dates: HALLOWEEN }, place: "edge", params: { side: "right", inset: 26 } },
  "halloween-bats": { prop: "bats", when: { dates: HALLOWEEN, dark: true }, place: "sky" },
  "halloween-ghost": { prop: "ghost", when: { dates: "10-29..10-31", dark: true }, place: "sky" },
  "halloween-horseman": { prop: "horseman", when: { dates: HALLOWEEN }, place: "edge", event: "horseman" },
  "thanksgiving-turkey": { prop: "turkey", when: THANKSGIVING, place: "edge", params: { side: "left" } },
  "thanksgiving-gourds": { prop: "gourds", when: THANKSGIVING, place: "edge", params: { side: "right" } },
  "winter-lights": { prop: "string-lights", when: { dates: "12-01..01-06" }, place: "edge" },
  "christmas-tree": { prop: "tree", when: { dates: "12-10..12-26" }, place: "edge", params: { side: "left" } },
  "nye-fireworks": { prop: "fireworks", when: { dates: "12-31..01-01", dark: true }, place: "sky" },
  "valentines-hearts": { prop: "hearts", when: { dates: "02-13..02-14" }, place: "sky" },
  "july4-bunting": { prop: "bunting", when: { dates: "07-01..07-04" }, place: "edge" },
  "july4-fireworks": {
    prop: "fireworks",
    when: { dates: "07-04", dark: true },
    place: "sky",
    params: { colors: ["#ff3b4e", "#f7f7ff", "#4d7cff", "#ff3b4e", "#f7f7ff"] },
  },
  "fiestas-patrias-flag": { prop: "flag", when: { dates: "07-28..07-29" }, place: "edge", params: { side: "right" } },
  "fiestas-patrias-bunting": { prop: "bunting", when: { dates: "07-27..07-29" }, place: "edge", params: { colors: ["#d91023", "#ffffff"] } },
};
export const PROP_RATES = { horseman: [900, 1800] };

const md = (date) => date.slice(5); // "MM-DD"
const inRange = (date, range) => {
  const [a, b = a] = String(range).split("..");
  const d = md(date);
  return a <= b ? d >= a && d <= b : d >= a || d <= b;
};
const addDays = (date, n) => {
  const d = new Date(`${date}T12:00:00Z`);
  d.setUTCDate(d.getUTCDate() + n);
  return d.toISOString().slice(0, 10);
};

// The nth weekday (0 Sunday .. 6 Saturday) of a month, as YYYY-MM-DD.
const nthWeekday = (year, month, weekday, nth) => {
  const first = new Date(Date.UTC(year, month - 1, 1)).getUTCDay();
  const day = 1 + ((weekday - first + 7) % 7) + (nth - 1) * 7;
  return `${year}-${String(month).padStart(2, "0")}-${String(day).padStart(2, "0")}`;
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
      if (w.nth_weekday) {
        const day = nthWeekday(Number(ctx.date.slice(0, 4)), ...w.nth_weekday);
        if (ctx.date < addDays(day, -(w.days_before ?? 0)) || ctx.date > addDays(day, w.days_after ?? 0)) return false;
      }
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

// Accent markup as items { key, host, html } (like scenes.js renderScene);
// props with an event are left to the scheduler.
export function renderAccents(accents, ctx) {
  return accents
    .filter((a) => !a.event)
    .map((a) => {
      const html = PROPS[a.prop].render(ctx, a.params || {});
      if (a.place === "ledge") return { key: `acc:${a.id}`, host: "back", html: `<div class="p-ledge">${html}</div>` };
      return { key: `acc:${a.id}`, host: a.place === "sky" ? "back" : a.place === "front" ? "front" : "edge", html };
    });
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
