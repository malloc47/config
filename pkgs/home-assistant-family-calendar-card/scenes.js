// scenes.js — composable sky scenes for family-calendar-card's sky theme
// (docs/family-calendar-scenes.md has the plan and the scene list).
//
// A scene is an id, a `when` (weather group or condition, phases) and a list
// of [layer, params]. Layers are small renderers reading one shared context
// (phase, darkness, weather, wind, date, ...); each returns markup for any of
// three hosts: `back` (behind everything), `edge` (pinned to the calendar
// sheet's top edge) and `front` (above the content, for flashes). Their CSS
// is static and collected into LAYER_CSS. Among the enabled scenes whose
// `when` matches, one is picked by a hash of (date, phase, group), so a
// static-weather week still varies day to day but nothing flickers within
// a day. Rare events (lightning, shooting stars, birds) are fired by the
// card's scheduler at the configured rates; a scene lists the ones it wants.

// Weather groups by HA condition.
const GROUPS = {
  sunny: "clear",
  "clear-night": "clear",
  partlycloudy: "partly",
  cloudy: "cloudy",
  exceptional: "cloudy",
  fog: "fog",
  rainy: "rain",
  pouring: "pouring",
  lightning: "storm",
  "lightning-rainy": "storm",
  snowy: "snow",
  "snowy-rainy": "sleet",
  hail: "sleet",
  windy: "wind",
  "windy-variant": "wind",
};

const LIGHT = ["dawn", "day", "golden"];
const DARK = ["dusk", "night", "late"];

// The phase of day from the sun: dawn (rising, below +6°), day, golden
// (setting, +10° to 0°), dusk (setting, 0° to -12°), night (below -12°)
// and late (night after 22:00 or before 05:00).
export function phaseOf(elevation, rising, hour) {
  if (elevation < -12) return hour >= 22 || hour < 5 ? "late" : "night";
  if (rising) return elevation < 6 ? "dawn" : "day";
  if (elevation > 10) return "day";
  return elevation >= 0 ? "golden" : "dusk";
}

export function contextOf({ elevation, rising, hour, condition, date, wind, temperature, dark, late, moon }) {
  const e = Number.isFinite(elevation) ? elevation : 30;
  const m = moon || { alt: -90, az: 180, illum: 0, phase: 0 };
  const up = m.alt > 0;
  return {
    // The moon (astro.js): up, and how much it brightens the sky (0-1).
    moon: { ...m, up },
    moonlight: up ? m.illum * Math.min(1, m.alt / 15 + 0.3) : 0,
    elevation: e,
    rising: !!rising,
    hour,
    phase: phaseOf(e, rising, hour),
    // 0 in daylight, 1 by full night, ramping through twilight.
    darkness: Math.max(0, Math.min(1, (6 - e) / 18)),
    dark: !!dark,
    late: !!late,
    condition: condition || "sunny",
    group: GROUPS[condition] || "clear",
    date,
    wind: Number.isFinite(wind) ? wind : 0,
    temperature: Number.isFinite(temperature) ? temperature : null,
  };
}

// FNV-1a, for the daily pick.
const hash = (s) => {
  let h = 2166136261;
  for (let i = 0; i < s.length; i++) {
    h ^= s.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
};

// ---- layers ----------------------------------------------------------------

// A deterministic pseudo-random sequence (mulberry32), so the star field is
// the same on every render.
const rng = (seed) => () => {
  seed |= 0;
  seed = (seed + 0x6d2b79f5) | 0;
  let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
  t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};
const STARS = (() => {
  const r = rng(1031);
  return Array.from({ length: 140 }, () => ({
    x: r() * 100,
    y: r() * 260,
    s: 0.8 + r() * r() * 2.2,
    o: 0.45 + r() * 0.55,
    tw: r() < 0.35 ? 2.5 + r() * 4 : 0,
    d: -r() * 6,
  }));
})();

// Screen position for an altitude/azimuth: east on the right (as the sun's
// glow), south in the middle; altitude 0 at the calendar's top edge.
const skyPos = (alt, az) => ({
  x: Math.max(4, Math.min(96, 50 + ((180 - az) * 40) / 90)),
  f: Math.max(0.05, 1 - alt / 90),
});

// The moon's lit shape for a phase (0 new .. 0.5 full .. 1), in a 100-unit
// box centred on 0: the right half lit while waxing, mirrored when waning.
const moonPath = (phase) => {
  const k = Math.cos(phase * 2 * Math.PI);
  const rx = Math.abs(k) * 50;
  return `M0 -50 A50 50 0 0 1 0 50 A${rx.toFixed(2)} 50 0 0 ${k > 0 ? 0 : 1} 0 -50 Z`;
};

export const LAYERS = {
  // Stars: fewer when the moon is bright or clouds are about, fading in
  // through twilight; a third twinkle. params: density (stars at a dark,
  // moonless sky), opacity.
  stars: {
    css: `.ly-stars { position: absolute; left: 0; right: 0; top: 0; height: 260px; }
      .ly-stars i { position: absolute; border-radius: 50%; background: #fff; }
      .ly-stars i.tw { animation: ly-twinkle var(--tw) ease-in-out infinite alternate; animation-delay: var(--d); }
      @keyframes ly-twinkle { 0% { opacity: var(--o); } 50% { opacity: calc(var(--o) * 0.25); } 100% { opacity: var(--o); } }`,
    render: (ctx, p) => {
      const n = Math.round((p.density ?? 60) * (1 - 0.7 * ctx.moonlight));
      const fade = Math.max(0, Math.min(1, (ctx.darkness - 0.35) / 0.5)) * (p.opacity ?? 1);
      if (n < 1 || fade <= 0) return {};
      return {
        back: `<div class="ly-stars" style="opacity: ${fade.toFixed(2)}">${STARS.slice(0, n)
          .map(
            (st) =>
              `<i class="${st.tw ? "tw" : ""}" style="left: ${st.x.toFixed(1)}%; top: ${st.y.toFixed(0)}px; width: ${st.s.toFixed(1)}px; height: ${st.s.toFixed(1)}px; opacity: ${st.o.toFixed(2)}; --o: ${st.o.toFixed(2)}${st.tw ? `; --tw: ${st.tw.toFixed(1)}s; --d: ${st.d.toFixed(1)}s` : ""}"></i>`,
          )
          .join("")}</div>`,
      };
    },
  },
  // The moon where it really is (when up), drawn in its real phase with a
  // faint earthshine on the dark part and a glow that grows with its light.
  // params: size, glow (a stronger halo).
  moon: {
    css: `.ly-moon { position: absolute; transform: translate(-50%, -50%); }
      .ly-moon .halo { position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); border-radius: 50%;
        background: radial-gradient(closest-side, rgba(235, 238, 255, var(--ha)), transparent); }
      .ly-moon svg { position: relative; display: block; overflow: visible; }`,
    render: (ctx, p) => {
      const m = ctx.moon;
      if (!m.up) return {};
      const size = p.size ?? 38;
      const { x, f } = skyPos(m.alt, m.az);
      const halo = size * (p.glow ? 7 : 4.5);
      const ha = ((p.glow ? 0.24 : 0.16) * m.illum + 0.03).toFixed(2);
      const waning = m.phase > 0.5;
      return {
        back: `<div class="ly-moon" style="left: ${x.toFixed(1)}%; top: calc(var(--edge-y, 220px) * ${f.toFixed(3)})">
          <span class="halo" style="width: ${halo}px; height: ${halo}px; --ha: ${ha}"></span>
          <svg viewBox="-50 -50 100 100" width="${size}" height="${size}" aria-hidden="true">
            <circle r="50" fill="rgba(150, 165, 195, 0.22)"></circle>
            <g transform="${waning ? "scale(-1 1)" : ""}"><path d="${moonPath(m.phase)}" fill="#f2eedf"></path></g>
            <g fill="rgba(70, 70, 90, 0.10)"><circle cx="-14" cy="-12" r="11"></circle><circle cx="16" cy="10" r="8"></circle>
              <circle cx="-6" cy="20" r="6"></circle><circle cx="20" cy="-20" r="5"></circle></g>
          </svg></div>`,
      };
    },
  },
  // The Milky Way: a soft diagonal band with a dark lane, for dark-moon
  // nights. params: opacity.
  milkyway: {
    css: `.ly-milkyway { position: absolute; left: -30%; right: -30%; top: 30px; height: 150px; transform: rotate(-24deg); filter: blur(6px);
      background: radial-gradient(60% 50% at 50% 50%, rgba(225, 228, 255, 0.22), transparent 70%),
        radial-gradient(30% 22% at 40% 52%, rgba(10, 12, 25, 0.35), transparent 70%),
        radial-gradient(18% 40% at 62% 46%, rgba(240, 230, 255, 0.16), transparent 70%); }`,
    render: (ctx, p) => {
      const o = Math.max(0, Math.min(1, (ctx.darkness - 0.6) / 0.4)) * (1 - ctx.moonlight) * (p.opacity ?? 1);
      return o > 0.05 ? { back: `<div class="ly-milkyway" style="opacity: ${o.toFixed(2)}"></div>` } : {};
    },
  },
  clouds: {
    css: `.ly-clouds { position: absolute; left: 0; right: 0; top: 0; height: 240px; }
      .ly-clouds i { position: absolute; border-radius: 50%; background: rgba(255, 255, 255, var(--a, 0.5)); filter: blur(22px); }`,
    render: (ctx, p) => {
      const a = p.opacity ?? 0.5;
      const blobs = [
        [46, 40, 300, 90],
        [65, 95, 260, 70],
        ...(p.deck ? [[5, 20, 340, 100], [20, 110, 300, 80]] : []),
      ];
      return {
        back: `<div class="ly-clouds" style="--a: ${a}">${blobs
          .map(([x, y, w, h]) => `<i style="left: ${x}%; top: ${y}px; width: ${w}px; height: ${h}px"></i>`)
          .join("")}</div>`,
      };
    },
  },
  rain: {
    css: `.ly-rain { position: absolute; left: 0; right: 0; top: 0; bottom: 60%;
      background: repeating-linear-gradient(104deg, transparent 0 11px, rgba(255, 255, 255, 0.22) 11px 12px, transparent 12px 23px),
        repeating-linear-gradient(104deg, transparent 0 17px, rgba(255, 255, 255, 0.14) 17px 18px, transparent 18px 31px); }`,
    render: (ctx, p) => ({ back: `<div class="ly-rain" style="opacity: ${p.opacity ?? (ctx.dark ? 0.45 : 0.9)}"></div>` }),
  },
  snow: {
    css: `.ly-snow { position: absolute; left: 0; right: 0; top: 0; bottom: 60%; background-size: 140px 120px;
      background-image: radial-gradient(2px 2px at 20px 30px, #fff 98%, transparent), radial-gradient(1.5px 1.5px at 90px 70px, #fff 98%, transparent),
        radial-gradient(2.5px 2.5px at 60px 100px, #fff 98%, transparent), radial-gradient(1.5px 1.5px at 120px 15px, #fff 98%, transparent); }`,
    render: (ctx, p) => ({ back: `<div class="ly-snow" style="opacity: ${p.opacity ?? 0.85}"></div>` }),
  },
  fog: {
    css: `.ly-fog { position: absolute; left: 0; right: 0; top: 0; bottom: 45%;
      background: linear-gradient(180deg, transparent, rgba(236, 240, 245, 0.35) 60%, transparent); }`,
    render: () => ({ back: `<div class="ly-fog"></div>` }),
  },
};


// ---- scenes ----------------------------------------------------------------

// Today's looks, one scene per combination; later tasks add variations
// (more scenes with the same `when`).
export const SCENES = {
  "clear-day": { when: { group: "clear", phase: LIGHT }, layers: [] },
  "clear-dusk": { when: { group: "clear", phase: "dusk" }, layers: [["stars", { density: 45 }], ["moon", {}]] },
  "clear-night-moonlit": {
    when: { group: "clear", phase: "night" },
    layers: [["stars", { density: 45 }], ["moon", { glow: true }]],
    events: ["shooting_star"],
  },
  "clear-night-starfield": {
    when: { group: "clear", phase: "night" },
    layers: [["stars", { density: 110 }], ["moon", {}]],
    events: ["shooting_star"],
  },
  "clear-late-milkyway": {
    when: { group: "clear", phase: "late", moon: "dark" },
    layers: [["milkyway", {}], ["stars", { density: 120 }], ["moon", {}]],
    events: ["shooting_star"],
  },
  "clear-late-moon": {
    when: { group: "clear", phase: "late" },
    layers: [["stars", { density: 60 }], ["moon", { glow: true, size: 44 }]],
    events: ["shooting_star"],
  },
  "partly-day": { when: { group: "partly", phase: LIGHT }, layers: [["clouds", {}]] },
  "partly-night": { when: { group: "partly", phase: DARK }, layers: [["stars", { density: 30 }], ["moon", {}]] },
  "cloudy-day": { when: { group: "cloudy", phase: LIGHT }, layers: [["clouds", { deck: true }]] },
  "cloudy-night": { when: { group: "cloudy", phase: DARK }, layers: [] },
  fog: { when: { group: "fog" }, layers: [["fog", {}]] },
  rain: { when: { group: "rain" }, layers: [["rain", {}]] },
  pouring: { when: { group: "pouring" }, layers: [["rain", { opacity: 1 }]] },
  storm: { when: { group: "storm" }, layers: [["rain", {}]], events: ["lightning"] },
  snow: { when: { group: "snow" }, layers: [["snow", {}]] },
  sleet: { when: { group: "sleet" }, layers: [["snow", {}], ["rain", { opacity: 0.6 }]] },
  "wind-day": { when: { group: "wind", phase: LIGHT }, layers: [] },
  "wind-night": { when: { group: "wind", phase: DARK }, layers: [["stars", { density: 40 }], ["moon", {}]] },
};

const listOf = (v) => (v === undefined ? null : [].concat(v));

function matches(when = {}, ctx) {
  const groups = listOf(when.group);
  const conditions = listOf(when.condition);
  const phases = listOf(when.phase);
  if (groups && !groups.includes(ctx.group)) return false;
  if (conditions && !conditions.includes(ctx.condition)) return false;
  if (phases && !phases.includes(ctx.phase)) return false;
  // moon: dark (down or a thin crescent), bright, or up.
  if (when.moon === "dark" && ctx.moonlight >= 0.25) return false;
  if (when.moon === "bright" && ctx.moonlight < 0.25) return false;
  if (when.moon === "up" && !ctx.moon.up) return false;
  return true;
}

// The scene for a context. cfg is the card's sky_scenes: `disable` (ids),
// `map` (condition or group -> ids, replacing the matching), `define` (extra
// scenes by id).
export function pickScene(ctx, cfg = {}) {
  const all = { ...SCENES, ...(cfg.define || {}) };
  const disabled = new Set(cfg.disable || []);
  const usable = (id) => all[id] && !disabled.has(id);
  const mapped = listOf(cfg.map?.[ctx.condition] ?? cfg.map?.[ctx.group]);
  let ids = mapped ? mapped.filter((id) => usable(id) && matches({ phase: all[id].when?.phase }, ctx)) : [];
  if (!ids.length) ids = Object.keys(all).filter((id) => usable(id) && matches(all[id].when, ctx));
  if (!ids.length) return { id: "none", layers: [], events: [] };
  const id = ids[hash(`${ctx.date}|${ctx.phase}|${ctx.group}`) % ids.length];
  return { id, layers: all[id].layers || [], events: all[id].events || [] };
}

// A scene's markup as items { key, host, html }, one per layer and host, so
// the card can update just the layers that changed.
export function renderScene(scene, ctx) {
  const items = [];
  scene.layers.forEach(([name, params], i) => {
    const layer = LAYERS[name];
    if (!layer) return;
    const r = layer.render(ctx, params || {});
    for (const host of ["back", "edge", "front"]) if (r[host]) items.push({ key: `${i}:${name}`, host, html: r[host] });
  });
  return items;
}

// Rare events: when the scene lists one, the card fires it at random
// intervals from sky_scenes.rates (seconds [min, max]). fire() adds
// short-lived markup to a host and returns how long to keep it (ms).
export const EVENTS = {
  // A meteor streaking down across the upper sky.
  shooting_star: {
    css: `.ly-meteor { position: absolute; width: 130px; height: 2px; transform-origin: 0 50%; opacity: 0;
      background: linear-gradient(90deg, rgba(255, 255, 255, 0.95), rgba(255, 255, 255, 0));
      animation: ly-meteor 1.1s ease-out forwards; }
      @keyframes ly-meteor { 0% { opacity: 0; transform: rotate(var(--rot)) translateX(0) scaleX(0.3); }
        15% { opacity: 1; } 100% { opacity: 0; transform: rotate(var(--rot)) translateX(-200px) scaleX(1); } }`,
    fire(hosts) {
      const node = document.createElement("i");
      node.className = "ly-meteor";
      node.style.left = `${30 + Math.random() * 60}%`;
      node.style.top = `${20 + Math.random() * 110}px`;
      node.style.setProperty("--rot", `${200 + Math.random() * 25}deg`);
      hosts.back.append(node);
      return [[node], 1500];
    },
  },
};
export const DEFAULT_RATES = { lightning: [30, 90], shooting_star: [180, 300], birds: [3000, 4200] };

// All layer and event CSS, for the card's stylesheet.
export const LAYER_CSS = [...Object.values(LAYERS), ...Object.values(EVENTS)]
  .map((l) => l.css || "")
  .join("\n");
