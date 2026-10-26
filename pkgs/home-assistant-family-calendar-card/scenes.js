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

export function contextOf({ elevation, rising, hour, condition, date, wind, temperature, dark, late }) {
  const e = Number.isFinite(elevation) ? elevation : 30;
  return {
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

const STAR_FIELD =
  "radial-gradient(1.2px 1.2px at 8% 12%, #fff 98%, transparent), radial-gradient(1px 1px at 22% 6%, #fff 98%, transparent)," +
  " radial-gradient(1.4px 1.4px at 37% 15%, #fff 98%, transparent), radial-gradient(1px 1px at 51% 8%, #fff 98%, transparent)," +
  " radial-gradient(1.2px 1.2px at 66% 4%, #fff 98%, transparent), radial-gradient(1px 1px at 78% 13%, #fff 98%, transparent)," +
  " radial-gradient(1.3px 1.3px at 91% 7%, #fff 98%, transparent), radial-gradient(1px 1px at 14% 22%, #fff 98%, transparent)," +
  " radial-gradient(1px 1px at 58% 19%, #fff 98%, transparent), radial-gradient(1.1px 1.1px at 84% 21%, #fff 98%, transparent)";

export const LAYERS = {
  stars: {
    css: `.ly-stars { position: absolute; left: 0; right: 0; top: 0; bottom: 55%; background: ${STAR_FIELD}; }`,
    render: (ctx, p) => ({ back: `<div class="ly-stars" style="opacity: ${p.opacity ?? 0.8}"></div>` }),
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

export const LAYER_CSS = Object.values(LAYERS)
  .map((l) => l.css || "")
  .join("\n");

// ---- scenes ----------------------------------------------------------------

// Today's looks, one scene per combination; later tasks add variations
// (more scenes with the same `when`).
export const SCENES = {
  "clear-day": { when: { group: "clear", phase: LIGHT }, layers: [] },
  "clear-night": { when: { group: "clear", phase: DARK }, layers: [["stars", {}]] },
  "partly-day": { when: { group: "partly", phase: LIGHT }, layers: [["clouds", {}]] },
  "partly-night": { when: { group: "partly", phase: DARK }, layers: [["stars", { opacity: 0.4 }]] },
  "cloudy-day": { when: { group: "cloudy", phase: LIGHT }, layers: [["clouds", { deck: true }]] },
  "cloudy-night": { when: { group: "cloudy", phase: DARK }, layers: [] },
  fog: { when: { group: "fog" }, layers: [["fog", {}]] },
  rain: { when: { group: "rain" }, layers: [["rain", {}]] },
  pouring: { when: { group: "pouring" }, layers: [["rain", { opacity: 1 }]] },
  storm: { when: { group: "storm" }, layers: [["rain", {}]], events: ["lightning"] },
  snow: { when: { group: "snow" }, layers: [["snow", {}]] },
  sleet: { when: { group: "sleet" }, layers: [["snow", {}], ["rain", { opacity: 0.6 }]] },
  "wind-day": { when: { group: "wind", phase: LIGHT }, layers: [] },
  "wind-night": { when: { group: "wind", phase: DARK }, layers: [["stars", { opacity: 0.6 }]] },
};

const listOf = (v) => (v === undefined ? null : [].concat(v));

function matches(when = {}, ctx) {
  const groups = listOf(when.group);
  const conditions = listOf(when.condition);
  const phases = listOf(when.phase);
  if (groups && !groups.includes(ctx.group)) return false;
  if (conditions && !conditions.includes(ctx.condition)) return false;
  if (phases && !phases.includes(ctx.phase)) return false;
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

// A scene's markup per host.
export function renderScene(scene, ctx) {
  const out = { back: "", edge: "", front: "" };
  for (const [name, params] of scene.layers) {
    const layer = LAYERS[name];
    if (!layer) continue;
    const r = layer.render(ctx, params || {});
    for (const k of Object.keys(out)) if (r[k]) out[k] += r[k];
  }
  return out;
}

// Rare events: when the scene lists one, the card fires it at random
// intervals from sky_scenes.rates (seconds [min, max]). fire() adds
// short-lived markup to a host and returns how long to keep it (ms).
export const EVENTS = {};
export const DEFAULT_RATES = { lightning: [30, 90], shooting_star: [180, 300], birds: [3000, 4200] };
