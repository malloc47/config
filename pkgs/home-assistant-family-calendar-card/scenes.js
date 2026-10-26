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

export function contextOf({ elevation, rising, hour, condition, date, wind, temperature, dark, late, moon, planets, kp, auroraKp = 4 }) {
  const e = Number.isFinite(elevation) ? elevation : 30;
  const darkness = Math.max(0, Math.min(1, (6 - e) / 18));
  const m = moon || { alt: -90, az: 180, illum: 0, phase: 0 };
  const up = m.alt > 0;
  return {
    // The moon (astro.js): up, and how much it brightens the sky (0-1).
    moon: { ...m, up },
    moonlight: up ? m.illum * Math.min(1, m.alt / 15 + 0.3) : 0,
    // Planets visible now (astro.js): above the horizon, clear of the sun,
    // and the sky dark enough for their brightness (Venus and Jupiter show
    // in twilight).
    planets: (planets || []).filter(
      (p) => p.alt > 4 && p.elong > (p.name === "mercury" ? 18 : 15) && darkness >= (p.mag < -1.5 ? 0.3 : 0.6),
    ),
    elevation: e,
    rising: !!rising,
    hour,
    phase: phaseOf(e, rising, hour),
    // 0 in daylight, 1 by full night, ramping through twilight.
    darkness,
    // Where the sun's glow sits (as sky.js draws it): right in the morning,
    // left in the evening, % of the page.
    sun: { x: rising ? 84 : 16, y: Math.max(4, Math.min(36, 36 - e * 1.3)) },
    // Geomagnetic activity (planetary Kp) and whether it reaches the aurora
    // threshold.
    kp: Number.isFinite(kp) ? kp : 0,
    aurora: Number.isFinite(kp) && kp >= auroraKp,
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
      const halo = size * (p.glow || p.blur ? 7 : 4.5);
      const ha = ((p.glow ? 0.24 : 0.16) * m.illum + 0.03).toFixed(2);
      const waning = m.phase > 0.5;
      return {
        back: `<div class="ly-moon" style="left: ${x.toFixed(1)}%; top: calc(var(--edge-y, 220px) * ${f.toFixed(3)})">
          <span class="halo" style="width: ${halo}px; height: ${halo}px; --ha: ${ha}"></span>
          <svg viewBox="-50 -50 100 100" width="${size}" height="${size}" aria-hidden="true"${p.blur ? ` style="filter: blur(${p.blur}px)"` : ""}>
            <circle r="50" fill="rgba(150, 165, 195, 0.22)"></circle>
            <g transform="${waning ? "scale(-1 1)" : ""}"><path d="${moonPath(m.phase)}" fill="#f2eedf"></path></g>
            <g fill="rgba(70, 70, 90, 0.10)"><circle cx="-14" cy="-12" r="11"></circle><circle cx="16" cy="10" r="8"></circle>
              <circle cx="-6" cy="20" r="6"></circle><circle cx="20" cy="-20" r="5"></circle></g>
          </svg></div>`,
      };
    },
  },
  // The bright planets where they really are: steady (no twinkle), sized by
  // brightness, tinted (Mars red, Saturn gold), optionally labelled.
  // params: labels, only (names).
  planets: {
    css: `.ly-planet { position: absolute; transform: translate(-50%, -50%); display: flex; align-items: center; gap: 6px; }
      .ly-planet i { display: block; border-radius: 50%; background: var(--t); box-shadow: 0 0 6px 1px var(--t); }
      .ly-planet span { font-size: 11px; font-weight: 600; letter-spacing: 0.04em; color: var(--on-sky2); white-space: nowrap;
        text-shadow: var(--sky-shadow); transform: translateX(calc(50% + 8px)); position: absolute; left: 0; }`,
    render: (ctx, p) => {
      const list = ctx.planets.filter((pl) => !p.only || p.only.includes(pl.name));
      if (!list.length) return {};
      return {
        back: list
          .map((pl) => {
            const { x, f } = skyPos(pl.alt, pl.az);
            const size = Math.max(2.4, Math.min(6, 4 - pl.mag * 0.7));
            const name = pl.name[0].toUpperCase() + pl.name.slice(1);
            return `<div class="ly-planet" style="left: ${x.toFixed(1)}%; top: calc(var(--edge-y, 220px) * ${f.toFixed(3)}); --t: ${pl.tint}">
              <i style="width: ${size.toFixed(1)}px; height: ${size.toFixed(1)}px"></i>${p.labels ? `<span>${name}</span>` : ""}</div>`;
          })
          .join(""),
      };
    },
  },
  // Cumulus clouds drifting across the sky: a shared puff shape (a mask)
  // over a top-to-bottom gradient, so the tint shades them: day (white, grey
  // base), lit (golden or, rising, pink undersides), night (slate), dark
  // (brooding). params: count, speed (s per crossing), opacity, tint, y
  // (centre px; default spread over the sky), spread.
  cumulus: {
    css: `.ly-cu { position: absolute; left: 0; right: 0; top: 0; height: 300px; overflow: hidden; }
      .ly-cu i { position: absolute; left: -280px; top: var(--y); width: 260px; height: 110px; opacity: var(--o);
        background: linear-gradient(180deg, var(--c1), var(--c2));
        -webkit-mask-image: radial-gradient(closest-side, #000 60%, transparent), radial-gradient(closest-side, #000 60%, transparent),
          radial-gradient(closest-side, #000 60%, transparent), radial-gradient(closest-side, #000 60%, transparent),
          radial-gradient(closest-side, #000 65%, transparent);
        -webkit-mask-size: 36% 56%, 42% 72%, 38% 62%, 30% 46%, 90% 36%;
        -webkit-mask-position: 14% 64%, 40% 30%, 66% 46%, 88% 66%, 50% 88%;
        -webkit-mask-repeat: no-repeat;
        animation: ly-cu-drift var(--dur) linear infinite; animation-delay: var(--d); }
      @keyframes ly-cu-drift { from { transform: translateX(0) scale(var(--s)); } to { transform: translateX(calc(100vw + 320px)) scale(var(--s)); } }`,
    render: (ctx, p) => {
      const tints = {
        day: ["#ffffff", "#dfe6ef"],
        lit: ctx.rising ? ["#fff1f0", "#efa3b4"] : ["#fff5e8", "#f2a173"],
        night: ["rgba(96, 108, 136, 0.95)", "rgba(42, 50, 70, 0.95)"],
        dark: ["#8b939f", "#4c535e"],
      };
      const [c1, c2] = tints[p.tint || (ctx.dark ? "night" : "day")];
      const n = p.count ?? 4;
      const speed = p.speed ?? 170;
      const r = rng(77 + n);
      const clouds = Array.from({ length: n }, (_, k) => ({
        y: (p.y ?? 90) + (r() - 0.5) * (p.spread ?? 150),
        s: 0.55 + r() * 0.7,
        dur: speed * (0.8 + r() * 0.5),
        d: -((k + r() * 0.6) / n) * speed,
      }));
      return {
        back: `<div class="ly-cu" style="--c1: ${c1}; --c2: ${c2}; --o: ${p.opacity ?? 0.92}">${clouds
          .map((c) => `<i style="--y: ${c.y.toFixed(0)}px; --s: ${c.s.toFixed(2)}; --dur: ${c.dur.toFixed(0)}s; --d: ${c.d.toFixed(0)}s"></i>`)
          .join("")}</div>`,
      };
    },
  },
  // An overcast deck: soft horizontal bands drifting slowly. params: tone
  // (soft, bright: milky with the sun showing faintly, brooding: dark and
  // low, night).
  deck: {
    css: `.ly-deck { position: absolute; left: 0; right: 0; top: 0; height: 320px; overflow: hidden; }
      .ly-deck i { position: absolute; left: 0; width: 200%; top: var(--y); height: var(--h); opacity: var(--o);
        background: linear-gradient(180deg, transparent, var(--c) 35%, var(--c) 65%, transparent);
        -webkit-mask-image: repeating-radial-gradient(ellipse 160px 60px at 30% 50%, #000 0 40%, rgba(0, 0, 0, 0.55) 70%, #000 100%);
        animation: ly-deck-drift var(--dur) linear infinite alternate; }
      @keyframes ly-deck-drift { from { transform: translateX(0); } to { transform: translateX(-35%); } }
      .ly-deck b { position: absolute; width: 110px; height: 110px; border-radius: 50%; transform: translate(-50%, -50%);
        background: radial-gradient(closest-side, rgba(255, 253, 240, 0.85) 0 22%, rgba(255, 250, 230, 0.35) 45%, transparent); }`,
    render: (ctx, p) => {
      const tones = {
        soft: ["rgba(214, 220, 229, 0.8)", [[10, 90, 0.7, 260], [70, 110, 0.6, 340], [150, 120, 0.55, 300]]],
        bright: ["rgba(250, 251, 253, 0.75)", [[0, 100, 0.65, 300], [80, 120, 0.55, 380]]],
        brooding: ["rgba(58, 64, 78, 0.92)", [[50, 120, 0.9, 240], [120, 130, 0.88, 320], [190, 120, 0.8, 280], [0, 90, 0.7, 360]]],
        night: ["rgba(60, 68, 88, 0.85)", [[10, 110, 0.8, 300], [100, 130, 0.7, 360]]],
      };
      const [c, bands] = tones[p.tone || (ctx.dark ? "night" : "soft")];
      const sun = p.tone === "bright" ? `<b style="left: ${ctx.sun.x}%; top: ${(ctx.sun.y * 12.8).toFixed(0)}px"></b>` : "";
      return {
        back: `<div class="ly-deck" style="--c: ${c}">${sun}${bands
          .map(([y, h, o, dur]) => `<i style="--y: ${y}px; --h: ${h}px; --o: ${o}; --dur: ${dur}s"></i>`)
          .join("")}</div>`,
      };
    },
  },
  // Cloud shadows: large soft shades sweeping over everything, faintly.
  shadows: {
    css: `.ly-shadows i { position: absolute; top: var(--y); left: -900px; width: 800px; height: 520px; border-radius: 50%;
        background: radial-gradient(closest-side, rgba(10, 20, 40, 0.11), transparent); mix-blend-mode: multiply;
        animation: ly-shadow var(--dur) linear infinite; animation-delay: var(--d); }
      @keyframes ly-shadow { to { transform: translateX(calc(100vw + 1000px)); } }`,
    render: () => ({
      front: `<div class="ly-shadows"><i style="--y: 80px; --dur: 70s; --d: -10s"></i><i style="--y: 520px; --dur: 95s; --d: -60s"></i></div>`,
    }),
  },
  // Crepuscular rays fanning from the sun's glow. params: opacity.
  rays: {
    css: `.ly-rays { position: absolute; width: 1400px; height: 1400px; transform: translate(-50%, -50%);
        background: repeating-conic-gradient(from 0deg, rgba(255, 238, 205, 0) 0deg 5deg, rgba(255, 236, 200, 0.16) 5deg 8deg, rgba(255, 238, 205, 0) 8deg 13deg);
        -webkit-mask-image: radial-gradient(closest-side, #000 8%, rgba(0, 0, 0, 0.5) 40%, transparent 72%);
        animation: ly-rays 40s ease-in-out infinite alternate; }
      @keyframes ly-rays { from { transform: translate(-50%, -50%) rotate(-3deg); } to { transform: translate(-50%, -50%) rotate(4deg); } }`,
    render: (ctx, p) => ({
      back: `<div class="ly-rays" style="left: ${ctx.sun.x}%; top: ${ctx.sun.y}%; opacity: ${p.opacity ?? 0.9}"></div>`,
    }),
  },
  // The sun showing through, dimming now and then as a cloud passes.
  sunpeek: {
    css: `.ly-sunpeek { position: absolute; width: 220px; height: 220px; transform: translate(-50%, -50%); border-radius: 50%;
        background: radial-gradient(closest-side, rgba(255, 253, 240, 0.95) 0 14%, rgba(255, 240, 200, 0.45) 30%, transparent);
        animation: ly-sunpeek 48s ease-in-out infinite; }
      @keyframes ly-sunpeek { 0%, 30%, 62%, 100% { opacity: 1; } 40%, 52% { opacity: 0.35; } 80% { opacity: 0.7; } }`,
    render: (ctx) => ({ back: `<div class="ly-sunpeek" style="left: ${ctx.sun.x}%; top: ${Math.min(ctx.sun.y, 14)}%"></div>` }),
  },
  // Fog: pale banks drifting across the lower sky. params: banks (1-3),
  // opacity, burn (thins as the sun climbs), night (bluer, dimmer).
  fog: {
    css: `.ly-fog { position: absolute; left: 0; right: 0; top: 0; height: 100%; overflow: hidden; }
      .ly-fog i { position: absolute; left: -50%; width: 200%; top: var(--y); height: var(--h); filter: blur(14px);
        background: linear-gradient(180deg, transparent, var(--c) 45%, var(--c) 55%, transparent);
        animation: ly-fog-drift var(--dur) ease-in-out infinite alternate; }
      @keyframes ly-fog-drift { from { transform: translateX(-12%); } to { transform: translateX(12%); } }`,
    render: (ctx, p) => {
      const burn = p.burn ? Math.max(0.2, Math.min(1, 1 - (ctx.elevation - 5) / 25)) : 1;
      const o = (p.opacity ?? 1) * burn;
      const c = p.night || ctx.dark ? "rgba(150, 165, 190, 0.32)" : "rgba(238, 241, 245, 0.55)";
      const banks = [[120, 140, 50], [200, 120, 70], [40, 110, 90]].slice(0, p.banks ?? 2);
      return {
        back: `<div class="ly-fog" style="--c: ${c}; opacity: ${o.toFixed(2)}">${banks
          .map(([y, h, dur]) => `<i style="--y: ${y}px; --h: ${h}px; --dur: ${dur}s"></i>`)
          .join("")}</div>`,
      };
    },
  },
  // A frosted, fogged band along the calendar sheet's top edge.
  fogedge: {
    css: `.ly-fogedge { position: absolute; left: 0; right: 0; top: -26px; height: 40px; border-radius: 22px 22px 0 0;
        background: linear-gradient(180deg, transparent, rgba(240, 244, 248, 0.28)); -webkit-backdrop-filter: blur(3px); backdrop-filter: blur(3px);
        -webkit-mask-image: linear-gradient(180deg, transparent, #000 60%); }`,
    render: () => ({ edge: `<div class="ly-fogedge"></div>` }),
  },
  // Streetlamps glowing in the fog below the sky line.
  lamps: {
    css: `.ly-lamps i { position: absolute; top: calc(var(--edge-y, 220px) - 46px); width: 120px; height: 120px; transform: translateX(-50%);
        border-radius: 50%; background: radial-gradient(closest-side, rgba(255, 205, 130, 0.5), rgba(255, 190, 110, 0.12) 50%, transparent);
        animation: ly-lamp 7s ease-in-out infinite alternate; animation-delay: var(--d); }
      @keyframes ly-lamp { from { opacity: 0.8; } to { opacity: 1; } }`,
    render: () => ({
      back: `<div class="ly-lamps">${[12, 38, 63, 88].map((x, i) => `<i style="left: ${x}%; --d: ${-i * 2}s"></i>`).join("")}</div>`,
    }),
  },
  // Warm town light glowing on the underside of an overcast night sky.
  cityglow: {
    css: `.ly-cityglow { position: absolute; left: -10%; right: -10%; top: calc(var(--edge-y, 220px) - 170px); height: 230px;
        background: radial-gradient(60% 70% at 50% 100%, rgba(255, 170, 105, 0.36), transparent 75%); }`,
    render: () => ({ back: `<div class="ly-cityglow"></div>` }),
  },
  // Aurora curtains: slow, swaying green-to-violet bands across the top of
  // the sky, brighter with more geomagnetic activity. params: opacity.
  aurora: {
    css: `.ly-aurora { position: absolute; left: 0; right: 0; top: 0; height: 240px; overflow: hidden; mix-blend-mode: screen; filter: blur(2.5px); }
      .ly-aurora i { position: absolute; top: var(--t); left: var(--x); width: var(--w); height: var(--h); border-radius: 50% 50% 40% 40%;
        -webkit-mask-image: repeating-linear-gradient(90deg, #000 0 2px, rgba(0, 0, 0, 0.45) 2px 5px, #000 5px 6px, rgba(0, 0, 0, 0.6) 6px 9px),
          linear-gradient(90deg, transparent, #000 25%, #000 75%, transparent);
        -webkit-mask-composite: source-in; mask-composite: intersect;
        background: linear-gradient(180deg, rgba(190, 120, 255, 0) 0%, rgba(175, 110, 255, 0.32) 22%, rgba(90, 255, 170, 0.5) 58%, rgba(60, 230, 150, 0) 100%);
        transform-origin: 50% 100%; animation: ly-aurora var(--dur) ease-in-out infinite alternate; animation-delay: var(--d); }
      @keyframes ly-aurora { 0% { transform: translateX(-14px) skewX(-9deg) scaleY(0.82); opacity: 0.55; } 50% { opacity: 1; }
        100% { transform: translateX(18px) skewX(11deg) scaleY(1.12); opacity: 0.75; } }`,
    render: (ctx, p) => {
      const fade = Math.max(0, Math.min(1, (ctx.darkness - 0.6) / 0.4));
      const o = Math.max(0.35, Math.min(1, (ctx.kp - 3) / 4)) * fade * (p.opacity ?? 1);
      if (o <= 0.05) return {};
      const bands = [
        [-4, 18, 22, 150, 14, 0],
        [12, 6, 18, 175, 18, -5],
        [27, 22, 24, 140, 12, -9],
        [46, 4, 20, 185, 21, -3],
        [63, 16, 22, 150, 15, -12],
        [80, 8, 24, 170, 19, -7],
      ];
      return {
        back: `<div class="ly-aurora" style="opacity: ${o.toFixed(2)}">${bands
          .map(([x, t, w, h, dur, d]) => `<i style="--x: ${x}%; --t: ${t}px; --w: ${w}%; --h: ${h}px; --dur: ${dur}s; --d: ${d}s"></i>`)
          .join("")}</div>`,
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
  // Rain falling through the sky and landing on the calendar sheet's top
  // edge (the drops stop there; `splashes` draws the landings). The slant
  // comes from skewing the container. params: density (drops, capped at
  // 90), speed (s to fall, ~0.6), slant (deg), opacity, tint (day, night,
  // lamp: warm, lit by streetlamps), length (px).
  rainfall: {
    css: `.ly-rainfall { position: absolute; left: -12%; right: -12%; top: 0; height: var(--edge-y, 220px); overflow: hidden; transform: skewX(var(--sk)); }
      .ly-rainfall i { position: absolute; top: -80px; left: var(--x); width: var(--w); height: var(--l); border-radius: 1px;
        background: linear-gradient(180deg, transparent, var(--c)); animation: ly-fall var(--dur) linear infinite; animation-delay: var(--d); }
      @keyframes ly-fall { to { transform: translateY(calc(var(--edge-y, 220px) + 80px)); } }`,
    render: (ctx, p) => {
      const n = Math.min(90, p.density ?? 45);
      const tint = { day: "rgba(255, 255, 255, 0.6)", night: "rgba(196, 212, 240, 0.45)", lamp: "rgba(255, 214, 160, 0.55)" }[
        p.tint || (ctx.dark ? "night" : "day")
      ];
      const speed = p.speed ?? 0.65;
      const len = p.length ?? 34;
      const r = rng(311 + n);
      const drops = Array.from({ length: n }, () => ({
        x: r() * 100,
        w: 1 + r() * 0.8,
        l: len * (0.6 + r() * 0.8),
        dur: speed * (0.75 + r() * 0.5),
        d: -r() * 2,
      }));
      return {
        back: `<div class="ly-rainfall" style="--sk: ${-(p.slant ?? 12)}deg; --c: ${tint}; opacity: ${p.opacity ?? 1}">${drops
          .map(
            (d) =>
              `<i style="--x: ${d.x.toFixed(1)}%; --w: ${d.w.toFixed(1)}px; --l: ${d.l.toFixed(0)}px; --dur: ${d.dur.toFixed(2)}s; --d: ${d.d.toFixed(2)}s"></i>`,
          )
          .join("")}</div>`,
      };
    },
  },
  // Splashes along the calendar sheet's top edge: little rings spreading
  // with a droplet hopping up, and beads of water sitting on the glass.
  // params: count, beads, heavy (bigger and more often).
  splashes: {
    css: `.ly-splash i { position: absolute; top: -3px; left: var(--x); width: 16px; height: 5px; margin-left: -8px; border-radius: 50%;
        border: 2px solid rgba(255, 255, 255, 0.8); opacity: 0; animation: ly-splash var(--dur) ease-out infinite; animation-delay: var(--d); }
      .ly-splash i::after { content: ""; position: absolute; left: 50%; top: -2px; width: 2.5px; height: 2.5px; margin-left: -1.25px; border-radius: 50%;
        background: rgba(255, 255, 255, 0.85); animation: inherit; animation-name: ly-hop; }
      .screen.dark .ly-splash i { border-color: rgba(205, 218, 240, 0.6); }
      .ly-splash b { position: absolute; top: 2px; left: var(--x); width: var(--s); height: var(--s); border-radius: 50%;
        background: radial-gradient(circle at 35% 30%, rgba(255, 255, 255, 0.95), rgba(255, 255, 255, 0.25) 45%, rgba(255, 255, 255, 0.05) 70%);
        box-shadow: 0 1px 1px rgba(0, 0, 0, 0.12); }
      @keyframes ly-splash { 0% { opacity: 0.95; transform: scale(0.2); } 30% { opacity: 0; transform: scale(var(--sc, 1.3)); } 100% { opacity: 0; } }
      @keyframes ly-hop { 0% { transform: translateY(0); opacity: 1; } 15% { transform: translateY(-9px); opacity: 0.9; } 30% { transform: translateY(0); opacity: 0; } 100% { opacity: 0; } }`,
    render: (ctx, p) => {
      const n = p.count ?? (p.heavy ? 26 : 16);
      const r = rng(907 + n);
      const rings = Array.from({ length: n }, () => ({ x: 2 + r() * 96, dur: (p.heavy ? 0.7 : 1.1) + r() * 1.2, d: -r() * 3 }));
      const beads = Array.from({ length: p.beads ?? 9 }, () => ({ x: 3 + r() * 94, s: 3 + r() * 4 }));
      return {
        edge: `<div class="ly-splash" style="--sc: ${p.heavy ? 1.7 : 1.3}">${rings
          .map((g) => `<i style="--x: ${g.x.toFixed(1)}%; --dur: ${g.dur.toFixed(2)}s; --d: ${g.d.toFixed(2)}s"></i>`)
          .join("")}${beads.map((b) => `<b style="--x: ${b.x.toFixed(1)}%; --s: ${b.s.toFixed(1)}px"></b>`).join("")}</div>`,
      };
    },
  },
  // Raindrops on the glass: a few beads trickling slowly down the calendar
  // sheet, faint enough to read through.
  windowdrops: {
    css: `.ly-wdrops i { position: absolute; top: calc(var(--edge-y, 220px) + var(--y)); left: var(--x); width: var(--s); height: calc(var(--s) * 1.2);
        border-radius: 50% 50% 50% 50% / 60% 60% 40% 40%; opacity: 0;
        background: radial-gradient(circle at 35% 30%, rgba(255, 255, 255, 0.7), rgba(255, 255, 255, 0.12) 50%, rgba(255, 255, 255, 0.02) 75%);
        box-shadow: 0 1px 1.5px rgba(0, 0, 0, 0.12); animation: ly-trickle var(--dur) ease-in infinite; animation-delay: var(--d); }
      @keyframes ly-trickle { 0% { opacity: 0; transform: translateY(0); } 10% { opacity: 0.85; } 60% { transform: translateY(14px); }
        85% { opacity: 0.6; transform: translateY(90px); } 100% { opacity: 0; transform: translateY(110px); } }`,
    render: () => {
      const r = rng(4242);
      return {
        front: `<div class="ly-wdrops">${Array.from({ length: 8 }, () => {
          const [x, y, s, dur, d] = [3 + r() * 94, 30 + r() * 300, 4 + r() * 5, 14 + r() * 14, -r() * 28];
          return `<i style="--x: ${x.toFixed(1)}%; --y: ${y.toFixed(0)}px; --s: ${s.toFixed(1)}px; --dur: ${dur.toFixed(0)}s; --d: ${d.toFixed(0)}s"></i>`;
        }).join("")}</div>`,
      };
    },
  },
  // A wet shimmer along the top of the controls: lamplight in the puddles.
  puddle: {
    css: `.ly-puddle { position: absolute; left: 12px; right: 12px; top: calc(var(--controls-y, 1080px) - 8px); height: 10px; filter: blur(2.5px);
        background: repeating-linear-gradient(90deg, transparent 0 31px, rgba(255, 214, 160, 0.26) 31px 44px, transparent 44px 97px),
          repeating-linear-gradient(90deg, transparent 0 13px, rgba(255, 226, 185, 0.18) 13px 18px, transparent 18px 59px);
        background-size: 291px 100%, 177px 100%; -webkit-mask-image: linear-gradient(90deg, transparent, #000 10%, #000 90%, transparent);
        animation: ly-puddle 9s linear infinite; }
      @keyframes ly-puddle { to { background-position: 291px 0, -177px 0; } }`,
    render: () => ({ front: `<div class="ly-puddle"></div>` }),
  },
  // Gusts: pale sheets of rain sweeping across now and then.
  gusts: {
    css: `.ly-gusts i { position: absolute; top: 0; height: var(--edge-y, 220px); width: 55%; left: -70%; transform: skewX(-22deg);
        background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.16), transparent); animation: ly-gust var(--dur) ease-in infinite; animation-delay: var(--d); }
      @keyframes ly-gust { 0% { transform: skewX(-22deg) translateX(0); opacity: 0; } 15% { opacity: 1; } 60% { opacity: 1; }
        100% { transform: skewX(-22deg) translateX(330%); opacity: 0; } }`,
    render: () => ({ back: `<div class="ly-gusts"><i style="--dur: 6.5s; --d: 0s"></i><i style="--dur: 9s; --d: -4s"></i></div>` }),
  },
  // Runoff: drops gathering on the sheet's top edge and dripping down the
  // glass.
  drips: {
    css: `.ly-drips i { position: absolute; top: 0; left: var(--x); width: 5px; height: 7px; margin-left: -2.5px; border-radius: 50% 50% 50% 50% / 40% 40% 60% 60%;
        background: radial-gradient(circle at 40% 35%, rgba(255, 255, 255, 0.9), rgba(255, 255, 255, 0.3) 55%, rgba(255, 255, 255, 0.08));
        transform-origin: 50% 0; opacity: 0; animation: ly-drip var(--dur) ease-in infinite; animation-delay: var(--d); }
      @keyframes ly-drip { 0% { opacity: 0; transform: scale(0.2); } 50% { opacity: 0.9; transform: scale(1); } 70% { transform: scale(1, 1.3) translateY(0); }
        100% { opacity: 0; transform: scale(0.8, 1.6) translateY(70px); } }`,
    render: () => {
      const r = rng(1717);
      return {
        edge: `<div class="ly-drips">${Array.from({ length: 10 }, () => {
          const [x, dur, d] = [3 + r() * 94, 2.4 + r() * 2.6, -r() * 5];
          return `<i style="--x: ${x.toFixed(1)}%; --dur: ${dur.toFixed(2)}s; --d: ${d.toFixed(2)}s"></i>`;
        }).join("")}</div>`,
      };
    },
  },
  // Snow drifting down to the calendar sheet's top edge, each flake swaying.
  // params: density (≤70), size (px, largest flakes), speed (s to fall),
  // opacity, gust (sideways drift, for flurries), tint (day, night, lamp).
  snowfall: {
    css: `.ly-snowfall { position: absolute; left: -10%; right: -10%; top: 0; height: var(--edge-y, 220px); overflow: hidden; }
      .ly-snowfall i { position: absolute; top: -12px; left: var(--x); animation: ly-snowfall var(--dur) linear infinite; animation-delay: var(--d); }
      .ly-snowfall i::before { content: ""; display: block; width: var(--s); height: var(--s); border-radius: 50%;
        background: radial-gradient(circle, var(--c) 0 45%, transparent 70%); animation: ly-sway var(--sw) ease-in-out infinite alternate; }
      @keyframes ly-snowfall { to { transform: translate(var(--gx), calc(var(--edge-y, 220px) + 12px)); } }
      @keyframes ly-sway { from { transform: translateX(-9px); } to { transform: translateX(9px); } }`,
    render: (ctx, p) => {
      const n = Math.min(70, p.density ?? 40);
      const tint = { day: "rgba(255, 255, 255, 0.95)", night: "rgba(225, 233, 250, 0.85)", lamp: "rgba(255, 228, 190, 0.92)" }[
        p.tint || (ctx.dark ? "night" : "day")
      ];
      const r = rng(523 + n);
      const flakes = Array.from({ length: n }, () => {
        const s = 2 + r() * ((p.size ?? 6) - 2);
        return {
          x: r() * 100,
          s,
          // Bigger flakes fall a little faster.
          dur: (p.speed ?? 9) * (1.2 - s / 20) * (0.75 + r() * 0.5),
          d: -r() * 12,
          sw: 2 + r() * 2.5,
          gx: p.gust ? 60 + r() * 80 : (r() - 0.5) * 30,
        };
      });
      return {
        back: `<div class="ly-snowfall" style="--c: ${tint}; opacity: ${p.opacity ?? 1}">${flakes
          .map(
            (f) =>
              `<i style="--x: ${f.x.toFixed(1)}%; --s: ${f.s.toFixed(1)}px; --dur: ${f.dur.toFixed(1)}s; --d: ${f.d.toFixed(1)}s; --sw: ${f.sw.toFixed(1)}s; --gx: ${f.gx.toFixed(0)}px"></i>`,
          )
          .join("")}</div>`,
      };
    },
  },
  // A snow ridge on the calendar sheet's top edge; with `grow`, it builds
  // through the day (thin in the morning, deepest by evening). params:
  // depth (px), grow.
  snowcap: {
    css: `.ly-snowcap { position: absolute; left: 6px; right: 6px; bottom: -3px; height: var(--h); border-radius: 12px 12px 3px 3px;
        background: linear-gradient(180deg, #ffffff, #e6edf6); box-shadow: 0 1px 2px rgba(0, 0, 0, 0.18), inset 0 -2px 3px rgba(150, 170, 200, 0.35);
        -webkit-mask-image: radial-gradient(ellipse 34px var(--h) at 6% 100%, #000 98%, transparent), radial-gradient(ellipse 60px var(--h) at 26% 100%, #000 98%, transparent),
          radial-gradient(ellipse 48px var(--h) at 46% 100%, #000 98%, transparent), radial-gradient(ellipse 70px var(--h) at 68% 100%, #000 98%, transparent),
          radial-gradient(ellipse 52px var(--h) at 92% 100%, #000 98%, transparent), linear-gradient(#000, #000);
        -webkit-mask-size: 100% 100%, 100% 100%, 100% 100%, 100% 100%, 100% 100%, 100% 55%; -webkit-mask-position: 0 0, 0 0, 0 0, 0 0, 0 0, 0 100%;
        -webkit-mask-repeat: no-repeat; }
      .screen.dark .ly-snowcap { background: linear-gradient(180deg, #d9e2f1, #aab7cc); }`,
    render: (ctx, p) => {
      const grow = p.grow ? Math.max(0.25, Math.min(1, ((ctx.hour ?? 12) - 6) / 12)) : 1;
      return { edge: `<div class="ly-snowcap" style="--h: ${((p.depth ?? 10) * grow).toFixed(1)}px"></div>` };
    },
  },
  // Hail: pellets falling fast and bouncing off the calendar's edge.
  hail: {
    css: `.ly-hail { position: absolute; left: 0; right: 0; top: 0; height: calc(var(--edge-y, 220px) + 40px); overflow: hidden; }
      .ly-hail i { position: absolute; top: -10px; left: var(--x); width: var(--s); height: var(--s); border-radius: 50%;
        background: radial-gradient(circle at 35% 30%, #ffffff, #dfe8f3 60%, #b9c8da); opacity: 0;
        animation: ly-hail var(--dur) cubic-bezier(0.4, 0, 1, 1) infinite; animation-delay: var(--d); }
      @keyframes ly-hail { 0% { opacity: 0.95; transform: translate(0, 0); } 62% { transform: translate(0, calc(var(--edge-y, 220px) + 4px)); }
        78% { transform: translate(var(--bx), calc(var(--edge-y, 220px) - 16px)); opacity: 0.9; }
        100% { transform: translate(calc(var(--bx) * 1.8), calc(var(--edge-y, 220px) + 2px)); opacity: 0; } }`,
    render: (ctx, p) => {
      const r = rng(6161);
      return {
        back: `<div class="ly-hail">${Array.from({ length: p.density ?? 30 }, () => {
          const [x, s, dur, d, bx] = [r() * 100, 2.5 + r() * 3, 0.9 + r() * 0.6, -r() * 2, (r() - 0.5) * 30];
          return `<i style="--x: ${x.toFixed(1)}%; --s: ${s.toFixed(1)}px; --dur: ${dur.toFixed(2)}s; --d: ${d.toFixed(2)}s; --bx: ${bx.toFixed(0)}px"></i>`;
        }).join("")}</div>`,
      };
    },
  },
};


// The old static rain layer's name, for any user-defined scenes using it.
LAYERS.rain = LAYERS.rainfall;
LAYERS.snow = LAYERS.snowfall;

// ---- scenes ----------------------------------------------------------------

// Today's looks, one scene per combination; later tasks add variations
// (more scenes with the same `when`).
export const SCENES = {
  "clear-day": { when: { group: "clear", phase: LIGHT }, layers: [] },
  "clear-dawn-morning-star": {
    when: { group: "clear", phase: "dawn", planets: ["venus", "jupiter"] },
    layers: [["planets", { only: ["venus", "jupiter"], labels: true }]],
  },
  "clear-dusk": { when: { group: "clear", phase: "dusk" }, layers: [["stars", { density: 45 }], ["planets", {}], ["moon", {}]] },
  "clear-dusk-evening-star": {
    when: { group: "clear", phase: "dusk", planets: ["venus", "jupiter"] },
    layers: [["stars", { density: 30 }], ["planets", { only: ["venus", "jupiter"], labels: true }], ["moon", {}]],
  },
  "clear-night-moonlit": {
    when: { group: "clear", phase: "night" },
    layers: [["stars", { density: 45 }], ["planets", {}], ["moon", { glow: true }]],
    events: ["shooting_star"],
  },
  "clear-night-starfield": {
    when: { group: "clear", phase: "night" },
    layers: [["stars", { density: 110 }], ["planets", {}], ["moon", {}]],
    events: ["shooting_star"],
  },
  "clear-night-planets": {
    when: { group: "clear", phase: ["night", "late"], planets: true },
    layers: [["stars", { density: 70 }], ["planets", { labels: true }], ["moon", {}]],
    events: ["shooting_star"],
  },
  "clear-night-aurora": {
    when: { group: "clear", phase: ["night", "late"], aurora: true },
    priority: 1,
    layers: [["aurora", {}], ["stars", { density: 70 }], ["planets", {}], ["moon", {}]],
    events: ["shooting_star"],
  },
  "clear-late-milkyway": {
    when: { group: "clear", phase: "late", moon: "dark" },
    layers: [["milkyway", {}], ["stars", { density: 120 }], ["planets", {}], ["moon", {}]],
    events: ["shooting_star"],
  },
  "clear-late-moon": {
    when: { group: "clear", phase: "late" },
    layers: [["stars", { density: 60 }], ["planets", {}], ["moon", { glow: true, size: 44 }]],
    events: ["shooting_star"],
  },
  "partly-day-parade": { when: { group: "partly", phase: "day" }, layers: [["cumulus", { count: 4 }]] },
  "partly-day-peek": { when: { group: "partly", phase: "day" }, layers: [["sunpeek", {}], ["cumulus", { count: 3 }]] },
  "partly-day-shadows": { when: { group: "partly", phase: "day" }, layers: [["cumulus", { count: 4 }], ["shadows", {}]] },
  "partly-twilight-lit": {
    when: { group: "partly", phase: ["dawn", "golden", "dusk"] },
    layers: [["stars", { density: 25 }], ["cumulus", { count: 4, tint: "lit" }]],
  },
  "partly-twilight-rays": {
    when: { group: "partly", phase: ["dawn", "golden"] },
    layers: [["rays", {}], ["cumulus", { count: 3, tint: "lit" }]],
  },
  "partly-night-patchy": {
    when: { group: "partly", phase: ["night", "late"] },
    layers: [["stars", { density: 35 }], ["planets", {}], ["moon", {}], ["cumulus", { count: 3, tint: "night", opacity: 0.75 }]],
  },
  "partly-night-moonclouds": {
    when: { group: "partly", phase: ["night", "late"], moon: "up" },
    layers: [["stars", { density: 25 }], ["moon", { glow: true }], ["cumulus", { count: 5, tint: "night", speed: 120, spread: 120 }]],
  },
  "cloudy-day-soft": { when: { group: "cloudy", phase: LIGHT }, layers: [["deck", { tone: "soft" }]] },
  "cloudy-day-bright": { when: { group: "cloudy", phase: LIGHT }, layers: [["deck", { tone: "bright" }]] },
  "cloudy-day-brooding": { when: { group: "cloudy", phase: LIGHT }, layers: [["deck", { tone: "brooding" }]] },
  "cloudy-night-cityglow": { when: { group: "cloudy", phase: DARK }, layers: [["cityglow", {}], ["deck", { tone: "night" }]] },
  "cloudy-night-halo": {
    when: { group: "cloudy", phase: DARK, moon: "up" },
    layers: [["moon", { blur: 2.5 }], ["deck", { tone: "night" }]],
  },
  "fog-day-rolling": { when: { group: "fog", phase: LIGHT }, layers: [["fog", { banks: 3 }]] },
  "fog-day-burnoff": { when: { group: "fog", phase: ["day", "golden"] }, layers: [["fog", { banks: 2, burn: true }]] },
  "fog-day-edge": { when: { group: "fog", phase: LIGHT }, layers: [["fog", { banks: 2 }], ["fogedge", {}]] },
  "fog-night-lamps": { when: { group: "fog", phase: DARK }, layers: [["lamps", {}], ["fog", { banks: 2, night: true }]] },
  "fog-night-moon": {
    when: { group: "fog", phase: DARK, moon: "up" },
    layers: [["moon", { blur: 3 }], ["fog", { banks: 3, night: true }]],
  },
  "rain-day-splash": { when: { group: "rain", phase: LIGHT }, layers: [["rainfall", { density: 45 }], ["splashes", {}]] },
  "rain-day-drizzle": {
    when: { group: "rain", phase: LIGHT },
    layers: [["rainfall", { density: 30, speed: 1.1, length: 16, opacity: 0.7, slant: 6 }], ["splashes", { count: 8, beads: 14 }]],
  },
  "rain-day-window": {
    when: { group: "rain", phase: LIGHT },
    layers: [["rainfall", { density: 35 }], ["splashes", { count: 12 }], ["windowdrops", {}]],
  },
  "rain-night-lamps": {
    when: { group: "rain", phase: DARK },
    layers: [["lamps", {}], ["rainfall", { density: 45, tint: "lamp" }], ["splashes", {}]],
  },
  "rain-night-puddles": {
    when: { group: "rain", phase: DARK },
    layers: [["rainfall", { density: 40 }], ["splashes", {}], ["puddle", {}]],
  },
  "pouring-downpour": {
    when: { group: "pouring" },
    layers: [["rainfall", { density: 85, speed: 0.45, slant: 6, length: 44 }], ["splashes", { heavy: true }]],
  },
  "pouring-gusty": {
    when: { group: "pouring" },
    layers: [["rainfall", { density: 65, speed: 0.5, slant: 24, length: 40 }], ["gusts", {}], ["splashes", { heavy: true }]],
  },
  "pouring-runoff": {
    when: { group: "pouring" },
    layers: [["rainfall", { density: 70, speed: 0.5, length: 40 }], ["splashes", { heavy: true }], ["drips", {}]],
  },
  "storm-day-distant": {
    when: { condition: "lightning-rainy", phase: LIGHT },
    layers: [["deck", { tone: "brooding" }], ["rainfall", { density: 55, speed: 0.5 }], ["splashes", { heavy: true }]],
    events: ["lightning"],
  },
  "storm-day-close": {
    when: { condition: "lightning-rainy", phase: LIGHT },
    layers: [["deck", { tone: "brooding" }], ["rainfall", { density: 60, speed: 0.48 }], ["splashes", { heavy: true }]],
    events: ["lightning", "bolt"],
  },
  "storm-night-strike": {
    when: { condition: "lightning-rainy", phase: DARK },
    layers: [["deck", { tone: "night" }], ["rainfall", { density: 55, speed: 0.5 }], ["splashes", { heavy: true }]],
    events: ["lightning", "bolt"],
  },
  "storm-night-heat": {
    when: { condition: "lightning-rainy", phase: DARK },
    layers: [["deck", { tone: "night" }], ["rainfall", { density: 35 }], ["splashes", {}]],
    events: ["heat_lightning", "lightning"],
  },
  "storm-dry": {
    when: { condition: "lightning" },
    layers: [["deck", { tone: "brooding" }]],
    events: ["heat_lightning", "lightning"],
  },
  "snow-day-gentle": { when: { group: "snow", phase: LIGHT }, layers: [["snowfall", { density: 40, size: 8, speed: 11 }]] },
  "snow-day-flurries": { when: { group: "snow", phase: LIGHT }, layers: [["snowfall", { density: 24, size: 5, speed: 6, gust: true }]] },
  "snow-day-cap": { when: { group: "snow", phase: LIGHT }, layers: [["snowfall", { density: 34 }], ["snowcap", { grow: true }]] },
  "snow-night-lamps": {
    when: { group: "snow", phase: DARK },
    layers: [["lamps", {}], ["snowfall", { density: 38, tint: "lamp" }], ["snowcap", {}]],
  },
  "snow-night-quiet": { when: { group: "snow", phase: DARK }, layers: [["snowfall", { density: 16, speed: 14, opacity: 0.85 }]] },
  "sleet-mix": {
    when: { condition: "snowy-rainy" },
    layers: [["snowfall", { density: 22, size: 4, speed: 5 }], ["rainfall", { density: 30, opacity: 0.6 }], ["splashes", { count: 8 }]],
  },
  "hail-bounce": { when: { condition: "hail" }, layers: [["deck", { tone: "brooding" }], ["hail", {}], ["splashes", { count: 6, beads: 4 }]] },
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
  if (when.aurora === true && !ctx.aurora) return false;
  // planets: true (any visible) or a list of names (one of them visible).
  if (when.planets === true && !ctx.planets.length) return false;
  if (Array.isArray(when.planets) && !ctx.planets.some((p) => when.planets.includes(p.name))) return false;
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
  // Only the highest-priority matches compete (the aurora on an active night
  // beats the everyday variations).
  const top = Math.max(...ids.map((id) => all[id].priority || 0));
  ids = ids.filter((id) => (all[id].priority || 0) === top);
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
// Lightning: light up the sky (front host), and briefly mark the page with
// .flash so props can catch the light.
const flash = (hosts, peak, ms = 700) => {
  const node = document.createElement("i");
  node.className = "ly-flash";
  node.style.setProperty("--pk", peak);
  hosts.front.append(node);
  hosts.screen?.classList.add("flash");
  setTimeout(() => hosts.screen?.classList.remove("flash"), ms * 0.6);
  return node;
};
// A forked bolt from the top of the sky down to the calendar's edge.
const boltPath = (r, h) => {
  let x = 0;
  let y = 0;
  let d = "M0 0";
  const forks = [];
  while (y < h) {
    y += 14 + r() * 22;
    x += (r() - 0.5) * 34;
    d += ` L${x.toFixed(0)} ${Math.min(y, h).toFixed(0)}`;
    if (r() < 0.28) {
      let fx = x;
      let fy = y;
      let f = `M${fx.toFixed(0)} ${fy.toFixed(0)}`;
      for (let k = 0; k < 3; k++) {
        fy += 10 + r() * 16;
        fx += (r() < 0.5 ? -1 : 1) * (8 + r() * 16);
        f += ` L${fx.toFixed(0)} ${fy.toFixed(0)}`;
      }
      forks.push(f);
    }
  }
  return { d, forks };
};

export const EVENTS = {
  // Sheet lightning: the sky flickers bright, no bolt.
  lightning: {
    css: `.ly-flash { position: fixed; inset: 0; pointer-events: none; background: rgba(235, 240, 255, 1); mix-blend-mode: screen; opacity: 0;
        animation: ly-flash 0.7s linear forwards; }
      @keyframes ly-flash { 0% { opacity: 0; } 8% { opacity: var(--pk); } 16% { opacity: 0.04; } 26% { opacity: calc(var(--pk) * 0.7); }
        44% { opacity: 0.02; } 52% { opacity: calc(var(--pk) * 0.35); } 100% { opacity: 0; } }
      .screen.flash .p-hh svg, .screen.flash .p-crow svg, .screen.flash .p-pk svg, .screen.flash .p-bat { filter: brightness(1.8) drop-shadow(0 0 3px rgba(230, 235, 255, 0.9)); }`,
    fire(hosts, ctx) {
      return [[flash(hosts, ctx.dark ? 0.24 : 0.2)], 900];
    },
  },
  // A forked bolt striking down to the calendar's edge, with a bright flash.
  bolt: {
    css: `.ly-bolt { position: absolute; top: 0; transform: translateX(-50%); overflow: visible; opacity: 0; animation: ly-bolt 0.6s linear forwards;
        filter: drop-shadow(0 0 4px rgba(210, 220, 255, 0.95)) drop-shadow(0 0 12px rgba(170, 190, 255, 0.7)); }
      .ly-bolt path { fill: none; stroke: #f6f8ff; stroke-linejoin: round; stroke-linecap: round; }
      @keyframes ly-bolt { 0% { opacity: 0; } 6% { opacity: 1; } 18% { opacity: 0.15; } 28% { opacity: 1; } 60% { opacity: 0.6; } 100% { opacity: 0; } }`,
    fire(hosts, ctx) {
      const r = rng(Date.now() & 0xffff);
      const h = (hosts.screen?.querySelector(".calendar")?.offsetTop || 220) - 6;
      const { d, forks } = boltPath(r, h);
      const wrap = document.createElement("div");
      wrap.innerHTML = `<svg class="ly-bolt" style="left: ${(15 + r() * 70).toFixed(0)}%" width="120" height="${h}" viewBox="-60 0 120 ${h}">
        <path d="${d}" stroke-width="2.6"></path>${forks.map((f) => `<path d="${f}" stroke-width="1.4"></path>`).join("")}</svg>`;
      const bolt = wrap.firstElementChild;
      hosts.back.append(bolt);
      return [[bolt, flash(hosts, ctx.dark ? 0.2 : 0.3)], 900];
    },
  },
  // Heat lightning: silent glows pulsing along the horizon.
  heat_lightning: {
    css: `.ly-heat { position: absolute; top: calc(var(--edge-y, 220px) - 150px); width: 360px; height: 220px; transform: translateX(-50%); border-radius: 50%;
        background: radial-gradient(closest-side, rgba(255, 210, 230, 0.55), rgba(255, 170, 200, 0.18) 50%, transparent); opacity: 0;
        animation: ly-heat 1.6s ease-in-out forwards; }
      @keyframes ly-heat { 0% { opacity: 0; } 12% { opacity: 0.9; } 25% { opacity: 0.2; } 38% { opacity: 0.7; } 100% { opacity: 0; } }`,
    fire(hosts) {
      const node = document.createElement("i");
      node.className = "ly-heat";
      node.style.left = `${10 + Math.random() * 80}%`;
      hosts.back.append(node);
      return [[node], 1800];
    },
  },
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
export const DEFAULT_RATES = {
  lightning: [30, 90],
  bolt: [120, 300],
  heat_lightning: [15, 45],
  shooting_star: [180, 300],
  birds: [3000, 4200],
};

// All layer and event CSS, for the card's stylesheet.
export const LAYER_CSS = [...Object.values(LAYERS), ...Object.values(EVENTS)]
  .map((l) => l.css || "")
  .join("\n");
