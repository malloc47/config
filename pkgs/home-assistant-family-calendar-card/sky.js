// sky.js — the "sky & glass" theme's sky and palette for family-calendar-card.
//
// skyTheme() turns the sun's elevation (and whether it is rising), the
// weather condition and the local hour into:
//   - a sky: a four-stop vertical gradient interpolated between keyframes by
//     elevation (separate morning and evening keyframes: dawn pinks, golden
//     hour ambers), greyed by the weather, plus a soft sun glow near the
//     horizon;
//   - overlays to show (stars, clouds, rain, snow, fog);
//   - a palette in the spirit of Material dynamic colour: the glass panels,
//     ink, rules and accent are tones of one seed hue taken from the sky,
//     light by day and dark from dusk, warmed late at night. Calendar colours
//     are not touched.
// All values are CSS strings, applied as custom properties by the card.

const MORNING = [
  { e: -18, h: 235, stops: ["#05070d", "#080c18", "#0b1222", "#10172a"] },
  { e: -10, h: 240, stops: ["#0f1530", "#1d2550", "#33366a", "#4a4578"] },
  { e: -4, h: 18, stops: ["#2c3a6b", "#6b4f86", "#c46d86", "#eea587"] },
  { e: 2, h: 22, stops: ["#3d5a96", "#8f7aa8", "#eaa289", "#f8d3aa"] },
  { e: 10, h: 205, stops: ["#4e86c8", "#7fb0e0", "#bcd7ef", "#f3e6d0"] },
  { e: 25, h: 212, stops: ["#2f7fd0", "#5aa2e6", "#9fcaf3", "#dcedfb"] },
];
const EVENING = [
  { e: -18, h: 222, stops: ["#0d1422", "#121b2d", "#18243a", "#1f2c44"] },
  { e: -8, h: 255, stops: ["#121a3a", "#262d5c", "#463f73", "#6a4f7e"] },
  { e: -2, h: 350, stops: ["#2a3463", "#6a4a7e", "#c2607a", "#e98a6e"] },
  { e: 4, h: 32, stops: ["#3b4e8a", "#b07a8a", "#ec9a68", "#f9cf8f"] },
  { e: 12, h: 38, stops: ["#3d7cc4", "#77a6d8", "#d6c7a8", "#f3d9a8"] },
  { e: 25, h: 212, stops: ["#2f7fd0", "#5aa2e6", "#9fcaf3", "#dcedfb"] },
];
// Late at night (22:00-05:00, sun well down): a deeper, star-lit navy.
const LATE = { h: 248, stops: ["#05070d", "#080c18", "#0b1222", "#10172a"] };

// How each condition greys the sky (0-1), darkens it, and what it overlays.
const WEATHER = {
  partlycloudy: { grey: 0.15, fx: ["clouds"] },
  cloudy: { grey: 0.6, dark: 0.05, fx: ["clouds"] },
  fog: { grey: 0.65, fx: ["fog"] },
  rainy: { grey: 0.7, dark: 0.12, fx: ["rain"] },
  pouring: { grey: 0.8, dark: 0.2, fx: ["rain"] },
  lightning: { grey: 0.7, dark: 0.2, fx: [] },
  "lightning-rainy": { grey: 0.75, dark: 0.2, fx: ["rain"] },
  snowy: { grey: 0.6, light: 0.1, fx: ["snow"] },
  "snowy-rainy": { grey: 0.65, fx: ["snow", "rain"] },
  hail: { grey: 0.65, fx: ["snow"] },
};

const rgb = (hex) => [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16));
const css = ([r, g, b]) => `rgb(${Math.round(r)}, ${Math.round(g)}, ${Math.round(b)})`;
const mix = (a, b, t) => a.map((v, i) => v + (b[i] - v) * t);
const lerpHue = (a, b, t) => {
  const d = ((b - a + 540) % 360) - 180;
  return (a + d * t + 360) % 360;
};

// The keyframes either side of an elevation, blended.
function sky(frames, e) {
  const clamped = Math.max(frames[0].e, Math.min(frames[frames.length - 1].e, e));
  let i = 0;
  while (i < frames.length - 2 && clamped > frames[i + 1].e) i++;
  const a = frames[i];
  const b = frames[i + 1];
  const t = (clamped - a.e) / (b.e - a.e);
  return { h: lerpHue(a.h, b.h, t), stops: a.stops.map((s, k) => mix(rgb(s), rgb(b.stops[k]), t)) };
}

export function skyTheme({ elevation = 30, rising = true, condition = "sunny", hour = 12, glass = 0.84 }) {
  const e = Number.isFinite(elevation) ? elevation : 30;
  const late = e < -12 && (hour >= 22 || hour < 5);
  let { h, stops } = late ? { h: LATE.h, stops: LATE.stops.map(rgb) } : sky(rising ? MORNING : EVENING, e);

  // Weather: grey each stop toward its own luminance, darken or lighten.
  const w = WEATHER[condition] || { grey: 0, fx: [] };
  stops = stops.map((c) => {
    const y = 0.2126 * c[0] + 0.7152 * c[1] + 0.0722 * c[2];
    let out = mix(c, [y, y, y], w.grey || 0);
    if (w.dark) out = mix(out, [0, 0, 0], w.dark);
    if (w.light) out = mix(out, [255, 255, 255], w.light);
    return out;
  });
  const chroma = 1 - (w.grey || 0) * 0.6;

  const dark = e < -4;
  const warm = dark && late;
  const fx = new Set(w.fx);
  if (dark && (condition === "clear-night" || condition === "sunny")) fx.add("stars");
  if (dark) fx.delete("clouds");

  // A soft sun glow near the horizon: right in the morning, left in the evening.
  let glow = "";
  if (e > -6 && !w.grey) {
    const y = Math.max(4, Math.min(36, 36 - e * 1.3));
    const x = rising ? 84 : 16;
    const a = e < 15 ? 0.75 : 0.55;
    const tint = e < 15 ? "255, 210, 150" : "255, 250, 225";
    glow = `radial-gradient(70% 30% at ${x}% ${y}%, rgba(${tint}, ${a}), transparent 70%), `;
  }

  const s = (n) => Math.round(n * chroma);
  const hh = Math.round(h);
  const t = dark
    ? {
        paper: `hsl(${hh}, 26%, 11%)`,
        glass: `hsla(${hh}, 26%, 11%, ${Math.min(1, glass + 0.02)})`,
        glass2: `hsla(${hh}, 26%, 19%, 0.66)`,
        ink: warm ? "hsl(36, 38%, 86%)" : `hsl(${hh}, 22%, 92%)`,
        ink2: warm ? "hsl(34, 16%, 64%)" : `hsl(${hh}, 14%, 70%)`,
        rule: `hsla(${hh}, 40%, 86%, 0.10)`,
        accent: warm ? "hsl(34, 62%, 66%)" : `hsl(${hh}, 80%, 74%)`,
        onAccent: `hsl(${hh}, 40%, 12%)`,
        accentSoft: `hsla(${hh}, 80%, 74%, 0.10)`,
        edge: "rgba(255, 255, 255, 0.08)",
        shadow: "0 14px 40px rgba(0, 0, 0, 0.5)",
        banner: warm ? "hsla(36, 40%, 84%, 0.92)" : `hsla(${hh}, 30%, 90%, 0.92)`,
        onBanner: `hsl(${hh}, 34%, 12%)`,
        onSky: warm ? "#efe2cd" : "#e9eef7",
        onSky2: warm ? "rgba(225, 210, 188, 0.74)" : "rgba(214, 224, 240, 0.78)",
        skyShadow: "none",
      }
    : {
        paper: `hsl(${hh}, ${s(38)}%, 97%)`,
        glass: `hsla(${hh}, ${s(38)}%, 97%, ${glass})`,
        glass2: `hsla(${hh}, ${s(46)}%, 90%, 0.62)`,
        ink: `hsl(${hh}, ${s(30)}%, 14%)`,
        ink2: `hsl(${hh}, ${s(14)}%, 35%)`,
        rule: `hsla(${hh}, ${s(30)}%, 25%, 0.10)`,
        accent: `hsl(${hh}, ${s(64)}%, 38%)`,
        onAccent: "#ffffff",
        accentSoft: `hsla(${hh}, ${s(70)}%, 45%, 0.12)`,
        edge: "rgba(255, 255, 255, 0.7)",
        shadow: "0 14px 40px rgba(25, 30, 60, 0.20)",
        banner: `hsla(${hh}, ${s(30)}%, 12%, 0.88)`,
        onBanner: `hsl(${hh}, ${s(30)}%, 96%)`,
        onSky: "#ffffff",
        onSky2: "rgba(255, 255, 255, 0.86)",
        skyShadow: "0 1px 12px rgba(20, 30, 60, 0.35)",
      };
  t.sky = `${glow}linear-gradient(180deg, ${css(stops[0])} 0%, ${css(stops[1])} 30%, ${css(stops[2])} 62%, ${css(stops[3])} 100%)`;
  t.rainOpacity = dark ? "0.45" : "0.9";
  return { vars: t, fx: [...fx], dark };
}
