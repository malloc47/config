// astro.js — low-precision sky positions for the sky theme's scenery: the
// moon's position, phase and illumination (here) and the planets (task 4).
// Standard formulas (as popularized by SunCalc; Astronomical Algorithms),
// good to a fraction of a degree, which is plenty for drawing.

const rad = Math.PI / 180;
const e = rad * 23.4397; // obliquity of the ecliptic

const toDays = (ms) => ms / 864e5 - 0.5 + 2440588 - 2451545;
const rightAscension = (l, b) => Math.atan2(Math.sin(l) * Math.cos(e) - Math.tan(b) * Math.sin(e), Math.cos(l));
const declination = (l, b) => Math.asin(Math.sin(b) * Math.cos(e) + Math.cos(b) * Math.sin(e) * Math.sin(l));
const siderealTime = (d, lw) => rad * (280.16 + 360.9856235 * d) - lw;
// Azimuth from south (radians, westward positive) and altitude.
const azimuth = (H, phi, dec) => Math.atan2(Math.sin(H), Math.cos(H) * Math.sin(phi) - Math.tan(dec) * Math.cos(phi));
const altitude = (H, phi, dec) => Math.asin(Math.sin(phi) * Math.sin(dec) + Math.cos(phi) * Math.cos(dec) * Math.cos(H));

function sunCoords(d) {
  const M = rad * (357.5291 + 0.98560028 * d);
  const C = rad * (1.9148 * Math.sin(M) + 0.02 * Math.sin(2 * M) + 0.0003 * Math.sin(3 * M));
  const L = M + C + rad * 102.9372 + Math.PI;
  return { dec: declination(L, 0), ra: rightAscension(L, 0) };
}

function moonCoords(d) {
  const L = rad * (218.316 + 13.176396 * d);
  const M = rad * (134.963 + 13.064993 * d);
  const F = rad * (93.272 + 13.22935 * d);
  const l = L + rad * 6.289 * Math.sin(M);
  const b = rad * 5.128 * Math.sin(F);
  return { ra: rightAscension(l, b), dec: declination(l, b), dist: 385001 - 20905 * Math.cos(M) };
}

// Horizontal position of a body with equatorial coordinates (ra, dec) at
// time ms from (lat, lng): altitude in degrees, compass azimuth in degrees
// (0 = north, 90 = east).
export function horizontal(ra, dec, ms, lat, lng) {
  const d = toDays(ms);
  const phi = rad * lat;
  const H = siderealTime(d, rad * -lng) - ra;
  return {
    alt: altitude(H, phi, dec) / rad,
    az: (azimuth(H, phi, dec) / rad + 180 + 360) % 360,
  };
}

// The moon now: altitude/azimuth (degrees), illuminated fraction (0-1),
// phase (0 new, 0.25 first quarter, 0.5 full, 0.75 last quarter).
export function moon(ms, lat, lng) {
  const d = toDays(ms);
  const s = sunCoords(d);
  const m = moonCoords(d);
  const sdist = 149598000;
  const phi = Math.acos(Math.sin(s.dec) * Math.sin(m.dec) + Math.cos(s.dec) * Math.cos(m.dec) * Math.cos(s.ra - m.ra));
  const inc = Math.atan2(sdist * Math.sin(phi), m.dist - sdist * Math.cos(phi));
  const angle = Math.atan2(
    Math.cos(s.dec) * Math.sin(s.ra - m.ra),
    Math.sin(s.dec) * Math.cos(m.dec) - Math.cos(s.dec) * Math.sin(m.dec) * Math.cos(s.ra - m.ra),
  );
  const pos = horizontal(m.ra, m.dec, ms, lat, lng);
  return {
    ...pos,
    illum: (1 + Math.cos(inc)) / 2,
    phase: 0.5 + (0.5 * inc * (angle < 0 ? -1 : 1)) / Math.PI,
  };
}

// Planets: JPL's approximate Keplerian elements (Standish; valid 1800-2050):
// [a, e, I, L, long. perihelion, long. node] at J2000 and per century.
const ELEMENTS = {
  mercury: [[0.38709927, 0.20563593, 7.00497902, 252.2503235, 77.45779628, 48.33076593],
    [0.00000037, 0.00001906, -0.00594749, 149472.67411175, 0.16047689, -0.12534081]],
  venus: [[0.72333566, 0.00677672, 3.39467605, 181.9790995, 131.60246718, 76.67984255],
    [0.0000039, -0.00004107, -0.0007889, 58517.81538729, 0.00268329, -0.27769418]],
  earth: [[1.00000261, 0.01671123, -0.00001531, 100.46457166, 102.93768193, 0],
    [0.00000562, -0.00004392, -0.01294668, 35999.37244981, 0.32327364, 0]],
  mars: [[1.52371034, 0.0933941, 1.84969142, -4.55343205, -23.94362959, 49.55953891],
    [0.00001847, 0.00007882, -0.00813131, 19140.30268499, 0.44441088, -0.29257343]],
  jupiter: [[5.202887, 0.04838624, 1.30439695, 34.39644051, 14.72847983, 100.47390909],
    [-0.00011607, -0.00013253, -0.00183714, 3034.74612775, 0.21252668, 0.20469106]],
  saturn: [[9.53667594, 0.05386179, 2.48599187, 49.95424423, 92.59887831, 113.66242448],
    [-0.0012506, -0.00050991, 0.00193609, 1222.49362201, -0.41897216, -0.28867794]],
};
// Rough brightness (magnitude at 1 AU from both, H) and drawing tint.
const LOOK = {
  mercury: { H: -0.6, tint: "#e8dcc8" },
  venus: { H: -4.4, tint: "#fffbea" },
  mars: { H: -1.5, tint: "#ffb38a" },
  jupiter: { H: -9.4, tint: "#fff1d6" },
  saturn: { H: -8.9, tint: "#f6e3b0" },
};

// Heliocentric ecliptic position (AU) of a planet at T Julian centuries.
function helio(name, T) {
  const [base, rate] = ELEMENTS[name];
  const [a, ec, I, L, w, O] = base.map((v, i) => v + rate[i] * T);
  const M = (((L - w) % 360) + 540) % 360 - 180;
  let E = M * rad + ec * Math.sin(M * rad);
  for (let k = 0; k < 6; k++) E -= (E - ec * Math.sin(E) - M * rad) / (1 - ec * Math.cos(E));
  const xp = a * (Math.cos(E) - ec);
  const yp = a * Math.sqrt(1 - ec * ec) * Math.sin(E);
  const om = (w - O) * rad;
  const Or = O * rad;
  const Ir = I * rad;
  const [co, so, cO, sO, cI, sI] = [Math.cos(om), Math.sin(om), Math.cos(Or), Math.sin(Or), Math.cos(Ir), Math.sin(Ir)];
  return [
    (co * cO - so * sO * cI) * xp + (-so * cO - co * sO * cI) * yp,
    (co * sO + so * cO * cI) * xp + (-so * sO + co * cO * cI) * yp,
    so * sI * xp + co * sI * yp,
  ];
}

// The bright planets now: altitude/azimuth (degrees), approximate magnitude
// and elongation from the sun (degrees), and a tint for drawing.
export function planets(ms, lat, lng) {
  const T = toDays(ms) / 36525;
  const earth = helio("earth", T);
  return Object.keys(LOOK).map((name) => {
    const p = helio(name, T);
    const g = p.map((v, i) => v - earth[i]);
    const delta = Math.hypot(...g);
    const r = Math.hypot(...p);
    const ra = Math.atan2(g[1] * Math.cos(e) - g[2] * Math.sin(e), g[0]);
    const dec = Math.asin((g[1] * Math.sin(e) + g[2] * Math.cos(e)) / delta);
    // Elongation: the angle sun-earth-planet.
    const re = Math.hypot(...earth);
    const elong = Math.acos(Math.max(-1, Math.min(1, (re * re + delta * delta - r * r) / (2 * re * delta)))) / rad;
    const phaseAngle = Math.acos(Math.max(-1, Math.min(1, (r * r + delta * delta - re * re) / (2 * r * delta))));
    const mag = LOOK[name].H + 5 * Math.log10(r * delta) + (name === "venus" || name === "mercury" ? 2 * (phaseAngle / Math.PI) : 0);
    return { name, ...horizontal(ra, dec, ms, lat, lng), mag, elong, tint: LOOK[name].tint, ra: ra / rad, dec: dec / rad };
  });
}

export const _internal = { toDays, rad, e, rightAscension, declination };
