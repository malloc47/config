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

export const _internal = { toDays, rad, e, rightAscension, declination };
