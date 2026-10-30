// kindle-weather-card — a full-screen Lovelace card laid out for a wall-mounted
// Kindle Paperwhite 3 (1072x1448 portrait, 300 dpi, 16 grays). Meant to fill a
// panel view that Puppet screenshots with the panel's 16-level palette
// (colors=000000,111111,...,FFFFFF); the Kindle's dashboard loop
// (hosts/aida/kindle-weather) wakes from suspend, downloads that image, draws
// it and suspends again, so the card shows nothing that needs a live clock.
//
// The 16 grays carry meaning rather than just anti-aliasing: temperature is
// shaded on one absolute 14-step ramp (cold dark, warm light;
// TEMP_* below) under the hourly curve, in each day's low-high bar and in the
// legend that explains it; precipitation chance darkens its bars; night hours
// are a pale band behind the hourly chart. Black text on white keeps full
// contrast, with #555555 secondary text and #aaaaaa rules.
//
// Every render depends only on the forecast and the current hour, so an
// unchanged forecast renders identically and Puppet's ETag lets the Kindle skip
// redrawing (no "Updated" stamp).
//
// Config:
//   type: custom:kindle-weather-card
//   weather_entity: weather.nws_khpn             # current conditions + forecasts
//   fallback_weather_entity: weather.forecast_home  # optional; used while the primary is down
//   inside_entity: sensor.<indoor temperature>   # optional
//   sun_entity: sun.sun                          # sunrise/sunset and night shading
//   days: 5                                      # daily forecast rows (from tomorrow)
//   hours: 24                                    # hourly chart span
//   rain_probability, outdoor_hours, cold, hot   # outlook rows; see ../lovelace-common/weather.js
//   hide_toasts: true                            # hide HA's toasts while shown
//
// Icons are Material Design Icons (https://pictogrammers.com/library/mdi/,
// Apache-2.0), embedded as SVG paths so the card has no runtime dependencies.

const { CONDITION_ICON, CONDITION_TEXT, Weather, num, temp } = await import(
  new URL(`./weather.js${new URL(import.meta.url).search}`, import.meta.url).href
);

const ICONS = {
  "weather-sunny": "M12,7A5,5 0 0,1 17,12A5,5 0 0,1 12,17A5,5 0 0,1 7,12A5,5 0 0,1 12,7M12,9A3,3 0 0,0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9M12,2L14.39,5.42C13.65,5.15 12.84,5 12,5C11.16,5 10.35,5.15 9.61,5.42L12,2M3.34,7L7.5,6.65C6.9,7.16 6.36,7.78 5.94,8.5C5.5,9.24 5.25,10 5.11,10.79L3.34,7M3.36,17L5.12,13.23C5.26,14 5.53,14.78 5.95,15.5C6.37,16.24 6.91,16.86 7.5,17.37L3.36,17M20.65,7L18.88,10.79C18.74,10 18.47,9.23 18.05,8.5C17.63,7.78 17.1,7.15 16.5,6.64L20.65,7M20.64,17L16.5,17.36C17.09,16.85 17.62,16.22 18.04,15.5C18.46,14.77 18.73,14 18.87,13.21L20.64,17M12,22L9.59,18.56C10.33,18.83 11.14,19 12,19C12.82,19 13.63,18.83 14.37,18.56L12,22Z",
  "weather-night": "M17.75,4.09L15.22,6.03L16.13,9.09L13.5,7.28L10.87,9.09L11.78,6.03L9.25,4.09L12.44,4L13.5,1L14.56,4L17.75,4.09M21.25,11L19.61,12.25L20.2,14.23L18.5,13.06L16.8,14.23L17.39,12.25L15.75,11L17.81,10.95L18.5,9L19.19,10.95L21.25,11M18.97,15.95C19.8,15.87 20.69,17.05 20.16,17.8C19.84,18.25 19.5,18.67 19.08,19.07C15.17,23 8.84,23 4.94,19.07C1.03,15.17 1.03,8.83 4.94,4.93C5.34,4.53 5.76,4.17 6.21,3.85C6.96,3.32 8.14,4.21 8.06,5.04C7.79,7.9 8.75,10.87 10.95,13.06C13.14,15.26 16.1,16.22 18.97,15.95M17.33,17.97C14.5,17.81 11.7,16.64 9.53,14.5C7.36,12.31 6.2,9.5 6.04,6.68C3.23,9.82 3.34,14.64 6.35,17.66C9.37,20.67 14.19,20.78 17.33,17.97Z",
  "weather-partly-cloudy": "M12.74,5.47C15.1,6.5 16.35,9.03 15.92,11.46C17.19,12.56 18,14.19 18,16V16.17C18.31,16.06 18.65,16 19,16A3,3 0 0,1 22,19A3,3 0 0,1 19,22H6A4,4 0 0,1 2,18A4,4 0 0,1 6,14H6.27C5,12.45 4.6,10.24 5.5,8.26C6.72,5.5 9.97,4.24 12.74,5.47M11.93,7.3C10.16,6.5 8.09,7.31 7.31,9.07C6.85,10.09 6.93,11.22 7.41,12.13C8.5,10.83 10.16,10 12,10C12.7,10 13.38,10.12 14,10.34C13.94,9.06 13.18,7.86 11.93,7.3M13.55,3.64C13,3.4 12.45,3.23 11.88,3.12L14.37,1.82L15.27,4.71C14.76,4.29 14.19,3.93 13.55,3.64M6.09,4.44C5.6,4.79 5.17,5.19 4.8,5.63L4.91,2.82L7.87,3.5C7.25,3.71 6.65,4.03 6.09,4.44M18,9.71C17.91,9.12 17.78,8.55 17.59,8L19.97,9.5L17.92,11.73C18.03,11.08 18.05,10.4 18,9.71M3.04,11.3C3.11,11.9 3.24,12.47 3.43,13L1.06,11.5L3.1,9.28C3,9.93 2.97,10.61 3.04,11.3M19,18H16V16A4,4 0 0,0 12,12A4,4 0 0,0 8,16H6A2,2 0 0,0 4,18A2,2 0 0,0 6,20H19A1,1 0 0,0 20,19A1,1 0 0,0 19,18Z",
  "weather-night-partly-cloudy": "M22,10.28C21.74,10.3 21.5,10.31 21.26,10.31C19.32,10.31 17.39,9.57 15.91,8.09C14.25,6.44 13.5,4.19 13.72,2C13.77,1.53 13.22,1 12.71,1C12.57,1 12.44,1.04 12.32,1.12C12,1.36 11.66,1.64 11.36,1.94C9.05,4.24 8.55,7.66 9.84,10.46C8.31,11.11 7.13,12.43 6.69,14.06L6,14A4,4 0 0,0 2,18A4,4 0 0,0 6,22H19A3,3 0 0,0 22,19A3,3 0 0,0 19,16C18.42,16 17.88,16.16 17.42,16.45L17.5,15.5C17.5,15.28 17.5,15.05 17.46,14.83C19.14,14.67 20.77,13.94 22.06,12.64C22.38,12.34 22.64,12 22.88,11.68C23.27,11.13 22.65,10.28 22.04,10.28M19,18A1,1 0 0,1 20,19A1,1 0 0,1 19,20H6A2,2 0 0,1 4,18A2,2 0 0,1 6,16H8.5V15.5C8.5,13.94 9.53,12.64 10.94,12.18C11.1,12.13 11.26,12.09 11.43,12.06C11.61,12.03 11.8,12 12,12C12.23,12 12.45,12.03 12.66,12.07C12.73,12.08 12.8,12.1 12.87,12.13C13,12.16 13.15,12.2 13.28,12.25C13.36,12.28 13.44,12.32 13.5,12.36C13.63,12.41 13.74,12.47 13.84,12.54C13.92,12.59 14,12.64 14.07,12.7C14.17,12.77 14.25,12.84 14.34,12.92C14.41,13 14.5,13.05 14.55,13.12C14.63,13.2 14.69,13.29 14.76,13.37C14.82,13.45 14.89,13.53 14.94,13.62C15,13.71 15.04,13.8 15.09,13.9C15.14,14 15.2,14.08 15.24,14.18C15.41,14.59 15.5,15.03 15.5,15.5V18M16.83,12.86C15.9,11.16 14.08,10 12,10H11.87C11.41,9.19 11.14,8.26 11.14,7.29C11.14,6.31 11.39,5.37 11.86,4.55C12.21,6.41 13.12,8.14 14.5,9.5C15.86,10.88 17.58,11.79 19.45,12.14C18.66,12.6 17.76,12.84 16.83,12.86Z",
  "weather-cloudy": "M6,19A5,5 0 0,1 1,14A5,5 0 0,1 6,9C7,6.65 9.3,5 12,5C15.43,5 18.24,7.66 18.5,11.03L19,11A4,4 0 0,1 23,15A4,4 0 0,1 19,19H6M19,13H17V12A5,5 0 0,0 12,7C9.5,7 7.45,8.82 7.06,11.19C6.73,11.07 6.37,11 6,11A3,3 0 0,0 3,14A3,3 0 0,0 6,17H19A2,2 0 0,0 21,15A2,2 0 0,0 19,13Z",
  "weather-fog": "M3,15H13A1,1 0 0,1 14,16A1,1 0 0,1 13,17H3A1,1 0 0,1 2,16A1,1 0 0,1 3,15M16,15H21A1,1 0 0,1 22,16A1,1 0 0,1 21,17H16A1,1 0 0,1 15,16A1,1 0 0,1 16,15M1,12A5,5 0 0,1 6,7C7,4.65 9.3,3 12,3C15.43,3 18.24,5.66 18.5,9.03L19,9C21.19,9 22.97,10.76 23,13H21A2,2 0 0,0 19,11H17V10A5,5 0 0,0 12,5C9.5,5 7.45,6.82 7.06,9.19C6.73,9.07 6.37,9 6,9A3,3 0 0,0 3,12C3,12.35 3.06,12.69 3.17,13H1.1L1,12M3,19H5A1,1 0 0,1 6,20A1,1 0 0,1 5,21H3A1,1 0 0,1 2,20A1,1 0 0,1 3,19M8,19H21A1,1 0 0,1 22,20A1,1 0 0,1 21,21H8A1,1 0 0,1 7,20A1,1 0 0,1 8,19Z",
  "weather-hail": "M6,14A1,1 0 0,1 7,15A1,1 0 0,1 6,16A5,5 0 0,1 1,11A5,5 0 0,1 6,6C7,3.65 9.3,2 12,2C15.43,2 18.24,4.66 18.5,8.03L19,8A4,4 0 0,1 23,12A4,4 0 0,1 19,16H18A1,1 0 0,1 17,15A1,1 0 0,1 18,14H19A2,2 0 0,0 21,12A2,2 0 0,0 19,10H17V9A5,5 0 0,0 12,4C9.5,4 7.45,5.82 7.06,8.19C6.73,8.07 6.37,8 6,8A3,3 0 0,0 3,11A3,3 0 0,0 6,14M10,18A2,2 0 0,1 12,20A2,2 0 0,1 10,22A2,2 0 0,1 8,20A2,2 0 0,1 10,18M14.5,16A1.5,1.5 0 0,1 16,17.5A1.5,1.5 0 0,1 14.5,19A1.5,1.5 0 0,1 13,17.5A1.5,1.5 0 0,1 14.5,16M10.5,12A1.5,1.5 0 0,1 12,13.5A1.5,1.5 0 0,1 10.5,15A1.5,1.5 0 0,1 9,13.5A1.5,1.5 0 0,1 10.5,12Z",
  "weather-lightning": "M6,16A5,5 0 0,1 1,11A5,5 0 0,1 6,6C7,3.65 9.3,2 12,2C15.43,2 18.24,4.66 18.5,8.03L19,8A4,4 0 0,1 23,12A4,4 0 0,1 19,16H18A1,1 0 0,1 17,15A1,1 0 0,1 18,14H19A2,2 0 0,0 21,12A2,2 0 0,0 19,10H17V9A5,5 0 0,0 12,4C9.5,4 7.45,5.82 7.06,8.19C6.73,8.07 6.37,8 6,8A3,3 0 0,0 3,11A3,3 0 0,0 6,14H7A1,1 0 0,1 8,15A1,1 0 0,1 7,16H6M12,11H15L13,15H15L11.25,22L12,17H9.5L12,11Z",
  "weather-lightning-rainy": "M4.5,13.59C5,13.87 5.14,14.5 4.87,14.96C4.59,15.44 4,15.6 3.5,15.33V15.33C2,14.47 1,12.85 1,11A5,5 0 0,1 6,6C7,3.65 9.3,2 12,2C15.43,2 18.24,4.66 18.5,8.03L19,8A4,4 0 0,1 23,12A4,4 0 0,1 19,16A1,1 0 0,1 18,15A1,1 0 0,1 19,14A2,2 0 0,0 21,12A2,2 0 0,0 19,10H17V9A5,5 0 0,0 12,4C9.5,4 7.45,5.82 7.06,8.19C6.73,8.07 6.37,8 6,8A3,3 0 0,0 3,11C3,12.11 3.6,13.08 4.5,13.6V13.59M9.5,11H12.5L10.5,15H12.5L8.75,22L9.5,17H7L9.5,11M17.5,18.67C17.5,19.96 16.5,21 15.25,21C14,21 13,19.96 13,18.67C13,17.12 15.25,14.5 15.25,14.5C15.25,14.5 17.5,17.12 17.5,18.67Z",
  "weather-pouring": "M9,12C9.53,12.14 9.85,12.69 9.71,13.22L8.41,18.05C8.27,18.59 7.72,18.9 7.19,18.76C6.65,18.62 6.34,18.07 6.5,17.54L7.78,12.71C7.92,12.17 8.47,11.86 9,12M13,12C13.53,12.14 13.85,12.69 13.71,13.22L11.64,20.95C11.5,21.5 10.95,21.8 10.41,21.66C9.88,21.5 9.56,20.97 9.7,20.43L11.78,12.71C11.92,12.17 12.47,11.86 13,12M17,12C17.53,12.14 17.85,12.69 17.71,13.22L16.41,18.05C16.27,18.59 15.72,18.9 15.19,18.76C14.65,18.62 14.34,18.07 14.5,17.54L15.78,12.71C15.92,12.17 16.47,11.86 17,12M17,10V9A5,5 0 0,0 12,4C9.5,4 7.45,5.82 7.06,8.19C6.73,8.07 6.37,8 6,8A3,3 0 0,0 3,11C3,12.11 3.6,13.08 4.5,13.6V13.59C5,13.87 5.14,14.5 4.87,14.96C4.59,15.43 4,15.6 3.5,15.32V15.33C2,14.47 1,12.85 1,11A5,5 0 0,1 6,6C7,3.65 9.3,2 12,2C15.43,2 18.24,4.66 18.5,8.03L19,8A4,4 0 0,1 23,12C23,13.5 22.2,14.77 21,15.46V15.46C20.5,15.73 19.91,15.57 19.63,15.09C19.36,14.61 19.5,14 20,13.72V13.73C20.6,13.39 21,12.74 21,12A2,2 0 0,0 19,10H17Z",
  "weather-rainy": "M6,14.03A1,1 0 0,1 7,15.03C7,15.58 6.55,16.03 6,16.03C3.24,16.03 1,13.79 1,11.03C1,8.27 3.24,6.03 6,6.03C7,3.68 9.3,2.03 12,2.03C15.43,2.03 18.24,4.69 18.5,8.06L19,8.03A4,4 0 0,1 23,12.03C23,14.23 21.21,16.03 19,16.03H18C17.45,16.03 17,15.58 17,15.03C17,14.47 17.45,14.03 18,14.03H19A2,2 0 0,0 21,12.03A2,2 0 0,0 19,10.03H17V9.03C17,6.27 14.76,4.03 12,4.03C9.5,4.03 7.45,5.84 7.06,8.21C6.73,8.09 6.37,8.03 6,8.03A3,3 0 0,0 3,11.03A3,3 0 0,0 6,14.03M12,14.15C12.18,14.39 12.37,14.66 12.56,14.94C13,15.56 14,17.03 14,18C14,19.11 13.1,20 12,20A2,2 0 0,1 10,18C10,17.03 11,15.56 11.44,14.94C11.63,14.66 11.82,14.4 12,14.15M12,11.03L11.5,11.59C11.5,11.59 10.65,12.55 9.79,13.81C8.93,15.06 8,16.56 8,18A4,4 0 0,0 12,22A4,4 0 0,0 16,18C16,16.56 15.07,15.06 14.21,13.81C13.35,12.55 12.5,11.59 12.5,11.59",
  "weather-snowy": "M6,14A1,1 0 0,1 7,15A1,1 0 0,1 6,16A5,5 0 0,1 1,11A5,5 0 0,1 6,6C7,3.65 9.3,2 12,2C15.43,2 18.24,4.66 18.5,8.03L19,8A4,4 0 0,1 23,12A4,4 0 0,1 19,16H18A1,1 0 0,1 17,15A1,1 0 0,1 18,14H19A2,2 0 0,0 21,12A2,2 0 0,0 19,10H17V9A5,5 0 0,0 12,4C9.5,4 7.45,5.82 7.06,8.19C6.73,8.07 6.37,8 6,8A3,3 0 0,0 3,11A3,3 0 0,0 6,14M7.88,18.07L10.07,17.5L8.46,15.88C8.07,15.5 8.07,14.86 8.46,14.46C8.85,14.07 9.5,14.07 9.88,14.46L11.5,16.07L12.07,13.88C12.21,13.34 12.76,13.03 13.29,13.17C13.83,13.31 14.14,13.86 14,14.4L13.41,16.59L15.6,16C16.14,15.86 16.69,16.17 16.83,16.71C16.97,17.24 16.66,17.79 16.12,17.93L13.93,18.5L15.54,20.12C15.93,20.5 15.93,21.15 15.54,21.54C15.15,21.93 14.5,21.93 14.12,21.54L12.5,19.93L11.93,22.12C11.79,22.66 11.24,22.97 10.71,22.83C10.17,22.69 9.86,22.14 10,21.6L10.59,19.41L8.4,20C7.86,20.14 7.31,19.83 7.17,19.29C7.03,18.76 7.34,18.21 7.88,18.07Z",
  "weather-snowy-rainy": "M18.5,18.67C18.5,19.96 17.5,21 16.25,21C15,21 14,19.96 14,18.67C14,17.12 16.25,14.5 16.25,14.5C16.25,14.5 18.5,17.12 18.5,18.67M4,17.36C3.86,16.82 4.18,16.25 4.73,16.11L7,15.5L5.33,13.86C4.93,13.46 4.93,12.81 5.33,12.4C5.73,12 6.4,12 6.79,12.4L8.45,14.05L9.04,11.8C9.18,11.24 9.75,10.92 10.29,11.07C10.85,11.21 11.17,11.78 11,12.33L10.42,14.58L12.67,14C13.22,13.83 13.79,14.15 13.93,14.71C14.08,15.25 13.76,15.82 13.2,15.96L10.95,16.55L12.6,18.21C13,18.6 13,19.27 12.6,19.67C12.2,20.07 11.54,20.07 11.15,19.67L9.5,18L8.89,20.27C8.75,20.83 8.18,21.14 7.64,21C7.08,20.86 6.77,20.29 6.91,19.74L7.5,17.5L5.26,18.09C4.71,18.23 4.14,17.92 4,17.36M1,11A5,5 0 0,1 6,6C7,3.65 9.3,2 12,2C15.43,2 18.24,4.66 18.5,8.03L19,8A4,4 0 0,1 23,12A4,4 0 0,1 19,16A1,1 0 0,1 18,15A1,1 0 0,1 19,14A2,2 0 0,0 21,12A2,2 0 0,0 19,10H17V9A5,5 0 0,0 12,4C9.5,4 7.45,5.82 7.06,8.19C6.73,8.07 6.37,8 6,8A3,3 0 0,0 3,11C3,11.85 3.35,12.61 3.91,13.16C4.27,13.55 4.26,14.16 3.88,14.54C3.5,14.93 2.85,14.93 2.47,14.54C1.56,13.63 1,12.38 1,11Z",
  "weather-windy": "M4,10A1,1 0 0,1 3,9A1,1 0 0,1 4,8H12A2,2 0 0,0 14,6A2,2 0 0,0 12,4C11.45,4 10.95,4.22 10.59,4.59C10.2,5 9.56,5 9.17,4.59C8.78,4.2 8.78,3.56 9.17,3.17C9.9,2.45 10.9,2 12,2A4,4 0 0,1 16,6A4,4 0 0,1 12,10H4M19,12A1,1 0 0,0 20,11A1,1 0 0,0 19,10C18.72,10 18.47,10.11 18.29,10.29C17.9,10.68 17.27,10.68 16.88,10.29C16.5,9.9 16.5,9.27 16.88,8.88C17.42,8.34 18.17,8 19,8A3,3 0 0,1 22,11A3,3 0 0,1 19,14H5A1,1 0 0,1 4,13A1,1 0 0,1 5,12H19M18,18H4A1,1 0 0,1 3,17A1,1 0 0,1 4,16H18A3,3 0 0,1 21,19A3,3 0 0,1 18,22C17.17,22 16.42,21.66 15.88,21.12C15.5,20.73 15.5,20.1 15.88,19.71C16.27,19.32 16.9,19.32 17.29,19.71C17.47,19.89 17.72,20 18,20A1,1 0 0,0 19,19A1,1 0 0,0 18,18Z",
  "weather-windy-variant": "M6,6L6.69,6.06C7.32,3.72 9.46,2 12,2A5.5,5.5 0 0,1 17.5,7.5L17.42,8.45C17.88,8.16 18.42,8 19,8A3,3 0 0,1 22,11A3,3 0 0,1 19,14H6A4,4 0 0,1 2,10A4,4 0 0,1 6,6M6,8A2,2 0 0,0 4,10A2,2 0 0,0 6,12H19A1,1 0 0,0 20,11A1,1 0 0,0 19,10H15.5V7.5A3.5,3.5 0 0,0 12,4A3.5,3.5 0 0,0 8.5,7.5V8H6M18,18H4A1,1 0 0,1 3,17A1,1 0 0,1 4,16H18A3,3 0 0,1 21,19A3,3 0 0,1 18,22C17.17,22 16.42,21.66 15.88,21.12C15.5,20.73 15.5,20.1 15.88,19.71C16.27,19.32 16.9,19.32 17.29,19.71C17.47,19.89 17.72,20 18,20A1,1 0 0,0 19,19A1,1 0 0,0 18,18Z",
  "alert-circle-outline": "M11,15H13V17H11V15M11,7H13V13H11V7M12,2C6.47,2 2,6.5 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M12,20A8,8 0 0,1 4,12A8,8 0 0,1 12,4A8,8 0 0,1 20,12A8,8 0 0,1 12,20Z",
  "home-thermometer-outline": "M19 8C20.11 8 21 8.9 21 10V16.76C21.61 17.31 22 18.11 22 19C22 20.66 20.66 22 19 22C17.34 22 16 20.66 16 19C16 18.11 16.39 17.31 17 16.76V10C17 8.9 17.9 8 19 8M19 9C18.45 9 18 9.45 18 10V11H20V10C20 9.45 19.55 9 19 9M12 5.69L7 10.19V18H14.1L14 19L14.1 20H5V12H2L12 3L16.4 6.96C15.89 7.4 15.5 7.97 15.25 8.61L12 5.69Z",
  "water-percent": "M12,3.25C12,3.25 6,10 6,14C6,17.32 8.69,20 12,20A6,6 0 0,0 18,14C18,10 12,3.25 12,3.25M14.47,9.97L15.53,11.03L9.53,17.03L8.47,15.97M9.75,10A1.25,1.25 0 0,1 11,11.25A1.25,1.25 0 0,1 9.75,12.5A1.25,1.25 0 0,1 8.5,11.25A1.25,1.25 0 0,1 9.75,10M14.25,14.5A1.25,1.25 0 0,1 15.5,15.75A1.25,1.25 0 0,1 14.25,17A1.25,1.25 0 0,1 13,15.75A1.25,1.25 0 0,1 14.25,14.5Z",
  "water": "M12,20A6,6 0 0,1 6,14C6,10 12,3.25 12,3.25C12,3.25 18,10 18,14A6,6 0 0,1 12,20Z",
  "weather-sunset-up": "M3,12H7A5,5 0 0,1 12,7A5,5 0 0,1 17,12H21A1,1 0 0,1 22,13A1,1 0 0,1 21,14H3A1,1 0 0,1 2,13A1,1 0 0,1 3,12M15,12A3,3 0 0,0 12,9A3,3 0 0,0 9,12H15M12,2L14.39,5.42C13.65,5.15 12.84,5 12,5C11.16,5 10.35,5.15 9.61,5.42L12,2M3.34,7L7.5,6.65C6.9,7.16 6.36,7.78 5.94,8.5C5.5,9.24 5.25,10 5.11,10.79L3.34,7M20.65,7L18.88,10.79C18.74,10 18.47,9.23 18.05,8.5C17.63,7.78 17.1,7.15 16.5,6.64L20.65,7M12.71,16.3L15.82,19.41C16.21,19.8 16.21,20.43 15.82,20.82C15.43,21.21 14.8,21.21 14.41,20.82L12,18.41L9.59,20.82C9.2,21.21 8.57,21.21 8.18,20.82C7.79,20.43 7.79,19.8 8.18,19.41L11.29,16.3C11.5,16.1 11.74,16 12,16C12.26,16 12.5,16.1 12.71,16.3Z",
  "weather-sunset-down": "M3,12H7A5,5 0 0,1 12,7A5,5 0 0,1 17,12H21A1,1 0 0,1 22,13A1,1 0 0,1 21,14H3A1,1 0 0,1 2,13A1,1 0 0,1 3,12M15,12A3,3 0 0,0 12,9A3,3 0 0,0 9,12H15M12,2L14.39,5.42C13.65,5.15 12.84,5 12,5C11.16,5 10.35,5.15 9.61,5.42L12,2M3.34,7L7.5,6.65C6.9,7.16 6.36,7.78 5.94,8.5C5.5,9.24 5.25,10 5.11,10.79L3.34,7M20.65,7L18.88,10.79C18.74,10 18.47,9.23 18.05,8.5C17.63,7.78 17.1,7.15 16.5,6.64L20.65,7M12.71,20.71L15.82,17.6C16.21,17.21 16.21,16.57 15.82,16.18C15.43,15.79 14.8,15.79 14.41,16.18L12,18.59L9.59,16.18C9.2,15.79 8.57,15.79 8.18,16.18C7.79,16.57 7.79,17.21 8.18,17.6L11.29,20.71C11.5,20.9 11.74,21 12,21C12.26,21 12.5,20.9 12.71,20.71Z",
  "arrow-up": "M13,20H11V8L5.5,13.5L4.08,12.08L12,4.16L19.92,12.08L18.5,13.5L13,8V20Z",
  "arrow-down": "M11,4H13V16L18.5,10.5L19.92,11.92L12,19.84L4.08,11.92L5.5,10.5L11,16V4Z",
  "umbrella-outline": "M12,4C8.9,4 6.18,6.03 5.3,9H18.7C17.82,6.04 15.09,4 12,4M12,2A9,9 0 0,1 21,11H13V19A3,3 0 0,1 10,22A3,3 0 0,1 7,19V18H9V19A1,1 0 0,0 10,20A1,1 0 0,0 11,19V11H3A9,9 0 0,1 12,2Z",
  "tshirt-crew-outline": "M16,21H8A1,1 0 0,1 7,20V12.07L5.7,13.12C5.31,13.5 4.68,13.5 4.29,13.12L1.46,10.29C1.07,9.9 1.07,9.27 1.46,8.88L7.34,3H9C9,4.1 10.34,5 12,5C13.66,5 15,4.1 15,3H16.66L22.54,8.88C22.93,9.27 22.93,9.9 22.54,10.29L19.71,13.12C19.32,13.5 18.69,13.5 18.3,13.12L17,12.07V20A1,1 0 0,1 16,21M20.42,9.58L16.11,5.28C15.8,5.63 15.43,5.94 15,6.2C14.16,6.7 13.13,7 12,7C10.3,7 8.79,6.32 7.89,5.28L3.58,9.58L5,11L8,9H9V19H15V9H16L19,11L20.42,9.58Z",
  "snowflake": "M20.79,13.95L18.46,14.57L16.46,13.44V10.56L18.46,9.43L20.79,10.05L21.31,8.12L19.54,7.65L20,5.88L18.07,5.36L17.45,7.69L15.45,8.82L13,7.38V5.12L14.71,3.41L13.29,2L12,3.29L10.71,2L9.29,3.41L11,5.12V7.38L8.5,8.82L6.5,7.69L5.92,5.36L4,5.88L4.47,7.65L2.7,8.12L3.22,10.05L5.55,9.43L7.55,10.56V13.45L5.55,14.58L3.22,13.96L2.7,15.89L4.47,16.36L4,18.12L5.93,18.64L6.55,16.31L8.55,15.18L11,16.62V18.88L9.29,20.59L10.71,22L12,20.71L13.29,22L14.7,20.59L13,18.88V16.62L15.5,15.17L17.5,16.3L18.12,18.63L20,18.12L19.53,16.35L21.3,15.88L20.79,13.95M9.5,10.56L12,9.11L14.5,10.56V13.44L12,14.89L9.5,13.44V10.56Z",
  "thermometer-high": "M15 13V5A3 3 0 0 0 9 5V13A5 5 0 1 0 15 13M12 4A1 1 0 0 1 13 5H11A1 1 0 0 1 12 4Z",
};

// DejaVu Sans, as on the E1001 (eink-dashboard-card): hand-hinted and clean
// when anti-aliased into the palette. Book serves weights up to 500, Bold 600
// and up; the condensed family is for the outlook text.
const FONT_FACES = [
  ["KindleSans", "DejaVuSans.ttf", [400, 500]],
  ["KindleSans", "DejaVuSans-Bold.ttf", [600, 700]],
  ["KindleSansCondensed", "DejaVuSansCondensed.ttf", [400, 500]],
  ["KindleSansCondensed", "DejaVuSansCondensed-Bold.ttf", [600, 700]],
];

// @font-face has no effect inside a shadow root, so register it on the document.
if (!document.getElementById("kindle-weather-card-font")) {
  const style = document.createElement("style");
  style.id = "kindle-weather-card-font";
  style.textContent = FONT_FACES.flatMap(([family, file, weights]) =>
    weights.map(
      (w) => `@font-face {
    font-family: "${family}";
    src: url("${new URL(`./${file}`, import.meta.url).href}") format("truetype");
    font-weight: ${w};
    font-display: block;
  }`,
    ),
  ).join("\n");
  document.head.appendChild(style);
}

const W = 1072;
const H = 1448;
// Hero row: a 200px icon, the block-digit temperature, then the condition and
// high/low right-aligned in what's left (984px inside the side padding).
const HERO_GAP = 24;
const HERO_TEMP_W = 480;

// The panel's 16 levels, #000000 (0) to #ffffff (15). Puppet maps every pixel
// to the nearest of these, so everything is drawn in exact palette grays.
const GRAY = (i) => `#${Math.round(i * 17).toString(16).padStart(2, "0").repeat(3)}`;
const INK = GRAY(0);
const PAPER = GRAY(15);
const MUTED = GRAY(5);
const AXIS = GRAY(7);
const RULE = GRAY(10);
const TRACK = GRAY(13);
const NIGHT = GRAY(14);

// Temperature shading: 14 steps of 7°F from GRAY(1) (below 10°F) to GRAY(14)
// (94°F and up), absolute so a shade means the same temperature everywhere.
const TEMP_LO = 3;
const TEMP_STEP = 7;
const TEMP_LEVELS = 14;
const tempLevel = (f) => Math.min(TEMP_LEVELS, Math.max(1, 1 + Math.floor((f - TEMP_LO) / TEMP_STEP)));
// Lower edge (°F) of a level's band.
const levelFloor = (level) => TEMP_LO + (level - 1) * TEMP_STEP;

// Precipitation chance: GRAY(12) at 10% darkening to GRAY(3) at 100%.
const precipGray = (p) => GRAY(Math.round(12 - (Math.min(100, Math.max(10, p)) - 10) / 10));

const esc = (s) =>
  String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);

const icon = (name, size, cls = "") =>
  `<svg class="icon ${cls}" viewBox="0 0 24 24" width="${size}" height="${size}" aria-hidden="true"><path d="${
    ICONS[name] || ICONS["alert-circle-outline"]
  }"/></svg>`;

const COMPASS = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"];
const compass = (deg) => (num(deg) === null ? "" : COMPASS[Math.round((((num(deg) % 360) + 360) % 360) / 45) % 8]);

// Block digits for the current temperature, as the E1001 draws its clock
// (hosts/aida/esphome/e1001-clock.yaml): the classic 5x7 digit shapes with
// every cell drawn as 2x2 square blocks (strokes two blocks wide), small gaps
// between blocks and one cell between characters. Rows are bit masks, bit 4 =
// leftmost column; "-" and "°" are narrower.
const BLOCK_GLYPHS = {
  0: [0x0e, 0x11, 0x11, 0x11, 0x11, 0x11, 0x0e],
  1: [0x04, 0x0c, 0x04, 0x04, 0x04, 0x04, 0x0e],
  2: [0x0e, 0x11, 0x01, 0x02, 0x04, 0x08, 0x1f],
  3: [0x1f, 0x02, 0x04, 0x02, 0x01, 0x11, 0x0e],
  4: [0x02, 0x06, 0x0a, 0x12, 0x1f, 0x02, 0x02],
  5: [0x1f, 0x10, 0x1e, 0x01, 0x01, 0x11, 0x0e],
  6: [0x06, 0x08, 0x10, 0x1e, 0x11, 0x11, 0x0e],
  7: [0x1f, 0x01, 0x02, 0x04, 0x08, 0x08, 0x08],
  8: [0x0e, 0x11, 0x11, 0x0e, 0x11, 0x11, 0x0e],
  9: [0x0e, 0x11, 0x11, 0x0f, 0x01, 0x02, 0x0c],
};
const BLOCK_NARROW = {
  "-": { cols: 4, rows: [0, 0, 0, 0x0f, 0, 0, 0] },
  "°": { cols: 3, rows: [0x07, 0x05, 0x07, 0, 0, 0, 0] },
};

// The text as block digits filling at most maxW x maxH px; the block pitch is
// the largest whole number of px that fits (capped at maxPitch).
const blockText = (text, maxW, maxH, maxPitch) => {
  const chars = [...text].map((c) =>
    BLOCK_NARROW[c] || { cols: 5, rows: BLOCK_GLYPHS[c] || BLOCK_GLYPHS[0].map(() => 0) },
  );
  const cols = chars.reduce((n, ch) => n + ch.cols, 0) + chars.length - 1;
  const p = Math.max(2, Math.min(maxPitch, Math.floor(maxW / (2 * cols)), Math.floor(maxH / 14)));
  const gap = Math.max(1, Math.round(p / 5));
  const b = p - gap;
  const cell = 2 * p;
  const rects = [];
  let x = 0;
  for (const ch of chars) {
    ch.rows.forEach((bits, r) => {
      for (let k = 0; k < ch.cols; k++) {
        if (!(bits & (1 << (ch.cols - 1 - k)))) continue;
        for (let dy = 0; dy < 2; dy++)
          for (let dx = 0; dx < 2; dx++)
            rects.push(`<rect x="${x + k * cell + dx * p}" y="${r * cell + dy * p}" width="${b}" height="${b}"/>`);
      }
    });
    x += (ch.cols + 1) * cell;
  }
  const w = cols * cell - gap;
  const h = 7 * cell - gap;
  return `<svg class="blocks" width="${w}" height="${h}" viewBox="0 0 ${w} ${h}" shape-rendering="crispEdges" fill="${INK}" aria-label="${esc(text)}">${rects.join("")}</svg>`;
};

// Smooth path through points without overshooting between them (monotone
// cubic, Fritsch-Carlson), so the curve never invents a peak the forecast
// doesn't have.
const monotonePath = (pts) => {
  const n = pts.length;
  if (n < 2) return pts.length ? `M${pts[0][0]},${pts[0][1]}` : "";
  const dx = [];
  const m = [];
  for (let i = 0; i < n - 1; i++) {
    dx.push(pts[i + 1][0] - pts[i][0]);
    m.push((pts[i + 1][1] - pts[i][1]) / dx[i]);
  }
  const t = [m[0]];
  for (let i = 1; i < n - 1; i++) t.push(m[i - 1] * m[i] <= 0 ? 0 : (m[i - 1] + m[i]) / 2);
  t.push(m[n - 2]);
  for (let i = 0; i < n - 1; i++) {
    if (m[i] === 0) {
      t[i] = 0;
      t[i + 1] = 0;
      continue;
    }
    const a = t[i] / m[i];
    const b = t[i + 1] / m[i];
    const s = a * a + b * b;
    if (s > 9) {
      const k = 3 / Math.sqrt(s);
      t[i] = k * a * m[i];
      t[i + 1] = k * b * m[i];
    }
  }
  let d = `M${pts[0][0].toFixed(1)},${pts[0][1].toFixed(1)}`;
  for (let i = 0; i < n - 1; i++) {
    const [x0, y0] = pts[i];
    const [x1, y1] = pts[i + 1];
    const h = dx[i] / 3;
    d += ` C${(x0 + h).toFixed(1)},${(y0 + t[i] * h).toFixed(1)} ${(x1 - h).toFixed(1)},${(y1 - t[i + 1] * h).toFixed(1)} ${x1.toFixed(1)},${y1.toFixed(1)}`;
  }
  return d;
};

// Hard-stop gradient stops shading temperatures on the ramp, for a gradient
// running from temperature `from` (offset 0) to `to` (offset 1).
const tempStops = (from, to) => {
  const span = to - from || 1;
  const off = (f) => Math.min(1, Math.max(0, (f - from) / span));
  const stops = [];
  let level = tempLevel(from);
  stops.push([0, GRAY(level)]);
  const end = tempLevel(to);
  const dir = Math.sign(end - level);
  while (level !== end) {
    const edge = dir > 0 ? levelFloor(level + 1) : levelFloor(level);
    stops.push([off(edge), GRAY(level)]);
    level += dir;
    stops.push([off(edge), GRAY(level)]);
  }
  stops.push([1, GRAY(level)]);
  return stops.map(([o, c]) => `<stop offset="${o.toFixed(4)}" stop-color="${c}"/>`).join("");
};

class KindleWeatherCard extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
    this._wx = new Weather(() => this._schedule());
    this._raf = null;
  }

  setConfig(config) {
    if (!config.weather_entity) throw new Error("weather_entity is required");
    this._config = {
      days: 5,
      hours: 24,
      sun_entity: "sun.sun",
      hide_toasts: true,
      rain_probability: 50,
      outdoor_hours: [7, 22],
      cold: 45,
      hot: 85,
      ...config,
    };
    this._wx.configure(this._config);
    this._ensureSubscriptions();
    this._schedule();
  }

  set hass(hass) {
    this._wx.setHass(hass);
    this._hass = hass;
    this._ensureSubscriptions();
    this._schedule();
  }

  connectedCallback() {
    this._ensureSubscriptions();
    this._setToastsHidden(this._config?.hide_toasts !== false);
  }

  disconnectedCallback() {
    this._wx.unsubscribe();
    this._setToastsHidden(false);
  }

  // As in eink-dashboard-card: HA's toasts ("Starting <integration>...",
  // "Connection lost") are top-layer popovers in <home-assistant>'s shadow
  // root, and Puppet would screenshot them into the image the Kindle then
  // shows for an hour; hide them while the card is on screen.
  _setToastsHidden(hidden) {
    const root = document.querySelector("home-assistant")?.shadowRoot;
    if (!root) return;
    const hideIn = (sr, css) => {
      if (!sr || sr.querySelector?.("#kindle-hide-toasts")) return;
      const style = document.createElement("style");
      style.id = "kindle-hide-toasts";
      style.textContent = css;
      sr.appendChild(style);
    };
    const hideManager = (m) => hideIn(m.shadowRoot, "ha-toast { display: none !important; }");
    if (hidden) {
      hideIn(root, "notification-manager { display: none !important; }");
      root.querySelectorAll("notification-manager").forEach(hideManager);
      if (!this._toastObserver) {
        this._toastObserver = new MutationObserver(() =>
          root.querySelectorAll("notification-manager").forEach((m) => {
            hideManager(m);
            if (!m.shadowRoot?.querySelector("#kindle-hide-toasts")) setTimeout(() => hideManager(m), 0);
          }),
        );
        this._toastObserver.observe(root, { childList: true });
      }
    } else {
      this._toastObserver?.disconnect();
      this._toastObserver = null;
      for (const sr of [root, ...[...root.querySelectorAll("notification-manager")].map((m) => m.shadowRoot)]) {
        sr?.querySelector("#kindle-hide-toasts")?.remove();
      }
    }
  }

  getCardSize() {
    return 20;
  }

  _ensureSubscriptions() {
    if (!this._hass || !this._config || !this.isConnected) return;
    this._wx.setHass(this._hass);
    this._wx.ensureSubscriptions();
  }

  _schedule() {
    if (this._raf) return;
    this._raf = requestAnimationFrame(() => {
      this._raf = null;
      this._render();
    });
  }

  _sunEvent() {
    const sun = this._hass.states[this._config.sun_entity];
    if (!sun) return null;
    const rise = Date.parse(sun.attributes.next_rising);
    const set = Date.parse(sun.attributes.next_setting);
    if (!Number.isFinite(rise) || !Number.isFinite(set)) return null;
    return rise < set
      ? { icon: "weather-sunset-up", time: this._wx.clockTime(rise), label: "Sunrise" }
      : { icon: "weather-sunset-down", time: this._wx.clockTime(set), label: "Sunset" };
  }

  // Night stretches as [start, end) timestamps covering `from`..`to`, from
  // sun.sun's next rising/setting stepped back and forward by days (the
  // minute-a-day drift doesn't matter at hourly resolution).
  _nights(from, to) {
    const sun = this._hass.states[this._config.sun_entity];
    const rise = Date.parse(sun?.attributes?.next_rising);
    const set = Date.parse(sun?.attributes?.next_setting);
    if (!Number.isFinite(rise) || !Number.isFinite(set)) return [];
    const DAY = 86400000;
    const nights = [];
    // Each night runs from a setting to the following rising.
    let s = set - Math.ceil((set - from) / DAY + 1) * DAY;
    for (; s < to; s += DAY) {
      let r = rise;
      while (r <= s) r += DAY;
      while (r - DAY > s) r -= DAY;
      if (r > from) nights.push([Math.max(s, from), Math.min(r, to)]);
    }
    return nights.filter(([a, b]) => b > a);
  }

  _hourly() {
    const hourStart = Math.floor(Date.now() / 3600000) * 3600000;
    return this._wx
      .src()
      .hourly.filter((f) => Date.parse(f.datetime) >= hourStart)
      .slice(0, this._config.hours + 1);
  }

  _days() {
    const today = this._wx.local(Date.now()).date;
    return this._wx
      .src()
      .daily.filter((f) => this._wx.local(f.datetime).date > today)
      .slice(0, this._config.days);
  }

  _todayHiLo() {
    const today = this._wx.local(Date.now()).date;
    return this._wx.src().daily.find((f) => this._wx.local(f.datetime).date === today);
  }

  _render() {
    if (!this._hass || !this._config) return;
    this._wx.invalidate();
    this._renderScreen();
    this._fitText();
    // The first layout can use a fallback font; measure again with DejaVu.
    document.fonts.ready.then(() => this._fitText());
  }

  // Outlook rows whose forecast text doesn't fit: drop the what-to-wear hint,
  // let the text wrap to two lines, try the shorter wordings, then shrink to
  // 80% before leaving the rest to the ellipsis (as eink-dashboard-card).
  _fitText() {
    // The condition wraps onto two lines; a single long word shrinks instead.
    const cond = this.shadowRoot.querySelector(".cond");
    if (cond) {
      cond.style.fontSize = "";
      for (let size = 46; size > 28 && cond.scrollWidth > cond.clientWidth + 1; size -= 2) cond.style.fontSize = `${size - 2}px`;
    }
    for (const row of this.shadowRoot.querySelectorAll("[data-fit]")) {
      const txt = row.querySelector(".txt");
      const alts = JSON.parse(row.dataset.fit);
      const over = () => txt.scrollWidth > txt.clientWidth + 1 || txt.scrollHeight > txt.clientHeight + 1;
      row.classList.remove("nowear", "wrap");
      txt.style.fontSize = "";
      txt.textContent = alts[0];
      if (!over()) continue;
      row.classList.add("nowear");
      if (!over()) continue;
      row.classList.add("wrap");
      let i = 0;
      while (over() && i < alts.length - 1) txt.textContent = alts[++i];
      const full = parseFloat(getComputedStyle(txt).fontSize);
      for (let size = full - 1; over() && size >= Math.ceil(full * 0.8); size--) txt.style.fontSize = `${size}px`;
    }
  }

  // Next `hours` hours: banded temperature area and curve over pale night
  // stretches, precipitation-chance bars below, hour labels every 3 hours.
  _chart(hours) {
    const cw = W - 2 * 44;
    const plotTop = 40;
    const plotBottom = 236;
    const precipTop = 252;
    const precipBottom = 316;
    const axisY = 350;
    const ch = 360;
    if (hours.length < 2) {
      return `<svg width="${cw}" height="${ch}"><text x="${cw / 2}" y="${ch / 2}" text-anchor="middle" class="ctext">No hourly forecast</text></svg>`;
    }
    const t0 = Date.parse(hours[0].datetime);
    const t1 = Date.parse(hours[hours.length - 1].datetime);
    const x = (ts) => ((ts - t0) / (t1 - t0)) * cw;
    const temps = hours.map((f) => num(f.temperature));
    const known = temps.filter((v) => v !== null);
    let lo = Math.min(...known);
    let hi = Math.max(...known);
    // At least a 14°F span, so a flat day doesn't look dramatic.
    if (hi - lo < 14) {
      const mid = (hi + lo) / 2;
      lo = mid - 7;
      hi = mid + 7;
    }
    lo -= 2;
    hi += 2;
    const y = (f) => plotBottom - ((f - lo) / (hi - lo)) * (plotBottom - plotTop);
    const pts = hours.map((f, i) => [x(Date.parse(f.datetime)), y(temps[i] ?? lo)]);
    const line = monotonePath(pts);
    const area = `${line} L${cw},${plotBottom} L0,${plotBottom} Z`;
    const nights = this._nights(t0, t1)
      .map(([a, b]) => `<rect x="${x(a).toFixed(1)}" y="0" width="${(x(b) - x(a)).toFixed(1)}" height="${precipBottom}" fill="${NIGHT}"/>`)
      .join("");

    // Precipitation: chance when the source gives one, else the hour's amount
    // read against 0.2 in (5 mm) as "certain".
    const unit = this._wx.src().state?.attributes?.precipitation_unit;
    const chance = (f) =>
      f.precipitation_probability != null
        ? num(f.precipitation_probability) || 0
        : Math.min(100, ((num(f.precipitation) || 0) / (unit === "mm" ? 5 : 0.2)) * 100);
    const slot = cw / (hours.length - 1);
    const bars = hours
      .slice(0, -1)
      .map((f, i) => {
        const p = chance(f);
        if (p < 5) return "";
        const h = (p / 100) * (precipBottom - precipTop);
        return `<rect x="${(i * slot + 3).toFixed(1)}" y="${(precipBottom - h).toFixed(1)}" width="${(slot - 6).toFixed(1)}" height="${h.toFixed(1)}" fill="${precipGray(p)}"/>`;
      })
      .join("");
    // Label the wettest hour of each 3-hour block when it's at least 20%,
    // inside the strip's top (haloed, so it reads over a tall bar).
    const pLabels = [];
    for (let b = 0; b < hours.length - 1; b += 3) {
      const block = hours.slice(b, Math.min(b + 3, hours.length - 1));
      let best = b;
      block.forEach((f, j) => {
        if (chance(f) > chance(hours[best])) best = b + j;
      });
      const p = Math.round(chance(hours[best]));
      if (hours[best].precipitation_probability != null && p >= 20) {
        pLabels.push(
          `<text x="${((best + 0.5) * slot).toFixed(1)}" y="${precipTop + 20}" text-anchor="middle" class="plabel">${p}%</text>`,
        );
      }
    }
    if (!bars) {
      pLabels.push(
        `<text x="${cw / 2}" y="${(precipTop + precipBottom) / 2 + 9}" text-anchor="middle" class="plabel">No precipitation expected</text>`,
      );
    }

    const labels = [];
    const ticks = [];
    hours.forEach((f, i) => {
      const h = this._wx.local(f.datetime).hour;
      if (i % 3 !== 0 || i === hours.length - 1) return;
      const px = pts[i][0];
      const anchor = i === 0 ? "start" : "middle";
      if (temps[i] !== null) {
        labels.push(
          `<text x="${px.toFixed(1)}" y="${(pts[i][1] - 14).toFixed(1)}" text-anchor="${anchor}" class="tlabel">${Math.round(temps[i])}°</text>`,
        );
      }
      ticks.push(
        `<text x="${px.toFixed(1)}" y="${axisY}" text-anchor="${anchor}" class="axis">${i === 0 ? "Now" : esc(this._wx.hourLabel(h))}</text>`,
      );
    });
    return `
      <svg width="${cw}" height="${ch}" viewBox="0 0 ${cw} ${ch}">
        <defs>
          <linearGradient id="tband" gradientUnits="userSpaceOnUse" x1="0" y1="${plotBottom}" x2="0" y2="${plotTop}">
            ${tempStops(lo, hi)}
          </linearGradient>
        </defs>
        ${nights}
        <path d="${area}" fill="url(#tband)"/>
        <path d="${line}" fill="none" stroke="${PAPER}" stroke-width="12" stroke-linejoin="round"/>
        <path d="${line}" fill="none" stroke="${INK}" stroke-width="5" stroke-linejoin="round"/>
        <line x1="0" y1="${precipBottom + 0.5}" x2="${cw}" y2="${precipBottom + 0.5}" stroke="${RULE}" stroke-width="1"/>
        ${bars}
        ${pLabels.join("")}
        ${labels.join("")}
        ${ticks.join("")}
      </svg>`;
  }

  // A day's low-high bar on the week's common scale, shaded on the
  // temperature ramp.
  _dayBar(d, wlo, whi, id) {
    const bw = 400;
    const bh = 26;
    const lo = num(d.templow);
    const hi = num(d.temperature);
    if (lo === null || hi === null) return `<svg width="${bw}" height="${bh}"></svg>`;
    const x = (f) => ((f - wlo) / (whi - wlo || 1)) * (bw - bh) + bh / 2;
    const x0 = x(lo) - bh / 2;
    const x1 = x(hi) + bh / 2;
    return `
      <svg width="${bw}" height="${bh}" viewBox="0 0 ${bw} ${bh}">
        <defs>
          <linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="${x(lo).toFixed(1)}" y1="0" x2="${x(hi).toFixed(1)}" y2="0">
            ${tempStops(lo, hi)}
          </linearGradient>
        </defs>
        <rect x="0" y="0" width="${bw}" height="${bh}" rx="${bh / 2}" fill="${TRACK}"/>
        <rect x="${x0.toFixed(1)}" y="1" width="${(x1 - x0).toFixed(1)}" height="${bh - 2}" rx="${(bh - 2) / 2}"
          fill="url(#${id})" stroke="${INK}" stroke-width="2"/>
      </svg>`;
  }

  // The temperature ramp's key: every shade with its band's lower edge.
  _legend() {
    const sw = (W - 2 * 44 - 220) / TEMP_LEVELS;
    const cells = [];
    for (let l = 1; l <= TEMP_LEVELS; l++) {
      const x = 220 + (l - 1) * sw;
      cells.push(`<rect x="${x.toFixed(1)}" y="4" width="${sw.toFixed(1)}" height="22" fill="${GRAY(l)}"/>`);
      if (l > 1 && (l - 1) % 2 === 1) {
        cells.push(`<text x="${x.toFixed(1)}" y="52" text-anchor="middle" class="legend">${levelFloor(l)}°</text>`);
      }
    }
    return `
      <svg width="${W - 2 * 44}" height="60" viewBox="0 0 ${W - 2 * 44} 60">
        <text x="0" y="24" class="legend">Temperature</text>
        ${cells.join("")}
        <rect x="220.5" y="4.5" width="${(sw * TEMP_LEVELS - 1).toFixed(1)}" height="21" fill="none" stroke="${INK}" stroke-width="1"/>
      </svg>`;
  }

  _renderScreen() {
    const c = this._config;
    const weather = this._wx.src().state;
    const wa = weather?.attributes || {};
    const inside = c.inside_entity && this._hass.states[c.inside_entity];
    const sun = this._hass.states[c.sun_entity];
    const night = sun ? sun.state === "below_horizon" : false;
    const hilo = this._todayHiLo();
    const sunEvent = this._sunEvent();
    const hours = this._hourly();
    const days = this._days();
    const outlook = [this._wx.outlookRow("today"), this._wx.outlookRow("tomorrow")];
    const dayTemps = days.flatMap((d) => [num(d.templow), num(d.temperature)]).filter((v) => v !== null);
    const wlo = Math.min(...dayTemps);
    const whi = Math.max(...dayTemps);
    const wind = num(wa.wind_speed);
    const stat = (ic, value, label) => `
      <div class="stat">
        ${icon(ic, 52)}
        <div><div class="sv">${esc(value)}</div><div class="sl">${esc(label)}</div></div>
      </div>`;

    this.shadowRoot.innerHTML = `
      <style>
        /* Inherited properties (HA sets line-height, fonts, colours on the page)
           cross the shadow boundary; reset them so the layout is pixel-stable. */
        :host { all: initial; display: block; }
        .screen {
          position: relative; width: ${W}px; height: ${H}px; overflow: hidden; box-sizing: border-box;
          background: ${PAPER}; color: ${INK};
          font-family: "KindleSans", sans-serif; font-feature-settings: "tnum" 1;
          font-size: 30px; font-weight: 500; line-height: normal; letter-spacing: normal;
          text-align: left; text-transform: none; white-space: normal;
          display: grid; grid-template-rows: 318px 112px 1px 236px 1px 392px 1px 1fr 72px;
          padding: 0 44px;
        }
        .icon { fill: ${INK}; flex: none; display: block; }
        .rule { background: ${RULE}; }
        /* Icon | block-digit temperature | condition and today's high/low. */
        .hero { display: grid; grid-template-columns: 200px auto 1fr; align-items: center;
          column-gap: ${HERO_GAP}px; padding-top: 8px; }
        .hero .temp { display: flex; justify-content: center; }
        .blocks { display: block; }
        .side { display: flex; flex-direction: column; align-items: flex-end; text-align: right; min-width: 0; gap: 10px; }
        .cond { font-family: "KindleSansCondensed", sans-serif; font-size: 46px; font-weight: 700;
          line-height: 1.1; color: ${MUTED}; max-width: 100%; }
        .hilo { display: flex; flex-direction: column; align-items: flex-end; gap: 2px; font-size: 44px; font-weight: 700; }
        .hilo span { display: inline-flex; align-items: center; gap: 2px; }
        .stats { display: grid; grid-template-columns: repeat(${3 + (inside ? 1 : 0)}, 1fr); align-items: center; }
        .stat { display: flex; align-items: center; gap: 12px; }
        .sv { font-size: 40px; font-weight: 700; line-height: 1.05; white-space: nowrap; }
        .sl { font-size: 24px; font-weight: 600; color: ${MUTED}; letter-spacing: 0.02em; }
        .outlook { display: grid; grid-template-rows: 1fr 1fr; }
        .orow { display: grid; grid-template-columns: 84px 236px 1fr; align-items: center; column-gap: 16px; min-width: 0; }
        .orow .lr { display: flex; flex-direction: column; min-width: 0; }
        .orow .lbl { font-size: 32px; font-weight: 700; }
        .orow .rng { font-size: 52px; font-weight: 700; line-height: 1.05; white-space: nowrap; }
        .orow .txt { font-family: "KindleSansCondensed", sans-serif; font-size: 40px; font-weight: 700;
          white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
        .orow .wear { display: flex; align-items: center; gap: 8px; font-family: "KindleSansCondensed", sans-serif;
          font-size: 32px; font-weight: 600; color: ${MUTED}; white-space: nowrap; overflow: hidden; margin-top: 4px; }
        .orow .wear .icon { fill: ${MUTED}; }
        .nowear .wear { display: none; }
        .wrap .txt { white-space: normal; display: -webkit-box; -webkit-box-orient: vertical;
          -webkit-line-clamp: 2; line-height: 1.1; }
        .hourly { display: flex; flex-direction: column; justify-content: center; }
        .hourly svg { display: block; }
        .hourly .head { font-size: 28px; font-weight: 700; margin: 6px 0 -26px; position: relative; }
        .tlabel { font-family: "KindleSans", sans-serif; font-size: 30px; font-weight: 700; fill: ${INK};
          paint-order: stroke; stroke: ${PAPER}; stroke-width: 8px; stroke-linejoin: round; }
        .plabel { font-family: "KindleSans", sans-serif; font-size: 24px; font-weight: 700; fill: ${MUTED};
          paint-order: stroke; stroke: ${PAPER}; stroke-width: 6px; stroke-linejoin: round; }
        .axis { font-family: "KindleSans", sans-serif; font-size: 25px; font-weight: 600; fill: ${AXIS}; }
        .ctext { font-family: "KindleSans", sans-serif; font-size: 30px; fill: ${MUTED}; }
        .days { display: flex; flex-direction: column; justify-content: space-evenly; }
        .day { display: grid; grid-template-columns: 96px 64px 122px 84px 1fr 84px; align-items: center; column-gap: 10px; }
        .day .name { font-size: 38px; font-weight: 700; }
        .day .pp { display: flex; align-items: center; gap: 2px; font-size: 30px; font-weight: 600; color: ${MUTED}; }
        .day .pp .icon { fill: ${MUTED}; }
        .day .lo { font-size: 38px; font-weight: 500; color: ${MUTED}; text-align: right; }
        .day .hi { font-size: 38px; font-weight: 700; }
        .day svg { display: block; }
        .legend { font-family: "KindleSans", sans-serif; font-size: 22px; font-weight: 600; fill: ${MUTED}; }
        footer { display: flex; align-items: center; }
      </style>
      <div class="screen">
        <section class="hero">
          ${icon(weather ? this._wx.conditionIcon(weather.state, night && weather.state === "partlycloudy") : "alert-circle-outline", 200)}
          <div class="temp">${blockText(num(wa.temperature) === null ? "--°" : `${Math.round(num(wa.temperature))}°`, HERO_TEMP_W, 280, 18)}</div>
          <div class="side">
            <div class="cond">${esc(CONDITION_TEXT[weather?.state] || weather?.state || "Unavailable")}</div>
            ${
              hilo && (num(hilo.temperature) !== null || num(hilo.templow) !== null)
                ? `<div class="hilo">${
                    num(hilo.temperature) !== null ? `<span>${icon("arrow-up", 42)}${temp(hilo.temperature)}</span>` : ""
                  }${num(hilo.templow) !== null ? `<span>${icon("arrow-down", 42)}${temp(hilo.templow)}</span>` : ""}</div>`
                : ""
            }
          </div>
        </section>
        <section class="stats">
          ${inside ? stat("home-thermometer-outline", temp(inside.state), "Inside") : ""}
          ${stat("water-percent", num(wa.humidity) === null ? "–" : `${Math.round(wa.humidity)}%`, "Humidity")}
          ${stat("weather-windy", wind === null ? "–" : `${Math.round(wind)} ${compass(wa.wind_bearing)}`.trim(), `Wind, ${wa.wind_speed_unit || "mph"}`)}
          ${sunEvent ? stat(sunEvent.icon, sunEvent.time, sunEvent.label) : ""}
        </section>
        <div class="rule"></div>
        <section class="outlook">
          ${outlook
            .map(
              (o) => `
          <div class="orow" data-fit="${esc(JSON.stringify(o.texts || [o.text]))}">
            ${icon(o.icon, 84)}
            <div class="lr"><div class="lbl">${esc(o.label)}</div><div class="rng">${esc(o.range)}</div></div>
            <div class="lr">
              <div class="txt">${esc(o.text)}</div>
              <div class="wear">${o.wear ? `${icon(o.wear.icon, 34)}<span>${esc(o.wear.text)}</span>` : ""}</div>
            </div>
          </div>`,
            )
            .join("")}
        </section>
        <div class="rule"></div>
        <section class="hourly">
          <div class="head">Next ${c.hours} hours</div>
          ${this._chart(hours)}
        </section>
        <div class="rule"></div>
        <section class="days">
          ${days
            .map(
              (d, i) => `
          <div class="day">
            <div class="name">${esc(this._wx.weekday(d.datetime))}</div>
            ${icon(CONDITION_ICON[d.condition] || "alert-circle-outline", 56)}
            <div class="pp">${icon("water", 28)}${esc(this._wx.precipText(d.precipitation_probability, d.precipitation))}</div>
            <div class="lo">${temp(d.templow)}</div>
            ${this._dayBar(d, wlo, whi, `dbar${i}`)}
            <div class="hi">${temp(d.temperature)}</div>
          </div>`,
            )
            .join("")}
        </section>
        <footer>${this._legend()}</footer>
      </div>`;
  }
}
customElements.define("kindle-weather-card", KindleWeatherCard);
window.customCards = window.customCards || [];
window.customCards.push({
  type: "kindle-weather-card",
  name: "Kindle weather",
  description: "Full-screen 1072x1448 16-gray weather dashboard for a wall-mounted Kindle (rendered via Puppet).",
});
