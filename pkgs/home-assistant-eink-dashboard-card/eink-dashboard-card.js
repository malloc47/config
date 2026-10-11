// eink-dashboard-card — a full-screen Lovelace card laid out for an 800x480
// 1-bit e-paper panel (Seeed reTerminal E1001). Meant to fill a panel view that
// Puppet screenshots with colors=000000,FFFFFF; the panel's ESPHome firmware
// downloads that image and draws the time and date itself into the blank
// "clock area" at the top centre, so the clock keeps its per-minute partial
// refresh while everything here refreshes on the image's slower schedule.
//
// Everything is pure black on white (Puppet maps each pixel to the nearer of
// the two colours, so greys would just threshold) and drawn at fixed pixel
// sizes for the panel resolution.
//
// Config:
//   type: custom:eink-dashboard-card
//   weather_entity: weather.forecast_home        # current conditions + forecasts
//   inside_entity: sensor.<indoor temperature>
//   sun_entity: sun.sun                          # optional, next sunrise/sunset
//   battery_entity: sensor.<panel battery %>     # optional
//   external_power_entity: binary_sensor.<...>   # optional; battery hidden while "on"
//   days: 5                                      # daily forecast columns
//   banner_template: "{{ ... }}"                 # Jinja message; default: input_text.eink_banner
//   doors_entity: binary_sensor.perimeter_doors  # group whose open members join the banner
//   layout: standard                             # "standard" or "focus" (big clock + outlook)
//   outdoor_hours: [7, 22]                       # focus: local hours "outside" for the outlook
//   cold: 45                                     # focus: °F below which it's coat weather
//   hot: 85                                      # focus: °F at/above which it's hot
//   show_clock_area: false                       # outline the device-drawn area (design aid)
//   hide_toasts: true                            # hide HA's toasts while shown (see below)
//
// Icons are Material Design Icons (https://pictogrammers.com/library/mdi/,
// Apache-2.0), embedded as SVG paths so the card has no runtime dependencies.

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
  "thermometer": "M15 13V5A3 3 0 0 0 9 5V13A5 5 0 1 0 15 13M12 4A1 1 0 0 1 13 5V8H11V5A1 1 0 0 1 12 4Z",
  "water": "M12,20A6,6 0 0,1 6,14C6,10 12,3.25 12,3.25C12,3.25 18,10 18,14A6,6 0 0,1 12,20Z",
  "water-percent": "M12,3.25C12,3.25 6,10 6,14C6,17.32 8.69,20 12,20A6,6 0 0,0 18,14C18,10 12,3.25 12,3.25M14.47,9.97L15.53,11.03L9.53,17.03L8.47,15.97M9.75,10A1.25,1.25 0 0,1 11,11.25A1.25,1.25 0 0,1 9.75,12.5A1.25,1.25 0 0,1 8.5,11.25A1.25,1.25 0 0,1 9.75,10M14.25,14.5A1.25,1.25 0 0,1 15.5,15.75A1.25,1.25 0 0,1 14.25,17A1.25,1.25 0 0,1 13,15.75A1.25,1.25 0 0,1 14.25,14.5Z",
  "weather-sunset-up": "M3,12H7A5,5 0 0,1 12,7A5,5 0 0,1 17,12H21A1,1 0 0,1 22,13A1,1 0 0,1 21,14H3A1,1 0 0,1 2,13A1,1 0 0,1 3,12M15,12A3,3 0 0,0 12,9A3,3 0 0,0 9,12H15M12,2L14.39,5.42C13.65,5.15 12.84,5 12,5C11.16,5 10.35,5.15 9.61,5.42L12,2M3.34,7L7.5,6.65C6.9,7.16 6.36,7.78 5.94,8.5C5.5,9.24 5.25,10 5.11,10.79L3.34,7M20.65,7L18.88,10.79C18.74,10 18.47,9.23 18.05,8.5C17.63,7.78 17.1,7.15 16.5,6.64L20.65,7M12.71,16.3L15.82,19.41C16.21,19.8 16.21,20.43 15.82,20.82C15.43,21.21 14.8,21.21 14.41,20.82L12,18.41L9.59,20.82C9.2,21.21 8.57,21.21 8.18,20.82C7.79,20.43 7.79,19.8 8.18,19.41L11.29,16.3C11.5,16.1 11.74,16 12,16C12.26,16 12.5,16.1 12.71,16.3Z",
  "weather-sunset-down": "M3,12H7A5,5 0 0,1 12,7A5,5 0 0,1 17,12H21A1,1 0 0,1 22,13A1,1 0 0,1 21,14H3A1,1 0 0,1 2,13A1,1 0 0,1 3,12M15,12A3,3 0 0,0 12,9A3,3 0 0,0 9,12H15M12,2L14.39,5.42C13.65,5.15 12.84,5 12,5C11.16,5 10.35,5.15 9.61,5.42L12,2M3.34,7L7.5,6.65C6.9,7.16 6.36,7.78 5.94,8.5C5.5,9.24 5.25,10 5.11,10.79L3.34,7M20.65,7L18.88,10.79C18.74,10 18.47,9.23 18.05,8.5C17.63,7.78 17.1,7.15 16.5,6.64L20.65,7M12.71,20.71L15.82,17.6C16.21,17.21 16.21,16.57 15.82,16.18C15.43,15.79 14.8,15.79 14.41,16.18L12,18.59L9.59,16.18C9.2,15.79 8.57,15.79 8.18,16.18C7.79,16.57 7.79,17.21 8.18,17.6L11.29,20.71C11.5,20.9 11.74,21 12,21C12.26,21 12.5,20.9 12.71,20.71Z",
  "arrow-up": "M13,20H11V8L5.5,13.5L4.08,12.08L12,4.16L19.92,12.08L18.5,13.5L13,8V20Z",
  "arrow-down": "M11,4H13V16L18.5,10.5L19.92,11.92L12,19.84L4.08,11.92L5.5,10.5L11,16V4Z",
  "battery-10": "M16,18H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-20": "M16,17H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-30": "M16,15H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-40": "M16,14H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-50": "M16,13H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-60": "M16,12H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-70": "M16,10H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-80": "M16,9H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-90": "M16,8H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery": "M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-outline": "M16,20H8V6H16M16.67,4H15V2H9V4H7.33A1.33,1.33 0 0,0 6,5.33V20.67C6,21.4 6.6,22 7.33,22H16.67A1.33,1.33 0 0,0 18,20.67V5.33C18,4.6 17.4,4 16.67,4Z",
  "battery-alert-variant-outline": "M14 20H6V6H14M14.67 4H13V2H7V4H5.33C4.6 4 4 4.6 4 5.33V20.67C4 21.4 4.6 22 5.33 22H14.67C15.4 22 16 21.4 16 20.67V5.33C16 4.6 15.4 4 14.67 4M21 7H19V13H21V8M21 15H19V17H21V15Z",
  "information-outline": "M11,9H13V7H11M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M11,17H13V11H11V17Z",
  "door-open": "M12,3C10.89,3 10,3.89 10,5H3V19H2V21H22V19H21V5C21,3.89 20.11,3 19,3H12M12,5H19V19H12V5M5,11H7V13H5V11Z",
  "garage-open-variant": "M22 9V20H20V11H4V20H2V9L12 5L22 9M19 12H5V14H19V12Z",
  "window-open-variant": "M21 20V2H3V20H1V23H23V20M19 4V11H17V4M5 4H7V11H5M5 20V13H7V20M9 20V4H15V20M17 20V13H19V20Z",
  "umbrella-outline": "M12,4C8.9,4 6.18,6.03 5.3,9H18.7C17.82,6.04 15.09,4 12,4M12,2A9,9 0 0,1 21,11H13V19A3,3 0 0,1 10,22A3,3 0 0,1 7,19V18H9V19A1,1 0 0,0 10,20A1,1 0 0,0 11,19V11H3A9,9 0 0,1 12,2Z",
  "tshirt-crew-outline": "M16,21H8A1,1 0 0,1 7,20V12.07L5.7,13.12C5.31,13.5 4.68,13.5 4.29,13.12L1.46,10.29C1.07,9.9 1.07,9.27 1.46,8.88L7.34,3H9C9,4.1 10.34,5 12,5C13.66,5 15,4.1 15,3H16.66L22.54,8.88C22.93,9.27 22.93,9.9 22.54,10.29L19.71,13.12C19.32,13.5 18.69,13.5 18.3,13.12L17,12.07V20A1,1 0 0,1 16,21M20.42,9.58L16.11,5.28C15.8,5.63 15.43,5.94 15,6.2C14.16,6.7 13.13,7 12,7C10.3,7 8.79,6.32 7.89,5.28L3.58,9.58L5,11L8,9H9V19H15V9H16L19,11L20.42,9.58Z",
  "snowflake": "M20.79,13.95L18.46,14.57L16.46,13.44V10.56L18.46,9.43L20.79,10.05L21.31,8.12L19.54,7.65L20,5.88L18.07,5.36L17.45,7.69L15.45,8.82L13,7.38V5.12L14.71,3.41L13.29,2L12,3.29L10.71,2L9.29,3.41L11,5.12V7.38L8.5,8.82L6.5,7.69L5.92,5.36L4,5.88L4.47,7.65L2.7,8.12L3.22,10.05L5.55,9.43L7.55,10.56V13.45L5.55,14.58L3.22,13.96L2.7,15.89L4.47,16.36L4,18.12L5.93,18.64L6.55,16.31L8.55,15.18L11,16.62V18.88L9.29,20.59L10.71,22L12,20.71L13.29,22L14.7,20.59L13,18.88V16.62L15.5,15.17L17.5,16.3L18.12,18.63L20,18.12L19.53,16.35L21.3,15.88L20.79,13.95M9.5,10.56L12,9.11L14.5,10.56V13.44L12,14.89L9.5,13.44V10.56Z",
  "thermometer-high": "M15 13V5A3 3 0 0 0 9 5V13A5 5 0 1 0 15 13M12 4A1 1 0 0 1 13 5H11A1 1 0 0 1 12 4Z",
  "sunglasses": "M7,17H4C2.38,17 0.96,15.74 0.76,14.14L0.26,11.15C0.15,10.3 0.39,9.5 0.91,8.92C1.43,8.34 2.19,8 3,8H9C9.83,8 10.58,8.35 11.06,8.96C11.17,9.11 11.27,9.27 11.35,9.45C11.78,9.36 12.22,9.36 12.64,9.45C12.72,9.27 12.82,9.11 12.94,8.96C13.41,8.35 14.16,8 15,8H21C21.81,8 22.57,8.34 23.09,8.92C23.6,9.5 23.84,10.3 23.74,11.11L23.23,14.18C23.04,15.74 21.61,17 20,17H17C15.44,17 13.92,15.81 13.54,14.3L12.64,11.59C12.26,11.31 11.73,11.31 11.35,11.59L10.43,14.37C10.07,15.82 8.56,17 7,17Z",
  "bell-outline": "M10 21H14C14 22.1 13.1 23 12 23S10 22.1 10 21M21 19V20H3V19L5 17V11C5 7.9 7 5.2 10 4.3V4C10 2.9 10.9 2 12 2S14 2.9 14 4V4.3C17 5.2 19 7.9 19 11V17L21 19M17 11C17 8.2 14.8 6 12 6S7 8.2 7 11V18H17V11Z",
};

// Static Inter instances autohinted at build time (see default.nix). With
// Puppet's Chromium rasterizing text 1-bit (no anti-aliasing), the hinting keeps
// stems even and stops small features like i-dots dropping out; anti-aliased
// text thresholded to black/white came out blobby.
const FONT_WEIGHTS = [500, 600, 700];

// @font-face has no effect inside a shadow root, so register it on the document.
if (!document.getElementById("eink-dashboard-card-font")) {
  const style = document.createElement("style");
  style.id = "eink-dashboard-card-font";
  style.textContent = FONT_WEIGHTS.map(
    (w) => `@font-face {
    font-family: "EinkInter";
    src: url("${new URL(`./Inter-${w}.ttf`, import.meta.url).href}") format("truetype");
    font-weight: ${w};
    font-display: block;
  }`,
  ).join("\n");
  document.head.appendChild(style);
}

const CONDITION_ICON = {
  "clear-night": "weather-night",
  cloudy: "weather-cloudy",
  exceptional: "alert-circle-outline",
  fog: "weather-fog",
  hail: "weather-hail",
  lightning: "weather-lightning",
  "lightning-rainy": "weather-lightning-rainy",
  partlycloudy: "weather-partly-cloudy",
  pouring: "weather-pouring",
  rainy: "weather-rainy",
  snowy: "weather-snowy",
  "snowy-rainy": "weather-snowy-rainy",
  sunny: "weather-sunny",
  windy: "weather-windy",
  "windy-variant": "weather-windy-variant",
};

const CONDITION_TEXT = {
  "clear-night": "Clear",
  cloudy: "Cloudy",
  exceptional: "Severe weather",
  fog: "Fog",
  hail: "Hail",
  lightning: "Lightning",
  "lightning-rainy": "Thunderstorms",
  partlycloudy: "Partly cloudy",
  pouring: "Heavy rain",
  rainy: "Rain",
  snowy: "Snow",
  "snowy-rainy": "Sleet",
  sunny: "Sunny",
  windy: "Windy",
  "windy-variant": "Windy & cloudy",
};

// Which condition "wins" when summarising several forecast hours.
const CONDITION_RANK = {
  exceptional: 100,
  "lightning-rainy": 90,
  lightning: 85,
  hail: 80,
  "snowy-rainy": 75,
  snowy: 70,
  pouring: 65,
  rainy: 60,
  fog: 50,
  "windy-variant": 45,
  windy: 40,
  cloudy: 30,
  partlycloudy: 20,
  sunny: 10,
  "clear-night": 10,
};

// Forecast buckets by local start hour; "night" runs 18:00-06:00, so hours
// before 06:00 belong to the previous day's night.
const BUCKETS = [
  { id: "morning", start: 6, end: 12, label: "Morning" },
  { id: "afternoon", start: 12, end: 18, label: "Afternoon" },
  { id: "night", start: 18, end: 30, label: "Night" },
];

// Free-text message shown first in the banner (empty hides it). Open doors from
// doors_entity are listed after it natively, with an icon and a short name.
const DEFAULT_BANNER = `{%- set msg = states('input_text.eink_banner') -%}
{{ msg if msg not in ['', 'unknown', 'unavailable'] else '' }}`;

const esc = (s) =>
  String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);

const icon = (name, size) =>
  `<svg class="icon" viewBox="0 0 24 24" width="${size}" height="${size}" aria-hidden="true"><path d="${
    ICONS[name] || ICONS["alert-circle-outline"]
  }"/></svg>`;

const num = (v) => {
  const n = parseFloat(v);
  return Number.isFinite(n) ? n : null;
};

const temp = (v) => (num(v) === null ? "–" : `${Math.round(num(v))}°`);

const precip = (v, unit) => {
  const n = num(v);
  if (n === null) return "–";
  if (n < 0.005) return `0 ${unit}`;
  const s = n < 1 ? n.toFixed(2).replace(/0+$/, "").replace(/\.$/, "") : n.toFixed(1);
  return `${s} ${unit}`;
};

class EinkDashboardCard extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
    this._forecasts = { hourly: null, daily: null };
    this._banner = "";
    // Active subscriptions by key ("hourly", "daily", "banner"): a promise of
    // the unsubscribe function, or a retry deadline after a failed subscribe.
    this._subs = {};
    this._subscribedFor = null;
    this._raf = null;
  }

  setConfig(config) {
    if (!config.weather_entity) throw new Error("weather_entity is required");
    this._config = {
      days: 5,
      sun_entity: "sun.sun",
      banner_template: DEFAULT_BANNER,
      show_clock_area: false,
      hide_toasts: true,
      layout: "standard",
      outdoor_hours: [7, 22],
      cold: 45,
      hot: 85,
      ...config,
    };
    this._unsubscribe();
    this._ensureSubscriptions();
    this._schedule();
  }

  set hass(hass) {
    if (this._subscribedFor && this._subscribedFor !== hass.connection) this._unsubscribe();
    this._hass = hass;
    this._ensureSubscriptions();
    this._schedule();
  }

  connectedCallback() {
    this._ensureSubscriptions();
    this._setToastsHidden(this._config?.hide_toasts !== false);
  }

  disconnectedCallback() {
    this._unsubscribe();
    this._setToastsHidden(false);
  }

  // HA's frontend shows toasts ("Starting <integration>...", "Connection lost")
  // over the page, and Puppet would screenshot them into the panel's image for
  // a whole refresh interval. The toasts are top-layer popovers rendered by a
  // <notification-manager> that HA mounts lazily (as a dialog, on the first
  // toast) inside <home-assistant>'s shadow root, out of reach of the card's
  // styles. So while the card is on screen, put a hiding stylesheet into that
  // shadow root, and into each manager's own shadow root as it appears.
  _setToastsHidden(hidden) {
    const root = document.querySelector("home-assistant")?.shadowRoot;
    if (!root) return;
    const hideIn = (sr, css) => {
      if (!sr || sr.getElementById?.("eink-hide-toasts") || sr.querySelector?.("#eink-hide-toasts")) return;
      const style = document.createElement("style");
      style.id = "eink-hide-toasts";
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
            // Its shadow root may render after insertion; retry once.
            if (!m.shadowRoot?.querySelector("#eink-hide-toasts")) setTimeout(() => hideManager(m), 0);
          }),
        );
        this._toastObserver.observe(root, { childList: true });
      }
    } else {
      this._toastObserver?.disconnect();
      this._toastObserver = null;
      for (const sr of [root, ...[...root.querySelectorAll("notification-manager")].map((m) => m.shadowRoot)]) {
        sr?.querySelector("#eink-hide-toasts")?.remove();
      }
    }
  }

  getCardSize() {
    return 10;
  }

  _unsubscribe() {
    for (const sub of Object.values(this._subs)) {
      if (sub.unsub) sub.unsub.then((f) => f && f()).catch(() => {});
    }
    this._subs = {};
    this._subscribedFor = null;
  }

  // (Re)subscribe anything not currently subscribed. Called on every hass
  // update, so a subscription that failed (e.g. the weather entity not loaded
  // yet right after an HA restart) is retried instead of leaving the card
  // permanently without forecasts; Puppet keeps one long-lived page, so the
  // card has to heal itself.
  _ensureSubscriptions() {
    if (!this._hass || !this._config || !this.isConnected) return;
    const conn = this._hass.connection;
    this._subscribedFor = conn;
    const wanted = {
      hourly: { type: "weather/subscribe_forecast", forecast_type: "hourly", entity_id: this._config.weather_entity },
      daily: { type: "weather/subscribe_forecast", forecast_type: "daily", entity_id: this._config.weather_entity },
    };
    if (this._config.banner_template) {
      wanted.banner = {
        type: "render_template",
        template: this._config.banner_template,
        strict: false,
        report_errors: false,
      };
    }
    const now = Date.now();
    for (const [key, msg] of Object.entries(wanted)) {
      const sub = this._subs[key];
      if (sub && (sub.unsub || now < sub.retryAt)) continue;
      const onMessage = (m) => {
        if (key === "banner") {
          if (!("result" in m)) return;
          this._banner = String(m.result ?? "").trim();
        } else {
          this._forecasts[key] = m.forecast || [];
        }
        this._schedule();
      };
      const unsub = conn.subscribeMessage(onMessage, msg);
      this._subs[key] = { unsub };
      unsub.catch((err) => {
        console.warn(`eink-dashboard-card: ${key} subscription failed, retrying`, err);
        if (this._subs[key]?.unsub === unsub) this._subs[key] = { retryAt: Date.now() + 15000 };
      });
    }
  }

  _schedule() {
    if (this._raf) return;
    this._raf = requestAnimationFrame(() => {
      this._raf = null;
      this._render();
    });
  }

  // Local date ("YYYY-MM-DD") and hour for a timestamp in HA's time zone, so the
  // render doesn't depend on the screenshot browser's own TZ.
  _local(ts) {
    const tz = this._hass?.config?.time_zone;
    const parts = Object.fromEntries(
      new Intl.DateTimeFormat("en-CA", {
        timeZone: tz,
        year: "numeric",
        month: "2-digit",
        day: "2-digit",
        hour: "2-digit",
        hourCycle: "h23",
      })
        .formatToParts(new Date(ts))
        .map((p) => [p.type, p.value]),
    );
    return { date: `${parts.year}-${parts.month}-${parts.day}`, hour: parseInt(parts.hour, 10) };
  }

  _weekday(ts, style = "short") {
    return new Intl.DateTimeFormat("en-US", { timeZone: this._hass?.config?.time_zone, weekday: style }).format(
      new Date(ts),
    );
  }

  _clockTime(ts) {
    return new Intl.DateTimeFormat("en-US", {
      timeZone: this._hass?.config?.time_zone,
      hour: "numeric",
      minute: "2-digit",
    }).format(new Date(ts));
  }

  _nightIcon(condition) {
    if (condition === "sunny") return "weather-night";
    if (condition === "partlycloudy") return "weather-night-partly-cloudy";
    return CONDITION_ICON[condition] || "alert-circle-outline";
  }

  _conditionIcon(condition, night) {
    return night ? this._nightIcon(condition) : CONDITION_ICON[condition] || "alert-circle-outline";
  }

  // Next three morning/afternoon/night buckets from the hourly forecast,
  // starting with whatever is left of the current one (if 2+ hours remain).
  _buckets() {
    const hourly = this._forecasts.hourly || [];
    const today = this._local(Date.now()).date;
    const groups = new Map();
    for (const f of hourly) {
      let { date, hour } = this._local(f.datetime);
      if (hour < 6) {
        const d = new Date(`${date}T12:00:00Z`);
        d.setUTCDate(d.getUTCDate() - 1);
        date = d.toISOString().slice(0, 10);
        hour += 24;
      }
      const bucket = BUCKETS.find((b) => hour >= b.start && hour < b.end);
      const key = `${date}/${bucket.id}`;
      if (!groups.has(key)) groups.set(key, { date, bucket, hours: [], ts: f.datetime });
      groups.get(key).hours.push(f);
    }
    return [...groups.values()]
      .filter((g) => g.hours.length >= 2)
      .slice(0, 3)
      .map((g) => {
        const temps = g.hours.map((h) => num(h.temperature)).filter((t) => t !== null);
        const night = g.bucket.id === "night";
        const condition = g.hours
          .map((h) => h.condition)
          .reduce((a, b) => ((CONDITION_RANK[b] || 0) > (CONDITION_RANK[a] || 0) ? b : a));
        const isToday = g.date === today;
        const label = isToday
          ? night
            ? "Tonight"
            : g.bucket.label
          : `${this._weekday(`${g.date}T12:00:00Z`)} ${g.bucket.label.toLowerCase()}`;
        return {
          label,
          icon: this._conditionIcon(condition, night),
          temp: temps.length ? (night ? Math.min(...temps) : Math.max(...temps)) : null,
          tempLabel: night ? "Low" : "High",
          precip: g.hours.reduce((s, h) => s + (num(h.precipitation) || 0), 0),
        };
      });
  }

  _days() {
    const daily = this._forecasts.daily || [];
    const today = this._local(Date.now()).date;
    return daily.filter((f) => this._local(f.datetime).date > today).slice(0, this._config.days);
  }

  _todayHiLo() {
    const daily = this._forecasts.daily || [];
    const today = this._local(Date.now()).date;
    return daily.find((f) => this._local(f.datetime).date === today);
  }

  _battery() {
    const c = this._config;
    if (!c.battery_entity) return null;
    const ext = c.external_power_entity && this._hass.states[c.external_power_entity];
    if (ext && ext.state === "on") return null;
    const level = num(this._hass.states[c.battery_entity]?.state);
    if (level === null) return null;
    const step = Math.max(10, Math.min(100, Math.round(level / 10) * 10));
    const name = level < 5 ? "battery-alert-variant-outline" : step === 100 ? "battery" : `battery-${step}`;
    return { level: Math.round(level), icon: name };
  }

  // Open members of doors_entity (a group), as banner items: an open-door icon
  // (garage/window by device class) and the name without a trailing "Door".
  // Gates use the door icon too: MDI's gate glyph breaks up at 22px in 1-bit.
  _openDoors() {
    const group = this._config.doors_entity && this._hass.states[this._config.doors_entity];
    const members = group?.attributes?.entity_id || [];
    return members
      .map((id) => this._hass.states[id])
      .filter((st) => st && st.state === "on")
      .map((st) => {
        const name = st.attributes.friendly_name || st.entity_id;
        const dc = st.attributes.device_class;
        const ic = dc === "garage_door" ? "garage-open-variant" : dc === "window" ? "window-open-variant" : "door-open";
        return { icon: ic, text: name.replace(/\s+door$/i, "") };
      });
  }

  _sunEvent() {
    const sun = this._hass.states[this._config.sun_entity];
    if (!sun) return null;
    const rise = Date.parse(sun.attributes.next_rising);
    const set = Date.parse(sun.attributes.next_setting);
    if (!Number.isFinite(rise) || !Number.isFinite(set)) return null;
    return rise < set
      ? { icon: "weather-sunset-up", time: this._clockTime(rise), label: "Sunrise" }
      : { icon: "weather-sunset-down", time: this._clockTime(set), label: "Sunset" };
  }

  // ---- focus layout: "what to wear" outlook ------------------------------

  _hourLabel(h) {
    const hh = ((h % 24) + 24) % 24;
    if (hh === 0) return "midnight";
    if (hh === 12) return "noon";
    return `${hh % 12 || 12} ${hh < 12 ? "AM" : "PM"}`;
  }

  // "1–3 PM" rather than "1 PM–3 PM" when both ends share AM/PM.
  _hourRange(a, b) {
    const la = this._hourLabel(a);
    const lb = this._hourLabel(b);
    const [na, sa] = la.split(" ");
    const sb = lb.split(" ")[1];
    return sa && sa === sb ? `${na}–${lb}` : `${la}–${lb}`;
  }

  // °F for classification, whatever the weather entity reports in.
  _toF(t) {
    const unit = this._hass.states[this._config.weather_entity]?.attributes?.temperature_unit;
    return unit === "°C" ? (t * 9) / 5 + 32 : t;
  }

  _isWet(f) {
    const unit = this._hass.states[this._config.weather_entity]?.attributes?.precipitation_unit;
    const min = unit === "mm" ? 0.25 : 0.01;
    return (
      (num(f.precipitation) || 0) >= min ||
      ["rainy", "pouring", "lightning-rainy", "snowy", "snowy-rainy", "hail"].includes(f.condition)
    );
  }

  // Hourly forecast entries for one outlook window, tagged with a continuous
  // hour number (hours after midnight run past 24 for "tonight").
  _window(kind) {
    const [start, end] = this._config.outdoor_hours;
    const now = this._local(Date.now());
    const d = new Date(`${now.date}T12:00:00Z`);
    d.setUTCDate(d.getUTCDate() + 1);
    const tomorrow = d.toISOString().slice(0, 10);
    const hours = (this._forecasts.hourly || []).map((f) => {
      const l = this._local(f.datetime);
      return { ...f, h: l.date === now.date ? l.hour : l.date === tomorrow ? l.hour + 24 : 99 };
    });
    if (kind === "tomorrow") return hours.filter((f) => f.h >= 24 + start && f.h < 24 + end);
    if (now.hour < end - 1) return hours.filter((f) => f.h >= Math.max(now.hour, start) && f.h < end);
    // Late evening: describe the night instead (until 6 AM).
    return hours.filter((f) => f.h >= now.hour && f.h < 30);
  }

  _outlookRow(kind) {
    const hours = this._window(kind);
    const now = this._local(Date.now());
    const [, end] = this._config.outdoor_hours;
    const tonight = kind === "today" && now.hour >= end - 1;
    const label = kind === "tomorrow" ? "Tomorrow" : tonight ? "Tonight" : "Today";
    if (!hours.length) return { label, icon: "alert-circle-outline", text: "No forecast", range: "", wear: null };

    const unit = this._hass.states[this._config.weather_entity]?.attributes?.precipitation_unit || "";
    const temps = hours.map((f) => num(f.temperature)).filter((t) => t !== null);
    const lo = Math.min(...temps);
    const hi = Math.max(...temps);
    const conds = hours.map((f) => f.condition);
    const worst = conds.reduce((a, b) => ((CONDITION_RANK[b] || 0) > (CONDITION_RANK[a] || 0) ? b : a));
    const nightish = tonight;

    // Rain spans: consecutive wet hours, bridging single dry hours.
    const wet = hours.filter((f) => this._isWet(f));
    const spans = [];
    for (const f of wet) {
      const last = spans[spans.length - 1];
      if (last && f.h - last.end <= 1) last.end = f.h + 1;
      else spans.push({ start: f.h, end: f.h + 1 });
    }
    const first = hours[0].h;
    const lastH = hours[hours.length - 1].h + 1;
    let text;
    if (spans.length) {
      const kindWord = conds.some((c) => c === "snowy")
        ? "Snow"
        : conds.some((c) => c === "snowy-rainy" || c === "hail")
          ? "Sleet"
          : conds.some((c) => c.startsWith("lightning"))
            ? "Storms"
            : "Rain";
      const describe = (sp, lead) => {
        const w = lead ? kindWord : "again";
        if (sp.start <= first && sp.end >= lastH) return lead ? `${kindWord} ${tonight ? "all night" : "all day"}` : "";
        if (sp.start <= first) return `${w} until ${this._hourLabel(sp.end)}`;
        if (sp.end >= lastH) return `${w} from ${this._hourLabel(sp.start)}`;
        return `${w} ${this._hourRange(sp.start, sp.end)}`;
      };
      const parts = spans.slice(0, 2).map((sp, i) => describe(sp, i === 0)).filter(Boolean);
      const total = wet.reduce((sum, f) => sum + (num(f.precipitation) || 0), 0);
      text = `${parts.join(", ")}${spans.length > 2 ? ", …" : ""}${total >= 0.005 ? ` (${precip(total, unit)})` : ""}`;
    } else {
      const sky = {
        sunny: tonight ? "clear" : "sunny",
        "clear-night": "clear",
        partlycloudy: "partly cloudy",
        cloudy: "cloudy",
        fog: "foggy",
        windy: "windy",
        "windy-variant": "windy",
      }[worst];
      text = sky ? `Dry, ${sky}` : "Dry";
    }

    // Dress for the coldest outdoor hour; flag heat separately.
    const loF = this._toF(lo);
    const hiF = this._toF(hi);
    const { cold, hot } = this._config;
    let wear =
      loF < 32
        ? { icon: "snowflake", text: "Heavy coat" }
        : loF < cold
          ? { icon: "tshirt-crew-outline", text: "Coat" }
          : loF < 58
            ? { icon: "tshirt-crew-outline", text: "Jacket" }
            : loF < 68
              ? { icon: "tshirt-crew-outline", text: "Light layer" }
              : { icon: "tshirt-crew-outline", text: "T-shirt" };
    if (hiF >= hot) wear = { icon: "thermometer-high", text: `${wear.text === "T-shirt" ? "Hot" : `${wear.text}, hot`}` };
    if (spans.length) {
      const snow = conds.some((c) => c === "snowy" || c === "snowy-rainy");
      wear = { icon: snow ? "snowflake" : "umbrella-outline", text: `${snow ? "Boots" : "Umbrella"}, ${wear.text.toLowerCase()}` };
    }

    return {
      label,
      icon: this._conditionIcon(worst, nightish),
      text,
      range: Math.round(lo) === Math.round(hi) ? temp(lo) : `${Math.round(lo)}–${temp(hi)}`,
      wear,
    };
  }

  _render() {
    if (!this._hass || !this._config) return;
    // Try the full door list first; if it overflows the banner, re-render once
    // with the doors collapsed into a single summary item.
    this._renderScreen(false);
    const banner = this.shadowRoot.querySelector(".banner");
    if (banner && banner.scrollWidth > banner.clientWidth + 1 && this._openDoors().length > 1) {
      this._renderScreen(true);
    }
  }

  _renderScreen(compactDoors) {
    const c = this._config;
    const weather = this._hass.states[c.weather_entity];
    const inside = c.inside_entity && this._hass.states[c.inside_entity];
    const sun = this._hass.states[c.sun_entity];
    const night = sun ? sun.state === "below_horizon" : false;
    const wa = weather?.attributes || {};
    const unit = wa.precipitation_unit || "";
    const hilo = this._todayHiLo();
    const sunEvent = this._sunEvent();
    const battery = this._battery();
    const buckets = this._buckets();
    const days = this._days();
    const doors = this._openDoors();
    const bannerItems = [
      ...(this._banner ? [{ icon: "bell-outline", text: this._banner, shrink: true }] : []),
      ...(compactDoors
        ? [{ icon: "door-open", text: `${doors.length} open: ${doors.map((d) => d.text).join(", ")}`, shrink: true }]
        : doors),
    ];

    const stat = (ic, value, label) => `
      <div class="stat">
        ${icon(ic, 30)}
        <div><div class="stat-value">${esc(value)}</div><div class="label">${esc(label)}</div></div>
      </div>`;

    const standardBody =
      c.layout === "focus"
        ? ""
        : `
        <section class="hero">
          <div class="now">
            <div class="now-top">
              ${icon(weather ? this._conditionIcon(weather.state, night && weather.state === "partlycloudy") : "alert-circle-outline", 72)}
              <div class="now-temp">${temp(wa.temperature)}</div>
            </div>
            <div class="now-cond">${esc(CONDITION_TEXT[weather?.state] || weather?.state || "Unavailable")}</div>
            ${
              hilo
                ? `<div class="hilo"><span>${icon("arrow-up", 16)}${temp(hilo.temperature)}</span><span>${icon(
                    "arrow-down",
                    16,
                  )}${temp(hilo.templow)}</span></div>`
                : ""
            }
          </div>
          <div class="clock${c.show_clock_area ? " outline" : ""}"></div>
          <div class="stats">
            ${stat("home-thermometer-outline", inside ? temp(inside.state) : "–", "Inside")}
            ${stat("water-percent", num(wa.humidity) === null ? "–" : `${Math.round(wa.humidity)}%`, "Humidity")}
            ${sunEvent ? stat(sunEvent.icon, sunEvent.time, sunEvent.label) : ""}
          </div>
        </section>
        <div class="rule"></div>
        <section class="today">
          ${buckets
            .map(
              (b, i) => `${i ? '<div class="vrule"></div>' : ""}
            <div class="bucket">
              ${icon(b.icon, 56)}
              <div>
                <div class="name">${esc(b.label)}</div>
                <div class="t">${temp(b.temp)}</div>
                <div class="meta">${icon("water", 16)}${esc(precip(b.precip, unit))}</div>
              </div>
            </div>`,
            )
            .join("")}
        </section>
        <div class="rule"></div>
        <section class="days">
          ${days
            .map(
              (d) => `
            <div class="day">
              <div class="name">${esc(this._weekday(d.datetime))}</div>
              ${icon(CONDITION_ICON[d.condition] || "alert-circle-outline", 36)}
              <div class="hl">${temp(d.temperature)} <span class="lo">/ ${temp(d.templow)}</span></div>
              <div class="meta">${icon("water", 15)}${esc(precip(d.precipitation, unit))}</div>
            </div>`,
            )
            .join("")}
        </section>`;
    const outlook = c.layout === "focus" ? [this._outlookRow("today"), this._outlookRow("tomorrow")] : [];
    const focusBody =
      c.layout !== "focus"
        ? ""
        : `
        <section class="fclock${c.show_clock_area ? " outline" : ""}"></section>
        <div class="rule"></div>
        <section class="nowrow">
          <div class="ncell">
            ${icon(weather ? this._conditionIcon(weather.state, night && weather.state === "partlycloudy") : "alert-circle-outline", 40)}
            <div class="v">${temp(wa.temperature)}</div>
            <div class="t">${esc(CONDITION_TEXT[weather?.state] || weather?.state || "Unavailable")}</div>
          </div>
          <div class="vrule"></div>
          <div class="ncell">
            ${icon("home-thermometer-outline", 30)}
            <div class="v">${inside ? temp(inside.state) : "–"}</div>
            <div class="t">Inside</div>
          </div>
          <div class="vrule"></div>
          <div class="ncell">
            ${sunEvent ? `${icon(sunEvent.icon, 30)}<div class="v" style="font-size:24px">${esc(sunEvent.time)}</div><div class="t">${esc(sunEvent.label)}</div>` : ""}
          </div>
        </section>
        <div class="rule"></div>
        <section class="outlook">
          ${outlook
            .map(
              (o) => `
            <div class="orow">
              ${icon(o.icon, 32)}
              <div class="lbl">${esc(o.label)}</div>
              <div class="txt">${esc(o.text)}</div>
              <div class="rng">${esc(o.range)}</div>
              <div class="wear">${o.wear ? `${icon(o.wear.icon, 24)}<span>${esc(o.wear.text)}</span>` : ""}</div>
            </div>`,
            )
            .join("")}
        </section>
        <div class="rule"></div>
        <section class="fdays">
          ${days
            .map(
              (d) => `
            <div class="fday">
              <div class="top">${esc(this._weekday(d.datetime))}${icon(CONDITION_ICON[d.condition] || "alert-circle-outline", 24)}</div>
              <div class="hl">${temp(d.temperature)} <span class="lo">/ ${temp(d.templow)}</span></div>
              <div class="meta" style="font-size:14px">${icon("water", 13)}${esc(precip(d.precipitation, unit))}</div>
            </div>`,
            )
            .join("")}
        </section>`;

    this.shadowRoot.innerHTML = `
      <style>
        /* Inherited properties (HA sets line-height, fonts, colours on the page)
           cross the shadow boundary; reset them so the layout is pixel-stable. */
        :host { all: initial; display: block; }
        .screen {
          position: relative; width: 800px; height: 480px; overflow: hidden;
          background: #fff; color: #000;
          font-family: "EinkInter", sans-serif; font-feature-settings: "tnum" 1;
          font-size: 16px; font-weight: 500; line-height: normal; letter-spacing: normal;
          text-align: left; text-transform: none; white-space: normal;
          display: grid;
        }
        .screen.standard { grid-template-rows: 186px 1px 118px 1px 122px 52px; }
        .screen.focus { grid-template-rows: 214px 1px 54px 1px 100px 1px 61px 48px; }
        .icon { fill: #000; flex: none; display: block; }
        .label { font-size: 15px; font-weight: 600; letter-spacing: 0.02em; }
        /* 1px dotted rules: alternate pixels read as a light grey line in 1-bit */
        .rule { margin: 0 16px; background: repeating-linear-gradient(90deg, #000 0 1px, transparent 1px 3px); }
        .vrule { width: 1px; margin: 14px 0;
          background: repeating-linear-gradient(180deg, #000 0 1px, transparent 1px 3px); }

        .hero { display: grid; grid-template-columns: 214px 372px 214px; }
        .now { display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 2px; }
        .now-top { display: flex; align-items: center; gap: 6px; }
        .now-temp { font-size: 54px; font-weight: 700; line-height: 1; }
        .now-cond { font-size: 18px; font-weight: 600; margin-top: 6px; }
        .hilo { display: flex; gap: 10px; font-size: 17px; font-weight: 600; margin-top: 2px; }
        .hilo span { display: inline-flex; align-items: center; gap: 1px; }
        .clock { margin: 10px 0; }
        .clock.outline { outline: 2px dashed #000; outline-offset: -2px; }
        .stats { display: flex; flex-direction: column; justify-content: center; gap: 12px; padding-left: 22px; }
        .stat { display: flex; align-items: center; gap: 10px; }
        .stat-value { font-size: 26px; font-weight: 700; line-height: 1.05; }

        .today { display: grid; grid-template-columns: 1fr 1px 1fr 1px 1fr; }
        .bucket { display: flex; align-items: center; justify-content: center; gap: 14px; }
        .bucket .name { font-size: 17px; font-weight: 700; }
        .bucket .t { font-size: 40px; font-weight: 700; line-height: 1.05; }
        .meta { display: flex; align-items: center; gap: 3px; font-size: 16px; font-weight: 600; }

        .days { display: grid; grid-template-columns: repeat(${days.length || 1}, 1fr); }
        .day { display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 1px; }
        .day .name { font-size: 17px; font-weight: 700; }
        .day .hl { font-size: 20px; font-weight: 700; }
        .day .hl .lo { font-weight: 500; }

        footer { display: flex; align-items: stretch; gap: 10px; padding: 6px 10px 8px; min-width: 0; }
        .banner { flex: 1; min-width: 0; display: flex; align-items: center; gap: 20px; overflow: hidden;
          background: #000; color: #fff; border-radius: 8px; padding: 0 14px; }
        .banner .icon { fill: #fff; }
        .banner .item { display: inline-flex; align-items: center; gap: 6px; flex: none;
          font-size: 19px; font-weight: 600; white-space: nowrap; }
        /* The message and the door summary shrink and ellipsize; door items don't. */
        .banner .item.shrink { flex: 0 1 auto; min-width: 0; }
        .banner .item > span { overflow: hidden; text-overflow: ellipsis; }
        .spacer { flex: 1; }

        /* focus layout */
        .fclock { margin: 8px 16px 6px; }
        .fclock.outline { outline: 2px dashed #000; outline-offset: -2px; }
        .nowrow { display: grid; grid-template-columns: 1.3fr 1px 1fr 1px 1fr; }
        .ncell { display: flex; align-items: center; justify-content: center; gap: 10px; }
        .ncell .v { font-size: 32px; font-weight: 700; line-height: 1; }
        .ncell .t { font-size: 17px; font-weight: 600; }
        .outlook { display: grid; grid-template-rows: 1fr 1fr; padding: 2px 16px; }
        .orow { display: grid; grid-template-columns: 34px 112px 1fr 92px 200px; align-items: center; column-gap: 8px; }
        .orow .lbl { font-size: 19px; font-weight: 700; }
        .orow .txt { font-size: 19px; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
        .orow .rng { font-size: 21px; font-weight: 700; text-align: right; }
        .orow .wear { display: flex; align-items: center; gap: 6px; font-size: 17px; font-weight: 600;
          white-space: nowrap; overflow: hidden; padding-left: 10px; }
        .fdays { display: grid; grid-template-columns: repeat(${days.length || 1}, 1fr); }
        .fday { display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 1px; }
        .fday .top { display: flex; align-items: center; gap: 6px; font-size: 16px; font-weight: 700; }
        .fday .hl { font-size: 17px; font-weight: 700; }
        .fday .hl .lo { font-weight: 500; }
        .updated, .battery { display: flex; align-items: center; gap: 4px; font-size: 15px; font-weight: 600; flex: none; }
      </style>
      <div class="screen ${c.layout === "focus" ? "focus" : "standard"}">
${c.layout === "focus" ? focusBody : standardBody}
        <footer>
          ${
            bannerItems.length
              ? `<div class="banner">${bannerItems
                  .map(
                    (it) =>
                      `<span class="item${it.shrink ? " shrink" : ""}">${icon(it.icon, 22)}<span>${esc(it.text)}</span></span>`,
                  )
                  .join("")}</div>`
              : `<div class="spacer"></div>`
          }
          <div class="updated">Updated ${esc(this._clockTime(Date.now()))}</div>
          ${battery ? `<div class="battery">${icon(battery.icon, 22)}${battery.level}%</div>` : ""}
        </footer>
      </div>`;
  }
}

customElements.define("eink-dashboard-card", EinkDashboardCard);

window.customCards = window.customCards || [];
window.customCards.push({
  type: "eink-dashboard-card",
  name: "E-ink dashboard",
  description: "Full-screen 800x480 1-bit dashboard for an ESPHome e-paper panel (rendered via Puppet).",
});
