# Bluetooth e-paper shelf labels (aida)

Two Poshiji four-colour (black/white/red/yellow) electronic shelf labels
with factory firmware, written directly over Bluetooth LE by Home
Assistant's `ble_esl` integration ([eigger/hass-ble-esl]). They need no
base station, no Poshiji cloud or app, and no reflashing. HA's own
bluetooth stack on aida's onboard adapter connects to each tag over GATT
and sends a full-screen image; the integration renders it from an
[imagespec] payload.

| What | Where |
|---|---|
| Integration + its Python deps (blesession, imagespec, pystrich) | `pkgs/home-assistant-ble-esl/` → `services.home-assistant.customComponents` |
| Layouts, render script, schedule (HA package `ble_esl`) | `hosts/aida/home-assistant/ble-esl.yaml` |
| Package wiring | `homeassistant.packages.ble_esl` in `modules/home-automation.nix` |
| Bluetooth | `hardware.bluetooth` in `modules/home-automation.nix` (aida's onboard Realtek, `hci0`, `0C:CD:B4:57:60:3C`) |
| Designer (interactive layout editor, from the integration) | HA sidebar → BLE ESL |

[eigger/hass-ble-esl]: https://github.com/eigger/hass-ble-esl
[imagespec]: https://github.com/eigger/imagespec

## Hardware

Identified from the printed rear labels and the factory boot screen, and
confirmed from each tag's advertisement. Under manufacturer ID `0x5258`
(XTE) the tag advertises its device number, which is how the integration
picks the model.

| | 4.2" | 2.9" |
|---|---|---|
| Printed model / BN | PSJ-420 / `PSJ4206GP` | PSJ-290 / `PSJ2906GE` |
| Panel | 400×300 BWRY | 296×128 BWRY |
| Boot-screen ID | `373916105BA9` | `373918100C3F` |
| BLE address | `A9:5B:10:16:39:37` | `3F:0C:10:18:39:37` |
| Firmware (advertised) | 4.0.3 | 4.0.2 |
| XTE device number | 153 (`0x99`) | 154 (`0x9a`) |
| HA device | `ESL 4.2" (PSJ-420)` | `ESL 2.9" (PSJ-290)` |
| Entities | `*.esl_420_*` | `*.esl_290_*` |

The boot-screen ID is the BLE address with its bytes reversed. The BN is
not an identifier the integration uses.

## Layouts

Both labels show NWS weather (`weather.nws_khpn`, `twice_daily` +
`hourly`), rendered by `script.esl_weather_render`:

- **4.2"**: date and "as of" header; current icon, temperature, condition,
  humidity and wind; the next four NWS day/night periods (red for daytime
  highs, black for overnight lows, precipitation shown when ≥ 10%); and a
  24-hour temperature line.
- **2.9"** (kids' room): the day to get ready for, **today before noon and
  tomorrow from noon on**, filling the screen. A weekday banner sits over
  the condition icon, then the high (red, ▲ glyph) and the low (▼ glyph)
  at 56/52 px; a three-character temperature like "101°" steps both down
  a size. Below them is the E1001 and
  kitchen cards' outlook line: rain timing with its chance ("Rain 1–4 PM
  (70%)"), or the sky and any lesser chance ("Cloudy (30% 2–5 PM)",
  "Dry, sunny"). The clothing hint ("Umbrella, jacket") sits at the right of
  that line when it fits. Everything covers that day's outdoor hours,
  7 AM–10 PM (the cards' `outdoor_hours: [7, 22]`; for today, what is left
  of them), from the hourly forecast; the high also takes NWS's daytime
  high. A one-line footer has the current temperature, condition and "as
  of" stamp. The script's `forecast_day` field (`auto`/`today`/`tomorrow`)
  forces either view for previewing. If today has no daytime period left,
  it shows the next day, labelled as such.

  The outlook is a Jinja port of `outlookRow()` in
  `pkgs/lovelace-common/weather.js`, with the same thresholds and wordings.
  If you change one, change the other. It was checked against the JS on
  eleven synthetic days (2026-11-07); all matched. The line is fitted like
  the card's `_fitText`: the fullest wording plus the hint; else drop the
  hint; else a shorter wording; else the tersest at 80% size. Widths come
  from per-character advance tables of the bundled fonts (`adv_bold`,
  `adv_regular`), which slightly overestimate.

`automation.esl_refresh_weather_labels` runs the script at 06, 09, 12, 15,
18 and 21:00. When NWS is unavailable or returns too few periods the run
stops, so the labels keep their last good image. The labels have no clock,
so the "as of" stamp shows how old the reading is.

Both writes are `ble_esl.write_guarded`, and both tags have **Prevent
Duplicate Send** on: an image identical to the last one sent returns
`status: duplicate` and never touches the radio. The last-sent image hash
survives HA restarts. Each tag's write uses `continue_on_error`, so one
failing tag does not stop the other.

### Editing a layout

1. Edit `hosts/aida/home-assistant/ble-esl.yaml`.
2. `nixos-deploy aida` (HA reloads or restarts to pick it up).
3. Preview: Developer Tools → Actions → `script.esl_weather_render` with
   `dry_run: true` (and `forecast_day` to see the other 2.9" view), then open `image.esl_420_preview_content` /
   `image.esl_290_preview_content`. This renders but sends nothing.
4. Send: run the automation, or the script without `dry_run`.

Payload notes for this file:

- Coordinates are in the panel's viewed (landscape) orientation; the
  integration rotates the PSJ-290's portrait buffer itself.
- Stick to `black`, `white`, `red`, `yellow`; anything else is quantized.
  Yellow text on white is hard to read, so use yellow as a fill behind
  black text.
- Fonts are the integration's bundled set (`fonts/NotoSansKR-*.ttf` covers
  Latin); icons are `mdi:*`.
- HA's sandboxed templates refuse attributes that shadow dict methods, so
  a key named `pop` (etc.) fails with `SecurityError`. That is why the
  precipitation key is `rain`.
- HA's package merge **drops empty mappings**. Write `selector: {boolean:}`
  style values (null) rather than `{}`, or the script fails validation
  ("Only one type can be specified").
- `text_fit` sizes against the full line height. With a tight `height` it
  shrinks far below `size`, so use plain `text` for short strings.
- Anchor big numbers at their baseline (`anchor: ls`). The default anchor
  puts the glyph box's top at `y`, and on the panel the high climbed into
  the banner.
- A `stack`/`row` with no `height` is as tall as the canvas, so
  `align: center` puts its children off-screen. Give it a height.

## State that is not in the repo

These live in HA's `.storage`, not in Nix. Redo them if a tag is removed
and re-added:

1. **Config entries.** Each tag is auto-discovered; confirm it under
   Settings → Devices & Services (one click, no fields). The model is set
   from the advertisement.
2. **Options** (per tag, Configure): Prevent Duplicate Send **on**;
   Retry Count 3, Debounce 0 (defaults).
3. **Names.** Rename the device to `ESL 4.2" (PSJ-420)` / `ESL 2.9" (PSJ-290)`
   and its entity IDs from `*.poshiji_<id>_*` to `*.esl_420_*` / `*.esl_290_*`.
   `ble-esl.yaml` targets `image.esl_420_last_updated_content` and
   `image.esl_290_last_updated_content`.

## Measurements (2026-11-07, tags on a desk near aida)

| | PSJ-420 | PSJ-290 |
|---|---|---|
| Advertised RSSI at discovery | −81 dBm | −71 dBm |
| RSSI during writes | −53 to −63 | −35 to −39 |
| Colour test write (total / connect / GATT session / disconnect) | 4.2 s / 0.8 / 1.1 / 2.2 | 7.4 s / 4.5 / 0.8 / 2.1 |
| Weather layout write | 6.4 s, 9,680 bytes | 6.9 s, 5,584 bytes |
| Attempts needed | 1 every time | 1 every time |
| Battery (advertised) | 99–100% | 100% |

"Total" is the BLE transaction as the integration times it
(`sensor.esl_*_write_duration`; its attributes break it down). The tag's
finish acknowledgement means it accepted the image, **not** that the
e-paper finished redrawing. The physical refresh happens afterwards and has
not been timed yet. The fixed cost is the connect/disconnect, not the
transfer: the image itself moves in well under a second.

Telemetry per tag: `sensor.esl_*_battery` (from advertisements; the tag
does not report temperature), `sensor.esl_*_signal_strength` (disabled by
default), `sensor.esl_*_write_duration`, `sensor.esl_*_failure_count`,
`sensor.esl_*_last_failure_time`, `binary_sensor.esl_*_display_in_sync`,
`image.esl_*_last_updated_content` (what was last sent).

## Bluetooth range

The onboard adapter has been enough so far: every write succeeded first
try, with no proxy. `automation.esl_notify_on_failed_write` pushes a phone
notification (`notify.mobile_app_motorola_razr_plus_2024`, channel
"E-paper labels") whenever a tag's `failure_count` goes up, i.e. a write
failed with all retries used. The notification names the tag, the stage it
failed at, its RSSI and the error. So after moving a tag, no checking is
needed; a bad spot announces itself at the next refresh that changes its
image. Writes that a retry rescued don't alert; for those, look at the
write-duration attributes (`attempt`, `connect_s`, `rssi`). If a tag
keeps failing, add an ESPHome Bluetooth proxy with
`bluetooth_proxy: {active: true}`; passive proxies only forward
advertisements and cannot write. All tags share one BLE lock, so writes go
one at a time.

## Open items

- Time the physical e-paper refresh for each panel (from the write
  finishing to the image settling).
- Battery life: the panels currently update up to six times a day, less
  whatever the duplicate guard skips. Watch `sensor.esl_*_battery`
  over the coming weeks before adding more updates.
- No met.no fallback: an NWS outage just leaves the last image up.
- Phase 2 from the original plan (feeding a Puppet render through
  imagespec's `dlimg`) isn't needed for these layouts. It remains an
  option if a layout ever outgrows imagespec.
- Upgrades: bump `version`/`hash` in `pkgs/home-assistant-ble-esl/` and
  check the manifest `requirements` against `blesession.nix`/`imagespec.nix`.
  nixpkgs' manifest check fails the build if they drift.
