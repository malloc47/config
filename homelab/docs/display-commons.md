# Display commons: shared notices, outlook and schedule for every display

Status: **design, not started** (2026-11-07; schedule directives and the
voice assistant's display added 2026-11-08). Paths are relative to
`homelab/` unless written `config: <path>`.

aida drives several displays of very different kinds. Each one grew its own
copy of the same few capabilities: notices, the weather outlook and the
kids' schedule. This document proposes computing those capabilities once in
Home Assistant, as a small number of native entities, and leaving each
display only the job of rendering them for its size, colours and update
rate. Adding or improving a capability should then not require thinking
about individual devices. For example, a notice whose text changes every
minute should degrade gracefully on slow panels, and its priority should
decide whether it appears at all on a small screen.

## Goals

- One implementation per capability, used by every display (no "keep in
  step with ..." ports).
- A notice or outlook item says what it is: its priority, its wordings
  from long to short, how often each wording changes, and how long it
  stays relevant. Every display then derives what to show from that plus
  its own profile.
- The data is native HA entities that any dashboard or automation can use.
  Keep the entity count small: a handful of feed entities, not one per
  notice or per display.
- A display redraws only when what *it* would show changes.
- Adding a notice source, or a display, is a local change.

Non-goals: moving the kitchen card's interactive pieces (strip toggle,
collapse, settings, sky simulation) or the sky scenery server-side; those
are presentation and stay in the cards.

## The displays today

| Display | Renderer | Updates | Constraints |
|---|---|---|---|
| Kitchen tablet (Apolosign 15.6", Kiosk Satellite) | `family-calendar-card` (JS, live in the browser) | live | large, touch, colour; animations cost CPU |
| Living-room Fire HD 8 (Kiosk Satellite) | built-in cards only (`living-room-dashboard.yaml`) | live | 1.5 GB RAM, no custom cards |
| E1001 (living room, 7.5" e-paper) | `eink-dashboard-card` → Puppet PNG; firmware draws clock/date/battery | 15-min poll + HA push (`eink_panel_refresh`) | 800×480, 4 grays, partial refreshes, ETag/304 |
| Kindle Paperwhite 3 (bedroom) | `kindle-weather-card` → Puppet PNG | hourly wake | 1072×1448, 16 grays, full redraw, battery |
| ESL 2.9" (kids' room) | Jinja + imagespec in `ble-esl.yaml` | 6 AM–9 PM every 3 h | 296×128, BWRY, BLE, battery |
| ESL 4.2" | same | same | 400×300, BWRY |
| Living-room voice assistant (Waveshare ESP32-S3 1.85C-BOX) | ESPHome firmware, LVGL (`hosts/aida/esphome/living-room-voice-assistant.yaml`) | live (HA state pushes) | 360×360 round LCD; LVGL fonts carry a compiled glyph set (Montserrat), icons only from a compiled list; a conversation, timer or clock owns the screen most of the time |
| Phone (mobile_app notify) | HA notifications | on event | interrupts; only for what's urgent |

## Where the shared logic lives today

| Capability | Implementations | Drift already present |
|---|---|---|
| Notices (banner message, Humira day, dishwasher, open doors) | `pkgs/lovelace-common/banner.js` (kitchen, E1001); Jinja port in `ble-esl.yaml` | the Humira rule is copied into 3 configs (the ESL's hard-coded); the ESL drops dishwasher/doors by its own rule ("moment-to-moment"); the Kindle shows none |
| Weather outlook (buckets, precipitation wording, what to wear) | `pkgs/lovelace-common/weather.js` (3 cards); ~300-line Jinja port in `ble-esl.yaml` | the rain-from-forecast fix (`rain_from_forecast`) is only in the kitchen card |
| Kids' routine / exception days | `family-calendar-card.js` `_regular()`; Jinja port in `ble-esl.yaml` | the lookaround fix is only in the card; `title_rewrites` copied twice |
| Freshness stamp | `pkgs/lovelace-common/stamp.js` (E1001, Kindle); the ESL's own "as of" | — |
| Fitting text (long → short wordings, overflow) | `_fitText` in two cards, Jinja advance-width tables, the kitchen bar's overflow levels | four variants of one idea |
| When to redraw | `eink_panel_refresh` lists the sources by hand; ESL fixed schedule; Kindle timer | the trigger list is a second copy of what the card shows |

The common cause: every capability is computed client-side, per renderer,
and the ESL, which can't run the JS, gets a hand-ported copy.

## Architecture

```
calendars, weather, appliances, doors, todo.notices, input helpers
                         │
                         ▼
     household integration (Python, in-repo, unit-tested)
  ┌────────────────────┬──────────────────────┬────────────────────┐
  │ sensor.household_  │ sensor.weather_      │ sensor.family_     │
  │ notices            │ outlook              │ schedule           │
  └────────────────────┴──────────────────────┴────────────────────┘
        │  items + per-profile selections + per-profile revisions
        ▼
  display profiles (Nix) ──► renderers: lovelace-common (cards, Puppet),
                             ESL (imagespec or Puppet + dlimg),
                             ESPHome/LVGL firmware (flat attributes),
                             Markdown cards, phone notify
```

### 1. Feed entities

Three sensors, plus one native to-do list:

- **`sensor.household_notices`**: state is the number of active notices;
  attributes are `items` (the notice contract below), `profiles` (each
  display's selection) and `revisions` (a hash per display profile). For
  profiles marked `flat`, there are also plain string attributes
  `<profile>_text` and `<profile>_icon` (the selection already rendered to
  one line). ESPHome reads an attribute as its string form, so firmware
  can't usefully read the nested `profiles` value.
- **`todo.notices`** (HA's local to-do integration): ad-hoc notices.
  Add one from the HA app, or from an automation with `todo.add_item`. The
  due date is the expiry, and the description carries options in the same
  `#notice` syntax as calendar events (below). It replaces
  `input_text.eink_banner` once every display reads the feed; until then the
  input_text is mapped in as one more source.
- **`sensor.weather_outlook`**: the current condition (with the
  rain-from-forecast correction), today/tomorrow/later buckets, each with
  its wordings from long to short, what to wear, high/low and a
  precipitation chance.
- **`sensor.family_schedule`**: the next ~8 days. For each kid: the
  routine (learned over the lookaround weeks), exception days with what's
  missing, the day's items, and pickups. The calendar list, its precedence,
  dedupe and `title_rewrites` live in this one place.

The recorder should exclude all three sensors, since their attributes churn
and history isn't useful. Each must stay well under HA's ~16 KB attribute
warning: the feeds hold days and items, not raw calendar data.

### 2. The notice contract

```yaml
- id: dishwasher                 # stable, for dedupe and dismissal
  icon: dishwasher               # an MDI name; every renderer can draw one
  priority: normal               # critical | high | normal | low
  relevant_for: 2h               # stale on a display refreshing less often
  rooms: [kitchen, living]       # omitted = everywhere
  forms:                         # best first; `changes` = how often this wording changes
    - { text: "Dishwasher 45% · 1 h 12 min left", changes: minute }
    - { text: "Dishwasher 45%", changes: 10min }
    - { text: "Dishwasher running", changes: event }
  since: 2026-11-07T19:02:00-05:00
  until: null                    # an expiry, when known
  source: appliance              # appliance | doors | calendar | todo | template
```

A renderer applies these rules, in this order:

1. Drop notices not meant for the display's rooms, below its minimum
   priority, or whose `relevant_for` is shorter than the display's refresh
   interval. `critical` is exempt from the last two.
2. Sort by priority, then by how recent they are.
3. For each notice, take the first form that **changes no faster than the
   display refreshes** and **fits** the space left. If none fits, show the
   icon alone where the profile allows, or drop the notice.
4. Stop when the profile's slots run out.
5. Adapt the result to the profile's character set and icon list. A
   `charset: basic` profile gets the text with typographic characters
   replaced ("·" → "-", "—" → "-", curly quotes → straight; anything else
   outside the set dropped). An icon not in the profile's `icons` list
   becomes its `fallbackIcon`. Firmware fonts then never show a missing
   glyph.

The selection runs server-side, so each profile's list arrives ready in
`profiles.<name>`; the JS and Jinja renderers only lay it out. Fitting to
pixel widths still happens in the renderer, which knows its fonts: the
server hands it the candidate forms in order, already filtered by update
rate.

Worked example, the dishwasher:

| Display | Result |
|---|---|
| Kitchen (live) | "Dishwasher 45% · 1 h 12 min left", ticking each minute; "Dishwasher 45%" when the bar is full |
| E1001 (push, partial) | "Dishwasher 45%"; its revision changes per 10% step, so it redraws only then |
| Kindle (hourly), ESL (3 h) | not shown: relevant for ~2 h, but they refresh too rarely to keep it current |
| a `critical` leak alert | shown everywhere, including the 2.9" |

### 3. Display profiles

Declared once in Nix and passed to the integration's config. The cards
read their own profile name from their card config.

```nix
displays = {
  kitchen = { refresh = "live"; slots = 6; minPriority = "low"; rooms = [ "kitchen" ]; };
  living-tablet = { refresh = "live"; slots = 3; minPriority = "normal"; rooms = [ "living" ]; };
  e1001 = { refresh = "5min"; slots = 3; minPriority = "normal"; rooms = [ "living" ]; };
  kindle = { refresh = "1h"; slots = 1; minPriority = "high"; rooms = [ "bedroom" ]; };
  esl-290 = { refresh = "3h"; slots = 1; minPriority = "high"; rooms = [ "kids" ]; };
  esl-420 = { refresh = "3h"; slots = 2; minPriority = "normal"; rooms = [ "kids" ]; };
  voice-living = {
    refresh = "live"; slots = 1; minPriority = "high"; rooms = [ "living" ];
    flat = true; charset = "basic";
    icons = [ "needle" "alert" "alarm-light" "dishwasher" "door-open" "trash-can-outline" ];
    fallbackIcon = "bell-outline";
  };
  phone = { refresh = "event"; minPriority = "critical"; };
};
```

`flat`, `charset` and `icons` exist for firmware-native displays (ESPHome
with LVGL or a `display:` lambda), whose fonts and icon glyphs are fixed at
build time. The `icons` list stays in step with the glyphs compiled into the
firmware: generate both from the same Nix list where possible.

### 4. Refresh driven by revisions

`revisions.<profile>` is a short hash of exactly what that profile would
show: its selected items in the forms it can use. A display's push
automation triggers on its own revision, with the profile's minimum
interval for battery devices:

- The E1001's hand-written trigger list (`eink_panel_refresh` in
  `modules/home-automation.nix`) becomes one trigger.
- A countdown that ticks every minute changes only the kitchen's revision,
  so the e-paper panels never wake for it.
- The ESLs can move from a fixed 3-hour schedule to "on change, at most
  every N hours". Their Prevent Duplicate Send guard stays as a second
  safety net.
- `stamp.js` keeps its role ("Updated HH:MM" moves only with the content)
  and could take its time from the revision change instead of hashing
  markup.

### 5. Thin renderers

- `pkgs/lovelace-common/`: `banner.js` reads `profiles.<name>` and keeps
  only layout and fitting; `weather.js` reads `sensor.weather_outlook` for
  the outlook rows (it keeps its forecast subscription for the Kindle's
  hourly chart, which is presentation). The kitchen strip reads
  `sensor.family_schedule`; its client-side toggles stay.
- Fire HD 8: a built-in Markdown card over `sensor.household_notices`. Any
  dashboard can show the notices this way.
- Phone: one automation sends notifications for new `critical`/`high`
  notices from the phone profile, deduplicated by `id`.
- Voice assistant (and future ESPHome displays): two `text_sensor`s with
  `platform: homeassistant` read `sensor.household_notices`'
  `voice-living_text` and `voice-living_icon` attributes. The LVGL page
  shows them under the clock and date only while the assistant is idle;
  listening, a reply or a running timer take precedence, as now.
  `critical` could also tint the phase ring. Updates arrive as HA state
  pushes, so the profile is `live`: firmware redraws only changed labels,
  and the selection already absorbed the minute-by-minute forms (`slots =
  1`, `minPriority = "high"`, so a dishwasher countdown never reaches it).
- ESLs, two options:
  1. Keep imagespec, but read the feeds: the Jinja shrinks to layout, with
     no outlook, routine or notice logic.
  2. **Render through Puppet** with `dlimg` (imagespec places a downloaded
     image): a Lovelace card sized 296×128 / 400×300, screenshotted in the
     BWRY palette. All displays then share one rendering stack, with real
     font measurement instead of advance-width tables. Do a spike first to
     judge small-text legibility in four colours.

### Where the feed code lives

A small **in-repo custom integration** (`pkgs/home-assistant-household/`,
packaged like `home-assistant-toniebox` and `home-assistant-gtfs2`), in
Python, with pytest fixtures that replay recorded calendars and forecasts.
Template sensors were considered: they need no new code type, but
`ble-esl.yaml` shows what ~1,400 lines of Jinja cost to keep correct, and
the routine logic needs tests. Its surface stays small: the three sensors,
optional `household.post_notice` / `household.clear_notice` services (the
to-do list may make them unnecessary), and the profile config from Nix.

## Event directives: calendar events as notices and schedule hints

Calendar events can carry instructions for the feeds, written in the
events themselves rather than in per-display config. One syntax covers two
uses: `#notice` makes an event a notice, and the schedule directives
(`#routine`, `#once`, `#exception`, `#pickup`, `#marker`) correct
`sensor.family_schedule` where its heuristics guess wrong. Today both are
config: a notice from a calendar takes a rule per card (`calendar_alerts:
{entity, match: "^Humira$", icon}`, copied into three configs), and the
schedule's guesses are tuned with `min_days`, `pickup_match` and a 15-minute
marker cutoff (copied into the card and the ESL Jinja). With directives, any
event on any calendar, now or later, can become a notice or steer the
schedule with no config change.

### Directive syntax

One directive per line in the event description: `#<name>`, then bare
words and `key=value` pairs (quote values with spaces). Lines are
case-insensitive and read after HTML is stripped (`<p>`, `<br>` and
entities like `&amp;`, which Google returns for rich-text descriptions).
Several directives can share an event (one per line), unknown directives
and keys are ignored (reserved for later), and a malformed line is ignored
and logged once. A directive on a recurring series applies to every
occurrence.

### Notice mechanisms considered

| Mechanism | Example | For | Against |
|---|---|---|---|
| Config rule (today) | `match: "^Humira$"` | works on calendars you can't edit (Holidays, subscribed school calendars) | a config change per notice; copied per display |
| Dedicated "Notices" calendar | everything on it is a notice | simplest; colour-coded in Google | events also belong on a person's calendar; duplication |
| Title emoji | `💉 Humira` | visible in the calendar, fast to type on a phone | carries only "notice + icon"; the emoji shows on every display (strip it, or map it to MDI, since e-ink and ESL fonts have no emoji) |
| Description directive | a line `#notice high icon=needle` | carries any field; invisible in calendar grids; set once on a recurring series, it covers every instance | not visible at a glance; descriptions arrive as HTML |

### Notices: a directive, an emoji shorthand, and rules for foreign calendars

1. **`#notice`** (the general mechanism):

   ```
   #notice
   #notice high
   #notice critical icon=water-alert rooms=kitchen,living
   #notice text="Humira tonight" short="Humira" from=-1d until=end
   ```

   | Key | Default | Meaning |
   |---|---|---|
   | (bare word) | `normal` | priority: `low`, `normal`, `high`, `critical` |
   | `icon` | from the emoji, else `bell-outline` | MDI name |
   | `text` / `short` | the title (emoji stripped, `title_rewrites` applied) | the long and short forms |
   | `rooms` | everywhere | profile room tags |
   | `from` | the event's first day, 00:00 | when it starts showing: `-1d` the day before, `-2h` before the start, `start` at the start |
   | `until` | the event's last day, 23:59 | `end` at the end; or a duration such as `+2h` |

   The notice's `id` is the event's UID plus the instance date, so a
   recurring series yields one notice per occurrence. Unknown keys are
   ignored (reserved for later); a malformed line is ignored and logged
   once.

2. **Title emoji shorthand** for the common case. A leading emoji from a
   small mapping table makes the event a notice with that icon and a
   default priority. The emoji is stripped from what displays show:

   | Emoji | Icon | Priority |
   |---|---|---|
   | 💉 💊 | `needle` / `pill` | high |
   | ⚠️ | `alert` | high |
   | 🚨 | `alarm-light` | critical |
   | 🗑️ ♻️ | `trash-can-outline` / `recycle` | normal |
   | 🎒 | `bag-personal-outline` | normal |
   | 📣 | `bullhorn-outline` | normal |

   An emoji not in the table still makes a `normal` notice with
   `bell-outline`. A `#notice` line in the description overrides the
   shorthand field by field.

3. **Config rules** stay for calendars you can't edit (Holidays, the
   school's published calendars), in the integration's config: one place,
   not per display.

Humira migrates in one of two ways: rename the series "💉 Humira", or add
`#notice high icon=needle` to its description; either way the rule
disappears from three configs. School closures already on the Family
calendar ("Transfig: no aftercare — Staff dev.") become notices with
`#notice high rooms=kitchen,kids from=-1d`, so they show the evening
before.

### Schedule directives

`sensor.family_schedule` keeps its heuristics as defaults: a routine is a
series on at least `min_days` weekdays of a week within the lookaround;
pickups match `pickup_match`; events of 15 minutes or less are markers
(pins such as the bus). Directives override them per event:

| Directive | Effect | Example |
|---|---|---|
| `#routine` | part of its row's routine even below `min_days` (a new arrangement starting midweek, a routine that's only twice a week) | Bastian's Tuesday/Thursday swim: `#routine` |
| `#once` | never part of a routine; always shown as that day's own item, even when it repeats | a lesson that happens to fall on three days one week |
| `#exception` | the days it covers are exception days for a row, with its wording, even when the routine events are all present; `for=` names the rows (calendar names), needed on shared calendars like Family; `text=` / `short=` give the wording | Family's "Transfig: no aftercare — Staff dev.": `#exception for=priscilla text="No after-care" short="No aftercare"` |
| `#pickup` | the row's pickup pin (shared between rows when the times agree), whatever its title | "Grandma gets Bastian": `#pickup` |
| `#marker` | a pin rather than a block, whatever its length | a 30-minute drop-off window: `#marker` |

An event can carry both kinds: the staff-development closure above, with

```
#exception for=priscilla text="No after-care"
#notice high rooms=kitchen,kids from=-1d
```

is an exception day on Priscilla's row (kitchen strip, both ESLs) and a
notice from the evening before. School-published calendars you can't edit
keep config rules here too, in the integration's config.

The emoji shorthand stays for notices only: the schedule directives are
rare corrections, better spelled out than encoded in a title.

## Entity budget

| Entity | Notes |
|---|---|
| `sensor.household_notices` | new |
| `sensor.weather_outlook` | new |
| `sensor.family_schedule` | new |
| `todo.notices` | new; native to-do list |
| `input_text.eink_banner` | removed once every renderer reads the feed |

Three to four net new entities, all excluded from the recorder.

## Migration plan

Each step stands alone.

1. **Notices feed.** The integration skeleton with the directive parser
   (shared with step 3), `sensor.household_notices` (sources: appliances,
   doors, the banner input_text, `todo.notices`, `#notice` directives, the
   emoji shorthand and config rules), display profiles from Nix, and
   per-profile revisions and flat attributes. Switch `banner.js` (kitchen,
   E1001) and the ESL notice line to read it. Replace `eink_panel_refresh`'s
   trigger list with the E1001 revision. Mark Humira in the calendar. Add
   the notice line to the voice assistant's idle screen (its two
   `text_sensor`s and LVGL labels, with the icon glyphs generated from the
   profile's `icons`).
2. **Weather outlook feed.** Move the outlook buckets, wordings and what
   to wear (`weather.js`), and the rain-from-forecast correction, into
   `sensor.weather_outlook`; the E1001, Kindle and ESLs gain the
   correction; delete the ESL's Jinja outlook.
3. **Schedule feed.** Move `_regular()`'s routine/exception logic with
   lookaround, dedupe and `title_rewrites` into `sensor.family_schedule`,
   with the schedule directives (`#routine`, `#once`, `#exception`,
   `#pickup`, `#marker`) overriding its heuristics. The kitchen strip and
   both ESLs read it, and the ESL's Jinja routine goes. Then mark the
   closures already on the Family calendar with `#exception for=...`.
4. **ESL via Puppet (spike).** Render one label as a Lovelace card through
   Puppet in BWRY and place it with `dlimg`; adopt it if small text holds
   up.
5. **Optional: dashboards from Nix.** Generate the four storage-mode
   dashboards (kitchen, E1001, Kindle, living room) in YAML mode from Nix,
   so their card configs and profiles come from one declaration. This ends
   pasting into the raw editor, but they can no longer be edited in the UI.

## Testing

- pytest fixtures replay real weeks (calendars as `calendar.get_events`
  returns them, forecasts as `weather.get_forecasts` returns them) through
  each feed. Golden files cover: the short week (half-days + Election Day,
  2026-10-28), DST, a recurring directive, HTML descriptions, overflowing
  profiles.
- The profile selector is a pure function (notices + profile → selection +
  revision + flat text), so the degradation rules, the charset adaptation
  and the icon fallback are tested exhaustively, independent of HA.
- The directive parser gets table tests: HTML descriptions as Google
  returns them, several directives per event, quoting, unknown keys,
  malformed lines, and recurring series.
- Renderers keep their existing checks: Puppet renders on scratch
  dashboards, never the live e-ink ones; ETag/304 across minute rollovers;
  ESL `dry_run` previews.

## Open questions

- Room tags: are kitchen / living / bedroom / kids enough, or should
  profiles also filter by person (e.g. Charis's notices on her own
  dashboard)?
- Should dismissing a notice (a tap in the kitchen) be per display, or
  household-wide (complete the to-do item / record the event UID as
  dismissed)?
- `#exception for=` takes calendar names (`priscilla`): should it also
  accept people, for when a kid's events span several calendars?
- Should the voice assistant also *speak* `critical` notices (an Assist
  announcement on the satellite), as an audio display with its own
  profile?
- Weather alerts from NWS: worth a source (as `high`/`critical`
  notices)?
- Phone notifications: only `critical`, or `high` too, with quiet hours?
