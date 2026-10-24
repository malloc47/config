// family-calendar-card — a full-screen Lovelace card for a touchscreen family
// calendar (the kitchen's Kiosk Satellite tablet), meant as the only card of a
// panel view. It fills the screen (portrait first; landscape moves the clock,
// weather and controls into a side column) with:
//
//   - a clock and date,
//   - current conditions and today/tomorrow outlook lines, worded exactly as
//     on the E1001 e-ink panel (shared ../lovelace-common/weather.js),
//   - chips that show/hide each configured calendar (remembered per device),
//   - a multi-day week grid: all-day and multi-day events as bars across the
//     days, timed events listed under each day; tap an event for details,
//     page with the arrows (it returns to the current week when left idle),
//   - a strip of ordinary Lovelace cards (`cards`) for room controls,
//   - optionally (banner: true) a notification bar along the bottom edge
//     with the E1001's banner notices (shared ../lovelace-common/banner.js),
//     shown only while there is something in it.
//
// Config:
//   type: custom:family-calendar-card
//   calendars:                         # chips, in this order, which is also the
//                                      # precedence for dedupe
//     - entity: calendar.family
//       name: Family                   # default: friendly name
//       color: "#039be5"               # default: from a built-in palette
//       hidden: false                  # start with the chip off (until tapped)
//   dedupe: true                       # hide an event a calendar earlier in the
//                                      # list (and shown) already has: same title
//                                      # ignoring case/punctuation, same times
//   regular_schedule:                  # optional: timed events that repeat on several
//     min_days: 3                      # weekdays of the visible days (same calendar,
//     weekdays: [1, 2, 3, 4, 5]        # title and times) move out of the day columns
//     title: Regular schedule          # into a strip: a row per calendar on a shared
//     pickup_match: "^pick-?up\\b"     # time axis, with the weekdays where any of it
//     pickup: "18:00"                  # is missing at the row's end. Pickup events
//     calendars: [calendar.<kid>, …]   # (title matching pickup_match) that are the
//     day_switch: "18:30"              # same for every row show once, between the
//     mode: regular                    # rows; otherwise each is named ("Bastian
//                                      # pickup"). pickup: a fixed pickup time instead,
//                                      # when the calendars have none. calendars: the
//                                      # rows (default: any calendar with a routine).
//                                      # weekdays: 0 = Sunday … 6 = Saturday
//                                      # A glyph toggle switches the strip (remembered
//                                      # per device; mode is the default) between the
//                                      # routine and the day's schedule: today, or
//                                      # tomorrow from day_switch, rolled forward to
//                                      # the next of the weekdays (so Friday evening
//                                      # through the weekend shows Monday). The day shown leaves
//                                      # the grid, and a row whose day matches its
//                                      # routine exactly takes its routine out of the
//                                      # whole week, as in regular mode. A chevron
//                                      # collapses the strip to its title (also per
//                                      # device), putting its events back in the grid.
//   title_rewrites:                    # shorten titles in the grid, in order (the
//     - match: "Discovery Village "    # popup keeps the full title); match is a
//       replace: ""                    # JS regex, replace may use $1 etc.
//       flags: g                       # default "g"; add "i" to ignore case
//   days: 7                            # columns in the week grid
//   first_day: today                   # today | sunday | monday
//   reset_after: 300                   # s idle before paging back to today (0: never)
//   refresh_interval: 300              # s between event refetches
//   weather_entity: weather.nws_khpn   # plus weather.js's options:
//   fallback_weather_entity: ...       #   rain_probability, outdoor_hours, cold, hot
//   inside_entity: sensor.<indoor temperature>   # optional, beside the outside temp
//   sun_entity: sun.sun                # night icons for the current conditions
//   cards: [...]                       # Lovelace cards for the controls strip
//   controls_columns: 3                # grid columns for those cards (portrait)
//   banner: false                      # bottom notification bar; its notices are
//                                      # configured as on eink-dashboard-card:
//                                      # banner_template, calendar_alerts,
//                                      # appliances, doors_entity (see banner.js)
//   height: auto                       # card height (CSS); auto fills the window
//                                      # below the card's top edge
//
// Events come from HA's calendar REST API (GET /api/calendars/<entity>) for
// every configured calendar, hidden or not, so toggling a chip is instant.

const { Weather, CONDITION_TEXT, num, temp } = await import(
  new URL(`./weather.js${new URL(import.meta.url).search}`, import.meta.url).href
);
const { Banner } = await import(new URL(`./banner.js${new URL(import.meta.url).search}`, import.meta.url).href);

// Google Calendar's event colours, for calendars without a configured colour.
const PALETTE = ["#039be5", "#33b679", "#e67c73", "#f6bf26", "#8e24aa", "#f4511e", "#3f51b5", "#0b8043", "#d50000", "#616161"];

const esc = (s) =>
  String(s ?? "").replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);

// "YYYY-MM-DD" plus n days (calendar arithmetic at UTC noon, DST-proof).
const addDays = (date, n) => {
  const d = new Date(`${date}T12:00:00Z`);
  d.setUTCDate(d.getUTCDate() + n);
  return d.toISOString().slice(0, 10);
};

// Weekday sets as short labels, Monday first: runs of three or more collapse
// to a range ("M–F", "M–W F"), others are listed ("M T Th F").
const DAY_LETTERS = ["Su", "M", "T", "W", "Th", "F", "Sa"];
const dayLabel = (set) => {
  const order = [1, 2, 3, 4, 5, 6, 0].filter((d) => set.has(d));
  const pos = (x) => (x + 6) % 7; // Monday = 0 … Sunday = 6
  const runs = [];
  for (const d of order) {
    const run = runs[runs.length - 1];
    if (run && pos(d) === pos(run[run.length - 1]) + 1) run.push(d);
    else runs.push([d]);
  }
  return runs
    .map((r) => (r.length >= 3 ? `${DAY_LETTERS[r[0]]}–${DAY_LETTERS[r[r.length - 1]]}` : r.map((d) => DAY_LETTERS[d]).join(" ")))
    .join(" ");
};

// Black or white text for a solid background colour.
const ink = (hex) => {
  const m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(hex || "");
  if (!m) return "#fff";
  const [r, g, b] = m.slice(1).map((h) => {
    const c = parseInt(h, 16) / 255;
    return c <= 0.03928 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
  });
  return 0.2126 * r + 0.7152 * g + 0.0722 * b > 0.4 ? "#202124" : "#fff";
};

// Calendar descriptions are often HTML (Google's editor); show them as text.
const plain = (html) => {
  if (!html) return "";
  const withBreaks = String(html).replace(/<br\s*\/?>/gi, "\n").replace(/<\/(p|div|li)>/gi, "\n");
  return (new DOMParser().parseFromString(withBreaks, "text/html").body.textContent || "").replace(/\n{3,}/g, "\n\n").trim();
};

class FamilyCalendarCard extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
    this._wx = new Weather(() => this._schedule());
    this._bn = new Banner(
      () => this._schedule(),
      (ts) => this._wx.local(ts).date,
    );
    this._events = {}; // entity -> normalized events
    this._fetchedRange = null;
    this._offset = 0; // pages away from the current week
    this._hidden = new Set();
    this._cards = [];
    this._calStamp = "";
    this._raf = null;
    this._html = {};
  }

  setConfig(config) {
    if (!Array.isArray(config.calendars) || !config.calendars.length) throw new Error("calendars is required");
    this._config = {
      days: 7,
      first_day: "today",
      reset_after: 300,
      refresh_interval: 300,
      sun_entity: "sun.sun",
      controls_columns: 3,
      height: "auto",
      dedupe: true,
      cards: [],
      ...config,
    };
    this._calendars = this._config.calendars.map((c, i) => ({
      color: PALETTE[i % PALETTE.length],
      ...(typeof c === "string" ? { entity: c } : c),
    }));
    this._rewrites = (this._config.title_rewrites || []).map((r) => {
      try {
        return { re: new RegExp(r.match, r.flags ?? "g"), replace: r.replace ?? "" };
      } catch (err) {
        throw new Error(`title_rewrites: bad match ${JSON.stringify(r.match)}: ${err.message}`);
      }
    });
    this._loadHidden();
    this._mode = null;
    this._collapsed = null;
    this._wx.configure(this._config);
    this._bn.configure(this._config);
    this._built = false;
    this._html = {};
    this._fetchedRange = null;
    this._buildCards();
    this._schedule();
  }

  set hass(hass) {
    this._wx.setHass(hass);
    this._bn.setHass(hass);
    this._hass = hass;
    for (const card of this._cards) card.hass = hass;
    if (this.isConnected) this._subscribe();
    // A calendar entity's state changes as its next event comes and goes (and
    // after HA re-syncs it); refetch soon after.
    const stamp = this._calendars.map((c) => hass.states[c.entity]?.last_updated).join("|");
    if (stamp !== this._calStamp) {
      const first = !this._calStamp;
      this._calStamp = stamp;
      if (!first) this._fetchSoon(2000);
    }
    this._schedule();
  }

  connectedCallback() {
    if (this._hass) this._subscribe();
    this._tick();
    this._fetchedRange = null;
    this._schedule();
  }

  disconnectedCallback() {
    this._wx.unsubscribe();
    clearTimeout(this._tickTimer);
    clearTimeout(this._fetchTimer);
    clearTimeout(this._resetTimer);
    clearInterval(this._refreshTimer);
    this._refreshTimer = null;
    if (this._onResize) window.removeEventListener("resize", this._onResize);
    this._onResize = null;
  }

  getCardSize() {
    return 20;
  }

  // Forecasts, plus the banner message's template when the bar is on.
  _subscribe() {
    this._wx.ensureSubscriptions(this._config.banner ? this._bn.subscriptions() : {});
  }

  // ---- state ------------------------------------------------------------

  _storageKey() {
    return `family-calendar-card:hidden:${location.pathname}`;
  }

  _loadHidden() {
    let saved = null;
    try {
      saved = JSON.parse(localStorage.getItem(this._storageKey()) || "null");
    } catch (_) {
      saved = null;
    }
    this._hidden = new Set(Array.isArray(saved) ? saved : this._calendars.filter((c) => c.hidden).map((c) => c.entity));
  }

  _saveHidden() {
    try {
      localStorage.setItem(this._storageKey(), JSON.stringify([...this._hidden]));
    } catch (_) {
      // Private mode or blocked storage: chips just don't persist.
    }
  }

  _today() {
    return this._wx.local(Date.now()).date;
  }

  _modeKey() {
    return `family-calendar-card:strip:${location.pathname}`;
  }

  // The strip's mode, "regular" or "day": the device's last choice, else the
  // configured default.
  _stripMode() {
    if (!this._mode) {
      let saved = null;
      try {
        saved = localStorage.getItem(this._modeKey());
      } catch (_) {
        saved = null;
      }
      this._mode = saved === "day" || saved === "regular" ? saved : this._config.regular_schedule?.mode === "day" ? "day" : "regular";
    }
    return this._mode;
  }

  _collapseKey() {
    return `family-calendar-card:strip-collapsed:${location.pathname}`;
  }

  // Whether the strip is collapsed to its title (the device's last choice).
  _stripCollapsed() {
    if (this._collapsed === null) {
      try {
        this._collapsed = localStorage.getItem(this._collapseKey()) === "1";
      } catch (_) {
        this._collapsed = false;
      }
    }
    return this._collapsed;
  }

  _toggleStripCollapsed() {
    this._collapsed = !this._stripCollapsed();
    try {
      localStorage.setItem(this._collapseKey(), this._collapsed ? "1" : "0");
    } catch (_) {
      // Private mode or blocked storage: the choice just doesn't persist.
    }
    this._schedule();
  }

  _setStripMode(mode) {
    this._mode = mode;
    try {
      localStorage.setItem(this._modeKey(), mode);
    } catch (_) {
      // Private mode or blocked storage: the choice just doesn't persist.
    }
    this._schedule();
  }

  // The day mode's date: today, or tomorrow from day_switch (local time),
  // rolled forward to the next of the schedule's weekdays.
  _stripDate() {
    const [h, m] = String(this._config.regular_schedule?.day_switch ?? "18:30").split(":").map(Number);
    const p = Object.fromEntries(
      new Intl.DateTimeFormat("en-US", { timeZone: this._hass.config?.time_zone, hour: "numeric", minute: "numeric", hourCycle: "h23" })
        .formatToParts(new Date())
        .map((x) => [x.type, x.value]),
    );
    const today = this._today();
    let date = Number(p.hour) * 60 + Number(p.minute) >= h * 60 + (m || 0) ? addDays(today, 1) : today;
    const weekdays = new Set(this._config.regular_schedule?.weekdays || [1, 2, 3, 4, 5]);
    for (let i = 0; i < 7 && !weekdays.has(new Date(`${date}T12:00:00Z`).getUTCDay()); i++) date = addDays(date, 1);
    return date;
  }

  // The visible days ("YYYY-MM-DD"), starting today or at the week's start.
  _range() {
    const today = this._today();
    let start = today;
    if (this._config.first_day === "sunday" || this._config.first_day === "monday") {
      const dow = new Date(`${today}T12:00:00Z`).getUTCDay();
      start = addDays(today, -((dow - (this._config.first_day === "monday" ? 1 : 0) + 7) % 7));
    }
    start = addDays(start, this._offset * this._config.days);
    return Array.from({ length: this._config.days }, (_, i) => addDays(start, i));
  }

  _page(delta) {
    this._offset = delta === 0 ? 0 : this._offset + delta;
    this._fetch();
    clearTimeout(this._resetTimer);
    if (this._offset && this._config.reset_after > 0) {
      this._resetTimer = setTimeout(() => this._page(0), this._config.reset_after * 1000);
    }
    this._schedule();
  }

  // ---- data -------------------------------------------------------------

  _fetchSoon(ms) {
    clearTimeout(this._fetchTimer);
    this._fetchTimer = setTimeout(() => this._fetch(), ms);
  }

  async _fetch() {
    if (!this._hass) return;
    const days = this._range();
    const key = `${days[0]}|${days[days.length - 1]}`;
    this._fetchedRange = key;
    // Pad a day either side (in UTC) and bucket by HA's time zone client-side.
    // Always include today and tomorrow, for the strip's day mode.
    const today = this._today();
    const first = days[0] < today ? days[0] : today;
    const last = days[days.length - 1] > addDays(today, 1) ? days[days.length - 1] : addDays(today, 1);
    const start = `${addDays(first, -1)}T00:00:00Z`;
    const end = `${addDays(last, 2)}T00:00:00Z`;
    await Promise.all(
      this._calendars.map(async (cal) => {
        try {
          const evs = await this._hass.callApi(
            "GET",
            `calendars/${cal.entity}?start=${encodeURIComponent(start)}&end=${encodeURIComponent(end)}`,
          );
          this._events[cal.entity] = evs.map((e) => this._normalize(e, cal));
        } catch (err) {
          console.warn(`family-calendar-card: fetching ${cal.entity} failed`, err);
        }
      }),
    );
    if (!this._refreshTimer) {
      this._refreshTimer = setInterval(() => this._fetch(), this._config.refresh_interval * 1000);
    }
    this._schedule();
  }

  _normalize(e, cal) {
    const allDay = !!e.start?.date;
    const summary = e.summary || "(No title)";
    const ev = { summary, title: this._shorten(summary), location: e.location, description: e.description, cal, allDay };
    if (allDay) {
      ev.startDay = e.start.date;
      ev.endDay = addDays(e.end?.date || addDays(e.start.date, 1), -1); // inclusive
    } else {
      ev.start = Date.parse(e.start.dateTime);
      ev.end = Date.parse(e.end?.dateTime || e.start.dateTime);
      ev.startDay = this._wx.local(ev.start).date;
      ev.endDay = this._wx.local(Math.max(ev.start, ev.end - 1)).date;
    }
    // Duplicate detection across calendars: the title folded to lowercase
    // letters and digits (accents dropped), plus the exact span.
    const folded = summary
      .normalize("NFKD")
      .replace(/\p{M}/gu, "")
      .toLowerCase()
      .replace(/[^\p{L}\p{N}]+/gu, " ")
      .trim();
    ev.key = `${folded}|${allDay ? `${ev.startDay}/${ev.endDay}` : `${ev.start}/${ev.end}`}`;
    return ev;
  }

  // The grid's title: title_rewrites applied in order, whitespace tidied, and
  // the first letter capitalized (a rewrite may drop a leading word).
  _shorten(summary) {
    let t = summary;
    for (const { re, replace } of this._rewrites) t = t.replace(re, replace);
    t = t.replace(/\s{2,}/g, " ").trim();
    if (!t) return summary;
    return t[0].toUpperCase() + t.slice(1);
  }

  // Visible events split into bars (all-day or spanning days, packed into
  // lanes) and per-day timed lists.
  _layout(days) {
    const first = days[0];
    const last = days[days.length - 1];
    const idx = (d) => Math.round((Date.parse(`${d}T12:00:00Z`) - Date.parse(`${first}T12:00:00Z`)) / 864e5);
    const bars = [];
    const perDay = days.map(() => []);
    // Calendars in list order, so with dedupe the first shown calendar that
    // has an event keeps it and later copies are dropped.
    // dupes: kept event -> the calendars whose copies were dropped.
    const seen = new Map();
    const dupes = new Map();
    for (const cal of this._calendars) {
      if (this._hidden.has(cal.entity)) continue;
      for (const ev of this._events[cal.entity] || []) {
        if (ev.endDay < first || ev.startDay > last) continue;
        if (this._config.dedupe) {
          const kept = seen.get(ev.key);
          if (kept) {
            if (!dupes.has(kept)) dupes.set(kept, []);
            dupes.get(kept).push(ev.cal);
            continue;
          }
          seen.set(ev.key, ev);
        }
        if (ev.allDay || ev.startDay !== ev.endDay) {
          bars.push({ ev, from: Math.max(0, idx(ev.startDay)), to: Math.min(days.length - 1, idx(ev.endDay)) });
        } else {
          perDay[idx(ev.startDay)].push(ev);
        }
      }
    }
    bars.sort((a, b) => a.from - b.from || b.to - b.from - (a.to - a.from) || a.ev.summary.localeCompare(b.ev.summary));
    const laneEnds = [];
    for (const bar of bars) {
      let lane = laneEnds.findIndex((end) => end < bar.from);
      if (lane < 0) lane = laneEnds.push(-1) - 1;
      laneEnds[lane] = bar.to;
      bar.lane = lane;
    }
    for (const list of perDay) list.sort((a, b) => a.start - b.start || a.end - b.end);
    // Collapsed, the strip still works out its title but leaves the grid
    // whole (it gets copies of the day lists).
    const collapsed = this._config.regular_schedule && this._stripCollapsed();
    const regular = this._regular(days, collapsed ? perDay.map((l) => [...l]) : perDay, dupes);
    if (regular) regular.collapsed = collapsed;
    return { bars, perDay, lanes: laneEnds.length, regular };
  }

  // regular_schedule: the strip above the grid, a row per calendar on a
  // shared time axis (times are minutes after local midnight).
  //
  // The routine: timed events that recur on at least min_days of the visible
  // weekdays with the same calendar, title and start/end times. A duplicate
  // dropped from a later calendar still counts toward that calendar's routine
  // (dedupe works per occurrence, so on days the copies differ the later
  // calendar keeps its own).
  //
  // Regular mode shows the routine (each event's occurrence today, else the
  // next, else the first), with the visible weekdays where any of it is
  // missing at the row's end, and takes it out of the grid (perDay, in
  // place). Day mode shows the actual timed events of today (or tomorrow,
  // after day_switch) and takes them out of that day's column; a row whose
  // day is exactly its routine also takes the routine out of the whole week.
  //
  // When every row has a pickup at the same time it becomes `shared` (one
  // pin between the rows); otherwise each row's pickup is labelled with the
  // calendar's name.
  _regular(days, perDay, dupes) {
    const cfg = this._config.regular_schedule;
    if (!cfg) return null;
    const weekdays = new Set(cfg.weekdays || [1, 2, 3, 4, 5]);
    const minDays = cfg.min_days ?? 3;
    const pickupRe = new RegExp(cfg.pickup_match ?? "^pick-?up\\b", "i");
    const only = cfg.calendars ? new Set(cfg.calendars) : null;
    const mode = this._stripMode();
    const today = this._today();
    const now = Date.now();
    const hm = new Intl.DateTimeFormat("en-US", {
      timeZone: this._hass.config?.time_zone,
      hour: "numeric",
      minute: "numeric",
      hourCycle: "h23",
    });
    const tod = (ts) => {
      const p = Object.fromEntries(hm.formatToParts(new Date(ts)).map((x) => [x.type, x.value]));
      return Number(p.hour) * 60 + Number(p.minute);
    };
    const keyOf = (cal, ev) => `${cal.entity}|${ev.key.split("|")[0]}|${tod(ev.start)}|${ev.end - ev.start}`;
    const item = (ev) => {
      const start = tod(ev.start);
      return { ev, start, end: start + (ev.end - ev.start) / 60000, pickup: pickupRe.test(ev.summary) };
    };
    const dropFromGrid = (evs) => {
      const drop = new Set(evs);
      perDay.forEach((list, i) => {
        perDay[i] = list.filter((ev) => !drop.has(ev));
      });
    };

    // The routine, by calendar.
    const visible = new Set();
    const series = new Map();
    days.forEach((d, i) => {
      const dow = new Date(`${d}T12:00:00Z`).getUTCDay();
      if (!weekdays.has(dow)) return;
      visible.add(dow);
      for (const ev of perDay[i]) {
        for (const cal of [ev.cal, ...(dupes.get(ev) || [])]) {
          if (only && !only.has(cal.entity)) continue;
          const key = keyOf(cal, ev);
          if (!series.has(key)) series.set(key, { key, cal, evs: [], dows: new Set() });
          const s = series.get(key);
          if (s.dows.has(dow)) continue; // one per weekday (a longer view repeats them)
          s.dows.add(dow);
          s.evs.push(ev);
        }
      }
    });
    const routine = new Map();
    for (const s of series.values()) {
      if (s.dows.size < minDays) continue;
      if (!routine.has(s.cal.entity)) routine.set(s.cal.entity, []);
      routine.get(s.cal.entity).push(s);
    }
    const rowCals = this._calendars.filter(
      (c) => !this._hidden.has(c.entity) && (only ? only.has(c.entity) : routine.has(c.entity)),
    );

    let list;
    let title;
    if (mode === "day") {
      const date = this._stripDate();
      // The day's timed events of the row calendars, deduped in list order; a
      // dropped copy still appears in its own calendar's row.
      const seen = new Map();
      const byRow = new Map(rowCals.map((c) => [c.entity, []]));
      for (const cal of rowCals) {
        for (const ev of this._events[cal.entity] || []) {
          if (ev.allDay || ev.startDay !== date || ev.endDay !== date) continue;
          const kept = this._config.dedupe ? seen.get(ev.key) : null;
          if (kept) {
            byRow.get(cal.entity).push(kept);
            continue;
          }
          seen.set(ev.key, ev);
          byRow.get(cal.entity).push(ev);
        }
      }
      const shownKeys = new Set(seen.keys());
      const idx = days.indexOf(date);
      if (idx >= 0) perDay[idx] = perDay[idx].filter((ev) => !shownKeys.has(ev.key));
      // Rows whose day is exactly their routine take it out of the week; an
      // event shared with another row (a dropped duplicate) only goes when
      // that row matches too.
      const normal = new Set(
        rowCals
          .filter((cal) => {
            const ss = routine.get(cal.entity) || [];
            const dayKeys = new Set(byRow.get(cal.entity).map((ev) => keyOf(cal, ev)));
            return ss.length && dayKeys.size === ss.length && ss.every((s) => dayKeys.has(s.key));
          })
          .map((cal) => cal.entity),
      );
      const rowSet = new Set(rowCals.map((c) => c.entity));
      dropFromGrid(
        [...normal]
          .flatMap((e) => routine.get(e).flatMap((s) => s.evs))
          .filter((ev) => [ev.cal, ...(dupes.get(ev) || [])].every((c) => !rowSet.has(c.entity) || normal.has(c.entity))),
      );
      list = rowCals
        .map((cal) => ({ cal, exceptions: "", items: byRow.get(cal.entity).map(item) }))
        .filter((r) => r.items.length);
      const fmt = (opts) => new Intl.DateTimeFormat("en-US", { timeZone: "UTC", ...opts }).format(new Date(`${date}T12:00:00Z`));
      title =
        date === today
          ? `Today · ${fmt({ weekday: "short", month: "short", day: "numeric" })}`
          : date === addDays(today, 1)
            ? `Tomorrow · ${fmt({ weekday: "short", month: "short", day: "numeric" })}`
            : `${fmt({ weekday: "long" })} · ${fmt({ month: "short", day: "numeric" })}`;
    } else {
      dropFromGrid([...routine.values()].flat().flatMap((s) => s.evs));
      list = rowCals
        .filter((c) => routine.has(c.entity))
        .map((cal) => {
          const missing = new Set();
          const items = routine.get(cal.entity).map(({ evs, dows }) => {
            for (const d of visible) if (!dows.has(d)) missing.add(d);
            const pick = evs.find((ev) => ev.startDay === today) || evs.find((ev) => ev.start > now) || evs[0];
            return { ...item({ ...pick, regularDays: dayLabel(dows) }), dows };
          });
          return { cal, items, exceptions: missing.size ? dayLabel(missing) : "" };
        });
      title = `${cfg.title ?? "Regular schedule"}${visible.size ? ` · ${dayLabel(visible)}` : ""}`;
    }
    for (const r of list) r.items.sort((a, b) => a.start - b.start);

    // Pickups: one shared pin when they agree for every row, else named ones.
    const pickups = list.flatMap((r) => r.items.filter((it) => it.pickup).map((it) => ({ r, it })));
    const covered = new Set(pickups.map(({ r }) => r.cal.entity));
    let shared = null;
    if (pickups.length && new Set(pickups.map(({ it }) => it.start)).size === 1 && list.every((r) => covered.has(r.cal.entity))) {
      const ev = pickups[0].it.ev;
      const dows = pickups.flatMap(({ it }) => [...(it.dows || [])]);
      shared = { minutes: pickups[0].it.start, ev: dows.length ? { ...ev, regularDays: dayLabel(new Set(dows)) } : ev };
      for (const { r, it } of pickups) r.items = r.items.filter((x) => x !== it);
    } else {
      for (const { r, it } of pickups) {
        const name = r.cal.name || this._hass.states[r.cal.entity]?.attributes?.friendly_name || r.cal.entity;
        const t = it.ev.title;
        it.label = `${name} ${t[0].toLowerCase()}${t.slice(1)}`;
      }
      if (!pickups.length && cfg.pickup && mode !== "day" && list.length) {
        const [ph, pm] = String(cfg.pickup).split(":").map(Number);
        shared = { minutes: ph * 60 + (pm || 0), ev: null };
      }
    }
    if (!list.some((r) => r.items.length) && !shared) return { mode, title, rows: [] };
    // The axis: from the earliest start's hour to the hour of the last end
    // (a short event's start).
    const starts = list.flatMap((r) => r.items.map((it) => it.start));
    const tails = list.flatMap((r) => r.items.map((it) => (it.end - it.start < 30 ? it.start : it.end)));
    if (shared) {
      starts.push(shared.minutes);
      tails.push(shared.minutes);
    }
    const from = Math.floor(Math.min(...starts) / 60) * 60;
    const to = Math.max(from + 60, Math.ceil(Math.max(...tails) / 60) * 60);
    return { mode, title, rows: list, from, to, shared };
  }

  // ---- controls strip -----------------------------------------------------

  async _buildCards() {
    const configs = this._config.cards || [];
    const token = (this._cardsToken = {});
    if (!configs.length) {
      this._cards = [];
      return;
    }
    const helpers = await window.loadCardHelpers();
    if (token !== this._cardsToken) return;
    this._cards = configs.map((cfg) => {
      const el = helpers.createCardElement(cfg);
      if (this._hass) el.hass = this._hass;
      // Cards re-create themselves on a rebuild request (e.g. after a lazy load).
      el.addEventListener("ll-rebuild", (ev) => {
        ev.stopPropagation();
        const fresh = helpers.createCardElement(cfg);
        fresh.hass = this._hass;
        el.replaceWith(fresh);
        this._cards[this._cards.indexOf(el)] = fresh;
      });
      return el;
    });
    this._mountCards();
  }

  _mountCards() {
    const box = this.shadowRoot.querySelector(".controls");
    if (!box) return;
    box.replaceChildren(...this._cards);
  }

  // ---- rendering ----------------------------------------------------------

  _schedule() {
    if (this._raf) return;
    this._raf = requestAnimationFrame(() => {
      this._raf = null;
      this._render();
    });
  }

  // Redraw the clock on the minute (and roll the calendar over at midnight).
  _tick() {
    clearTimeout(this._tickTimer);
    const today = this._today?.() ?? null;
    if (this._lastToday && today && today !== this._lastToday) this._fetch();
    this._lastToday = today;
    this._schedule();
    this._tickTimer = setTimeout(() => this._tick(), 60000 - (Date.now() % 60000) + 50);
  }

  _build() {
    const c = this._config;
    this.shadowRoot.innerHTML = `
      <style>${STYLE}</style>
      <div class="screen" lang="en" style="--controls-columns: ${Number(c.controls_columns) || 3}; --days: ${Number(c.days) || 7}">
        <header class="top">
          <div class="clock"><div class="time"></div><div class="date"></div></div>
          <div class="weather"></div>
        </header>
        <section class="calendar">
          <div class="bar">
            <div class="range"></div>
            <div class="nav">
              <button class="today-btn" data-page="0">Today</button>
              <button class="arrow" data-page="-1" aria-label="Previous"><ha-icon icon="mdi:chevron-left"></ha-icon></button>
              <button class="arrow" data-page="1" aria-label="Next"><ha-icon icon="mdi:chevron-right"></ha-icon></button>
            </div>
          </div>
          <div class="chips"></div>
          <div class="regular"></div>
          <div class="heads"></div>
          <div class="week"></div>
        </section>
        <section class="controls"></section>
        <footer class="banner" hidden></footer>
        <div class="detail" hidden></div>
      </div>`;
    const root = this.shadowRoot;
    root.querySelector(".nav").addEventListener("click", (e) => {
      const b = e.target.closest("[data-page]");
      if (b) this._page(Number(b.dataset.page));
    });
    root.querySelector(".chips").addEventListener("click", (e) => {
      const chip = e.target.closest("[data-entity]");
      if (!chip) return;
      const ent = chip.dataset.entity;
      if (this._hidden.has(ent)) this._hidden.delete(ent);
      else this._hidden.add(ent);
      this._saveHidden();
      this._schedule();
    });
    for (const sel of [".week", ".regular"]) {
      root.querySelector(sel).addEventListener("click", (e) => {
        const mode = e.target.closest("[data-mode]");
        if (mode) return this._setStripMode(mode.dataset.mode);
        if (e.target.closest("[data-collapse]")) return this._toggleStripCollapsed();
        const el = e.target.closest("[data-ev]");
        if (el) this._showDetail(this._shown[Number(el.dataset.ev)]);
      });
    }
    root.querySelector(".detail").addEventListener("click", (e) => {
      if (e.target.closest(".close") || !e.target.closest(".sheet")) this._hideDetail();
    });
    this._built = true;
    this._bannerHtml = null;
    this._mountCards();
    if (!this._onResize) {
      this._onResize = () => {
        this._pinsDirty = true;
        this._schedule();
      };
      window.addEventListener("resize", this._onResize);
    }
  }

  // Fill the window below the card's top edge: that's the whole screen on the
  // kiosk (Kiosk Satellite hides HA's toolbar), and still fits in a browser
  // with the toolbar showing, or in Puppet (which renders the toolbar and
  // crops it off).
  _fitHeight() {
    const screen = this.shadowRoot.querySelector(".screen");
    const h =
      this._config.height === "auto"
        ? `${Math.max(320, window.innerHeight - Math.max(0, this.getBoundingClientRect().top))}px`
        : this._config.height;
    if (screen.style.height !== h) screen.style.height = h;
  }

  // Assign innerHTML only when it changed, so scroll positions and tap
  // targets survive the frequent hass updates.
  _set(sel, html) {
    if (this._html[sel] === html) return;
    this._html[sel] = html;
    this.shadowRoot.querySelector(sel).innerHTML = html;
  }

  _render() {
    if (!this._hass || !this._config) return;
    if (!this._built) this._build();
    this._fitHeight();
    this._wx.invalidate();
    const days = this._range();
    if (this._fetchedRange !== `${days[0]}|${days[days.length - 1]}`) this._fetch();
    this._renderClock();
    this._renderWeather();
    this._renderCalendar(days);
    this._renderBanner();
  }

  _renderClock() {
    const tz = this._hass.config?.time_zone;
    const now = new Date();
    const parts = new Intl.DateTimeFormat("en-US", { timeZone: tz, hour: "numeric", minute: "2-digit" }).formatToParts(now);
    const hm = parts
      .filter((p) => p.type === "hour" || p.type === "minute" || (p.type === "literal" && p.value === ":"))
      .map((p) => p.value)
      .join("");
    const ampm = parts.find((p) => p.type === "dayPeriod")?.value || "";
    this._set(".time", `${esc(hm)}<span class="ampm">${esc(ampm)}</span>`);
    this._set(
      ".date",
      esc(new Intl.DateTimeFormat("en-US", { timeZone: tz, weekday: "long", month: "long", day: "numeric" }).format(now)),
    );
  }

  _renderWeather() {
    const c = this._config;
    const weather = this._wx.src().state;
    const wa = weather?.attributes || {};
    const sun = this._hass.states[c.sun_entity];
    const night = sun ? sun.state === "below_horizon" : false;
    const inside = c.inside_entity && this._hass.states[c.inside_entity];
    const ic = weather ? this._wx.conditionIcon(weather.state, night && weather.state === "partlycloudy") : "alert-circle-outline";
    const rows = this._wx.src().hourly.length ? [this._wx.outlookRow("today"), this._wx.outlookRow("tomorrow")] : [];
    this._set(
      ".weather",
      `
      <div class="now">
        <ha-icon icon="mdi:${ic}"></ha-icon>
        <span class="now-temp">${temp(wa.temperature)}</span>
        <span class="now-cond">${esc(CONDITION_TEXT[weather?.state] || (weather?.state === "clear-night" ? "Clear" : weather?.state) || "Unavailable")}</span>
        ${inside && num(inside.state) !== null ? `<span class="inside"><ha-icon icon="mdi:home-thermometer-outline"></ha-icon>${temp(inside.state)}</span>` : ""}
      </div>
      ${rows
        .map(
          (o) => `
        <div class="orow">
          <ha-icon icon="mdi:${o.icon}"></ha-icon>
          <div class="olabel"><b>${esc(o.label)}</b> <span class="rng">${esc(o.range)}</span></div>
          <div class="otext">${esc(o.text)}${
            o.wear ? `<span class="wear"><ha-icon icon="mdi:${o.wear.icon}"></ha-icon>${esc(o.wear.text)}</span>` : ""
          }</div>
        </div>`,
        )
        .join("")}`,
    );
  }

  _renderCalendar(days) {
    const today = this._today();
    const now = Date.now();
    const fmt = (d, opts) => new Intl.DateTimeFormat("en-US", { timeZone: "UTC", ...opts }).format(new Date(`${d}T12:00:00Z`));
    const first = days[0];
    const last = days[days.length - 1];
    const sameMonth = first.slice(0, 7) === last.slice(0, 7);
    this._set(
      ".range",
      `${esc(fmt(first, { month: "long" }))} ${esc(fmt(first, { day: "numeric" }))} – ${
        sameMonth ? "" : `${esc(fmt(last, { month: "short" }))} `
      }${esc(fmt(last, { day: "numeric" }))}`,
    );
    this.shadowRoot.querySelector(".today-btn").toggleAttribute("disabled", this._offset === 0);

    this._set(
      ".chips",
      this._calendars
        .map((cal) => {
          const off = this._hidden.has(cal.entity);
          const name = cal.name || this._hass.states[cal.entity]?.attributes?.friendly_name || cal.entity;
          return `<button class="chip${off ? " off" : ""}" data-entity="${esc(cal.entity)}" style="--c: ${esc(cal.color)}" aria-pressed="${!off}">
            <span class="dot"></span>${esc(name)}</button>`;
        })
        .join(""),
    );

    this._set(
      ".heads",
      days
        .map(
          (d) => `<div class="head${d === today ? " is-today" : ""}${d < today ? " past" : ""}">
            <span class="dow">${esc(fmt(d, { weekday: "short" }))}</span><span class="dnum">${esc(fmt(d, { day: "numeric" }))}</span></div>`,
        )
        .join(""),
    );

    const { bars, perDay, lanes, regular } = this._layout(days);
    const shown = [];
    const ref = (ev) => shown.push(ev) - 1;
    const time = (ts) => this._wx.clockTime(ts).replace(/\s/g, " ");
    const barHtml = bars
      .map(({ ev, from, to, lane }) => {
        const clipL = ev.startDay < first;
        const clipR = ev.endDay > last;
        const when = !ev.allDay && !clipL ? `${time(ev.start)} ` : "";
        const done = ev.allDay ? ev.endDay < today : ev.end <= now;
        return `<button class="ev-bar${clipL ? " clip-l" : ""}${clipR ? " clip-r" : ""}${done ? " done" : ""}" data-ev="${ref(ev)}"
          style="grid-column: ${from + 1} / ${to + 2}; grid-row: ${lane + 1}; --c: ${esc(ev.cal.color)}; --ink: ${ink(ev.cal.color)}">
          <span class="bt">${when ? `<span class="t">${esc(when)}</span>` : ""}${esc(ev.title)}</span></button>`;
      })
      .join("");
    const cols = perDay
      .map(
        (list, i) => `<div class="col${days[i] === today ? " is-today" : ""}" style="grid-column: ${i + 1}">${list
          .map(
            (ev) => `<button class="ev${ev.end <= now ? " done" : ""}${ev.start <= now && now < ev.end ? " live" : ""}"
              data-ev="${ref(ev)}" style="--c: ${esc(ev.cal.color)}">
              <span class="t">${esc(time(ev.start))}</span><span class="s">${esc(ev.title)}</span></button>`,
          )
          .join("")}</div>`,
      )
      .join("");
    const regularHtml = regular ? this._regularHtml(regular, ref) : "";
    this._shown = shown;
    if (this._html[".regular"] !== regularHtml) {
      this._set(".regular", regularHtml);
      this._pinsDirty = true;
    }
    if (this._pinsDirty) this._layoutPins();
    this._set(
      ".week",
      `${lanes ? `<div class="bars" style="grid-template-rows: repeat(${lanes}, auto)">${barHtml}</div>` : ""}<div class="cols">${cols}</div>`,
    );
  }

  // The notification bar: the E1001 banner's items, or nothing (and no bar).
  // If they overflow, the open doors fold into one summary item. It's plain
  // page content, so it never wakes the tablet from its screensaver.
  _renderBanner() {
    const bar = this.shadowRoot.querySelector(".banner");
    const html = (items) =>
      items
        .map(
          (it) =>
            `<span class="item${it.shrink ? " shrink" : ""}"><ha-icon icon="mdi:${esc(it.icon)}"></ha-icon><span>${esc(it.text)}</span></span>`,
        )
        .join("");
    const items = this._config.banner ? this._bn.items() : [];
    const full = html(items);
    if (full === this._bannerHtml) return;
    this._bannerHtml = full;
    // Show it first, so the overflow check measures the laid-out bar.
    bar.hidden = !items.length;
    this.shadowRoot.querySelector(".screen").classList.toggle("has-banner", items.length > 0);
    bar.innerHTML = full;
    if (items.length && bar.scrollWidth > bar.clientWidth + 1 && this._bn.openDoors().length > 1) {
      bar.innerHTML = html(this._bn.items(true));
    }
  }

  // The regular-schedule strip: an hour axis ending at pickup, then a row per
  // calendar. Events of 30+ minutes are blocks sized by their times (a block
  // starts no earlier than the previous one ends, so overlaps don't stack),
  // clipped at pickup; shorter ones (a bus stop) are pins under the track.
  _regularHtml(rg, ref) {
    const toggle = `<div class="rg-toggle" role="group" aria-label="Schedule">${[
      ["regular", "mdi:calendar-sync-outline", "Regular schedule"],
      ["day", "mdi:calendar-today-outline", "Today's schedule"],
    ]
      .map(
        ([m, ic, label]) =>
          `<button data-mode="${m}" class="${rg.mode === m ? "on" : ""}" aria-label="${label}" aria-pressed="${rg.mode === m}"><ha-icon icon="${ic}"></ha-icon></button>`,
      )
      .join("")}</div>`;
    const chevron = `<button class="rg-collapse" data-collapse aria-label="${rg.collapsed ? "Show" : "Hide"} the schedule" aria-expanded="${!rg.collapsed}"><ha-icon icon="mdi:chevron-${rg.collapsed ? "down" : "up"}"></ha-icon></button>`;
    const head = `<div class="rg-head"><div class="rg-title">${esc(rg.title)}</div><div class="rg-tools">${rg.collapsed ? "" : toggle}${chevron}</div></div>`;
    if (rg.collapsed) return `<div class="rg-wrap">${head}</div>`;
    if (!rg.rows.length) {
      return `<div class="rg-wrap">${head}<div class="rg-none">${rg.mode === "day" ? "Nothing scheduled" : "No regular schedule this week"}</div></div>`;
    }
    const span = rg.to - rg.from;
    const pct = (m) => (((Math.min(Math.max(m, rg.from), rg.to) - rg.from) / span) * 100).toFixed(2);
    const clock = (m) => {
      const h = Math.floor(m / 60) % 24;
      const mm = String(m % 60).padStart(2, "0");
      return `${h % 12 || 12}:${mm}`;
    };
    const ticks = [];
    for (let m = rg.from; m <= rg.to; m += 60) {
      const h = (m / 60) % 24;
      const label = m === rg.from || h === 12 ? `${h % 12 || 12} ${h < 12 ? "AM" : "PM"}` : `${h % 12 || 12}`;
      ticks.push(`<span class="tick" style="left: ${pct(m)}%">${esc(label)}</span>`);
    }
    // A shared pickup: a pin like the bus stops under the first row (so
    // between the rows when there are two); at the axis end its label sits
    // left of the end line.
    const sh = rg.shared;
    const atEnd = sh && sh.minutes === rg.to;
    const pickup = !sh
      ? ""
      : sh.ev
        ? `<button class="rg-pin flip pickup" data-ev="${ref(sh.ev)}" style="left: ${pct(sh.minutes)}%"><span class="t">${esc(clock(sh.minutes))}</span>${esc(sh.ev.title)}</button>`
        : `<span class="rg-pin flip pickup" style="left: ${pct(sh.minutes)}%"><span class="t">${esc(clock(sh.minutes))}</span>Pickup</span>`;
    const rows = rg.rows
      .map((row, r) => {
        let edge = rg.from;
        const blocks = [];
        const pins = [];
        for (const it of row.items) {
          if (it.end - it.start < 30) {
            pins.push(it);
            continue;
          }
          const a = Math.max(it.start, edge);
          const b = Math.min(it.end, rg.to);
          if (b - a < 5) continue;
          edge = b;
          blocks.push(`<button class="rg-block" data-ev="${ref(it.ev)}" style="left: ${pct(a)}%; width: calc(${pct(b)}% - ${pct(a)}%); --c: ${esc(row.cal.color)}">
            <span class="s">${esc(it.ev.title)}</span><span class="t">${esc(clock(it.start))}</span></button>`);
        }
        // Pins sit on lines under the blocks; _layoutPins() moves any that
        // collide onto further lines once their labels can be measured.
        const pinHtml =
          pins
            .map((it) => {
              const left = Number(pct(it.start));
              return `<button class="rg-pin${left > 70 ? " flip" : ""}" data-ev="${ref(it.ev)}" style="left: ${left}%; --c: ${esc(row.cal.color)}">
              <span class="t">${esc(clock(it.start))}</span>${esc(it.label ?? it.ev.title)}</button>`;
            })
            .join("") + (r === 0 ? pickup : "");
        const name = row.cal.name || this._hass.states[row.cal.entity]?.attributes?.friendly_name || row.cal.entity;
        return `<div class="rg-row">
          <div class="rg-name" style="--c: ${esc(row.cal.color)}"><span class="dot"></span>${esc(name)}</div>
          <div class="rg-track${atEnd ? " to-pickup" : ""}">${blocks.join("")}${pinHtml}</div>
          <div class="rg-exc">${row.exceptions ? `<span>except <b>${esc(row.exceptions)}</b></span>` : ""}</div>
        </div>`;
      })
      .join("");
    return `<div class="rg-wrap" style="--rg-hours: ${span / 60}">
      ${head}
      <div class="rg-row rg-axis"><div></div><div class="rg-ticks">${ticks.join("")}</div><div></div></div>
      ${rows}</div>`;
  }

  // Lay out each track's pins: greedily onto the first line (under the
  // blocks) where the label doesn't overlap another, then size the track.
  _layoutPins() {
    for (const track of this.shadowRoot.querySelectorAll(".rg-track")) {
      const pins = [...track.querySelectorAll(".rg-pin")]
        .map((el) => ({ el, rect: el.getBoundingClientRect() }))
        .sort((a, b) => a.rect.left - b.rect.left);
      const lineEnds = [];
      for (const { el, rect } of pins) {
        let line = lineEnds.findIndex((end) => rect.left >= end + 8);
        if (line < 0) line = lineEnds.push(0) - 1;
        lineEnds[line] = rect.right;
        el.style.top = `${30 + line * 20}px`;
      }
      track.style.height = `${30 + lineEnds.length * 20}px`;
    }
    this._pinsDirty = false;
  }

  _showDetail(ev) {
    if (!ev) return;
    const tz = this._hass.config?.time_zone;
    const dayFmt = (d) =>
      new Intl.DateTimeFormat("en-US", { timeZone: "UTC", weekday: "long", month: "long", day: "numeric" }).format(
        new Date(`${d}T12:00:00Z`),
      );
    let when;
    if (ev.allDay) {
      when = ev.startDay === ev.endDay ? `${dayFmt(ev.startDay)} · all day` : `${dayFmt(ev.startDay)} – ${dayFmt(ev.endDay)}`;
    } else if (ev.startDay === ev.endDay) {
      when = `${dayFmt(ev.startDay)} · ${this._wx.clockTime(ev.start)} – ${this._wx.clockTime(ev.end)}`;
    } else {
      const f = (ts) =>
        new Intl.DateTimeFormat("en-US", { timeZone: tz, weekday: "short", month: "short", day: "numeric", hour: "numeric", minute: "2-digit" }).format(
          new Date(ts),
        );
      when = `${f(ev.start)} – ${f(ev.end)}`;
    }
    const name = ev.cal.name || this._hass.states[ev.cal.entity]?.attributes?.friendly_name || ev.cal.entity;
    const desc = plain(ev.description);
    const box = this.shadowRoot.querySelector(".detail");
    box.innerHTML = `
      <div class="sheet" style="--c: ${esc(ev.cal.color)}">
        <button class="close" aria-label="Close"><ha-icon icon="mdi:close"></ha-icon></button>
        <div class="d-cal"><span class="dot"></span>${esc(name)}</div>
        <h2>${esc(ev.summary)}</h2>
        <div class="d-row"><ha-icon icon="mdi:clock-outline"></ha-icon><span>${esc(when)}</span></div>
        ${ev.regularDays ? `<div class="d-row"><ha-icon icon="mdi:calendar-sync-outline"></ha-icon><span>Regular schedule this week: ${esc(ev.regularDays)}</span></div>` : ""}
        ${ev.location ? `<div class="d-row"><ha-icon icon="mdi:map-marker-outline"></ha-icon><span>${esc(ev.location)}</span></div>` : ""}
        ${desc ? `<div class="d-desc">${esc(desc)}</div>` : ""}
      </div>`;
    box.hidden = false;
    clearTimeout(this._detailTimer);
    this._detailTimer = setTimeout(() => this._hideDetail(), 60000);
  }

  _hideDetail() {
    clearTimeout(this._detailTimer);
    const box = this.shadowRoot.querySelector(".detail");
    box.hidden = true;
    box.innerHTML = "";
  }
}

const STYLE = `
  :host { display: block; }
  button { font: inherit; color: inherit; background: none; border: 0; padding: 0; margin: 0; cursor: pointer;
    -webkit-tap-highlight-color: transparent; text-align: left; }
  ha-icon { flex: none; }
  .screen {
    box-sizing: border-box; overflow: hidden; position: relative;
    display: grid; gap: 12px; padding: 12px;
    grid-template-columns: minmax(0, 1fr);
    grid-template-rows: auto minmax(0, 1fr) auto;
    grid-template-areas: "top" "calendar" "controls";
    color: var(--primary-text-color); font-family: var(--ha-font-family-body, Roboto, sans-serif);
    font-variant-numeric: tabular-nums;
  }
  /* With notices, the bar takes a last row and sits flush with the bottom. */
  .screen.has-banner {
    padding-bottom: 0;
    grid-template-rows: auto minmax(0, 1fr) auto auto;
    grid-template-areas: "top" "calendar" "controls" "banner";
  }
  /* Landscape: calendar on the left, clock/weather/controls in a side column. */
  @media (orientation: landscape) {
    .screen {
      grid-template-columns: minmax(0, 1fr) clamp(300px, 30%, 420px);
      grid-template-rows: auto minmax(0, 1fr);
      grid-template-areas: "calendar top" "calendar controls";
      --controls-columns: 2 !important;
    }
    .screen.has-banner {
      grid-template-rows: auto minmax(0, 1fr) auto;
      grid-template-areas: "calendar top" "calendar controls" "banner banner";
    }
    .top { flex-direction: column; align-items: stretch !important; }
    .controls { align-content: start; overflow-y: auto; }
  }

  /* ---- clock + weather ---- */
  .top { grid-area: top; display: flex; align-items: center; gap: 24px; padding: 0 4px; }
  .clock { flex: none; }
  .time { font-size: 96px; font-weight: 500; line-height: 0.95; letter-spacing: -0.03em; }
  .ampm { font-size: 30px; font-weight: 500; letter-spacing: 0; margin-left: 6px; color: var(--secondary-text-color); }
  .date { font-size: 24px; font-weight: 500; margin-top: 6px; color: var(--secondary-text-color); }
  .weather { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 8px; }
  .now { display: flex; align-items: center; gap: 8px; --mdc-icon-size: 40px; }
  .now-temp { font-size: 38px; font-weight: 600; }
  .now-cond { font-size: 18px; color: var(--secondary-text-color); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .inside { display: inline-flex; align-items: center; gap: 2px; margin-left: auto; font-size: 22px; font-weight: 500;
    color: var(--secondary-text-color); --mdc-icon-size: 24px; }
  .orow { display: grid; grid-template-columns: 34px minmax(0, 1fr); column-gap: 10px; align-items: center; --mdc-icon-size: 32px; }
  .orow ha-icon { grid-row: span 2; color: var(--state-icon-color, var(--primary-color)); }
  .olabel { font-size: 18px; }
  .olabel .rng { font-weight: 600; margin-left: 4px; }
  .otext { font-size: 16px; color: var(--secondary-text-color); display: flex; flex-wrap: wrap; column-gap: 12px; }
  .wear { display: inline-flex; align-items: center; gap: 4px; --mdc-icon-size: 18px; }
  .wear ha-icon { grid-row: auto; color: inherit; }

  /* ---- calendar ---- */
  .calendar { grid-area: calendar; min-height: 0; display: flex; flex-direction: column;
    background: var(--ha-card-background, var(--card-background-color));
    border-radius: var(--ha-card-border-radius, 12px);
    border: var(--ha-card-border-width, 1px) solid var(--ha-card-border-color, var(--divider-color));
    overflow: hidden; }
  .bar { display: flex; align-items: center; padding: 12px 12px 4px 16px; gap: 8px; }
  .range { flex: 1; font-size: 24px; font-weight: 600; }
  .nav { display: flex; align-items: center; gap: 4px; }
  .today-btn { font-size: 16px; font-weight: 500; padding: 8px 16px; border-radius: 20px;
    border: 1px solid var(--divider-color); }
  .today-btn[disabled] { opacity: 0.4; cursor: default; }
  .arrow { width: 44px; height: 44px; border-radius: 50%; display: grid; place-items: center; --mdc-icon-size: 30px; }
  .arrow:active, .today-btn:active, .chip:active { background: var(--secondary-background-color); }
  .chips { display: flex; flex-wrap: wrap; gap: 8px; padding: 6px 12px 10px; flex: none; }
  .chip { flex: none; display: inline-flex; align-items: center; gap: 8px; height: 38px; padding: 0 14px 0 10px;
    border-radius: 19px; font-size: 16px; font-weight: 500; white-space: nowrap;
    background: color-mix(in srgb, var(--c) 16%, transparent); border: 1.5px solid var(--c); }
  .chip .dot, .d-cal .dot { width: 14px; height: 14px; border-radius: 50%; background: var(--c); box-sizing: border-box; }
  .chip.off { background: none; border-color: var(--divider-color); color: var(--secondary-text-color); }
  .chip.off .dot { background: none; border: 2px solid var(--c); }

  /* Regular schedule: an hour axis, then a row per calendar (name | time
     track | exception days). */
  .regular { flex: none; padding: 0 12px 10px; }
  .rg-wrap { display: flex; flex-direction: column; gap: 4px; }
  .regular:empty { display: none; }
  .rg-head { display: flex; align-items: center; justify-content: space-between; gap: 8px; }
  .rg-title { font-size: 13px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.06em;
    color: var(--secondary-text-color); }
  /* Mode toggle: two small glyphs, the active one filled. */
  .rg-toggle { display: flex; gap: 2px; padding: 2px; border-radius: 16px; border: 1px solid var(--divider-color); }
  .rg-toggle button { width: 34px; height: 26px; border-radius: 13px; display: grid; place-items: center;
    --mdc-icon-size: 18px; color: var(--secondary-text-color); }
  .rg-toggle button.on { background: var(--primary-color); color: var(--text-primary-color, #fff); }
  .rg-tools { display: flex; align-items: center; gap: 6px; }
  .rg-collapse { width: 32px; height: 32px; border-radius: 16px; display: grid; place-items: center;
    border: 1px solid var(--divider-color); --mdc-icon-size: 20px; color: var(--secondary-text-color); }
  .rg-none { font-size: 15px; color: var(--secondary-text-color); padding: 4px 0 2px; }
  .rg-row { display: grid; grid-template-columns: 84px minmax(0, 1fr) 84px; column-gap: 8px; align-items: start; }
  .rg-ticks { position: relative; height: 16px; }
  .tick { position: absolute; transform: translateX(-50%); font-size: 11px; color: var(--secondary-text-color);
    white-space: nowrap; }
  .tick:first-child { transform: none; }
  .rg-name { display: flex; align-items: center; gap: 6px; height: 30px; font-size: 15px; font-weight: 600;
    white-space: nowrap; overflow: hidden; }
  .rg-name .dot { width: 10px; height: 10px; border-radius: 50%; background: var(--c); flex: none; }
  .rg-track { position: relative; height: 30px;
    background: repeating-linear-gradient(90deg, var(--divider-color) 0 1px, transparent 1px calc(100% / var(--rg-hours, 11))); }
  .rg-track.to-pickup { border-right: 2px solid var(--primary-text-color); }
  .rg-block { position: absolute; top: 0; height: 30px; box-sizing: border-box; padding: 0 6px; border-radius: 6px;
    display: flex; align-items: center; gap: 6px; overflow: hidden; white-space: nowrap;
    background: color-mix(in srgb, var(--c) 30%, var(--card-background-color)); border-left: 3px solid var(--c);
    box-shadow: 0 0 0 1px var(--card-background-color); }
  .rg-block .s { font-size: 14px; font-weight: 500; overflow: hidden; text-overflow: ellipsis; }
  .rg-block .t { font-size: 11px; font-weight: 600; color: var(--secondary-text-color); }
  .rg-pin { position: absolute; top: 30px; display: inline-flex; align-items: center; gap: 4px; font-size: 12px; white-space: nowrap;
    padding-left: 6px; border-left: 2px solid var(--c); height: 18px; }
  .rg-pin.flip { transform: translateX(-100%); padding: 0 6px 0 0; border-left: 0; border-right: 2px solid var(--c); }
  .rg-pin .t { font-weight: 600; color: var(--secondary-text-color); }
  /* Ends flush with the axis-end line, which carries on as its tick. */
  .rg-pin.pickup { --c: var(--primary-text-color); font-weight: 600; margin-left: 2px; }
  .rg-exc { height: 30px; display: flex; align-items: center; font-size: 13px; line-height: 1.15;
    color: var(--secondary-text-color); }
  .rg-exc b { color: var(--primary-text-color); margin-left: 3px; white-space: nowrap; }

  .heads, .bars, .cols { display: grid; grid-template-columns: repeat(var(--days), minmax(0, 1fr)); }
  .heads { border-bottom: 1px solid var(--divider-color); padding: 0 2px; }
  .head { display: flex; flex-direction: column; align-items: center; padding: 4px 0 8px; }
  .head .dow { font-size: 14px; font-weight: 500; text-transform: uppercase; letter-spacing: 0.04em; color: var(--secondary-text-color); }
  .head .dnum { font-size: 24px; font-weight: 500; width: 40px; height: 40px; line-height: 40px; text-align: center; border-radius: 50%; }
  .head.is-today .dow { color: var(--primary-color); }
  .head.is-today .dnum { background: var(--primary-color); color: var(--text-primary-color, #fff); }
  .head.past { opacity: 0.6; }
  .week { flex: 1; min-height: 0; overflow-y: auto; scrollbar-width: none; display: flex; flex-direction: column; padding: 0 2px; }
  .week::-webkit-scrollbar { display: none; }
  .bars { gap: 3px 0; padding: 6px 0 4px; border-bottom: 1px solid var(--divider-color); }
  .ev-bar { margin: 0 2px; padding: 3px 6px; border-radius: 6px; font-size: 14px; font-weight: 500; line-height: 1.25;
    background: var(--c); color: var(--ink); min-width: 0; }
  .ev-bar .bt { overflow: hidden; overflow-wrap: break-word; hyphens: auto;
    display: -webkit-box; -webkit-box-orient: vertical; -webkit-line-clamp: 2; }
  .ev-bar .t { font-weight: 400; opacity: 0.85; }
  .ev-bar.clip-l { border-top-left-radius: 0; border-bottom-left-radius: 0; margin-left: 0; }
  .ev-bar.clip-r { border-top-right-radius: 0; border-bottom-right-radius: 0; margin-right: 0; }
  .cols { flex: 1; align-items: stretch; }
  .col { display: flex; flex-direction: column; gap: 4px; padding: 6px 2px 12px; min-height: 100%; box-sizing: border-box; }
  .col + .col { border-left: 1px solid var(--divider-color); }
  .col.is-today { background: color-mix(in srgb, var(--primary-color) 6%, transparent); }
  .ev { display: flex; flex-direction: column; gap: 1px; padding: 4px 3px 4px 4px; border-radius: 6px; min-width: 0;
    background: color-mix(in srgb, var(--c) 18%, transparent); border-left: 3px solid var(--c); }
  .ev .t { font-size: 12px; font-weight: 600; color: var(--secondary-text-color); white-space: nowrap; }
  .ev .s { font-size: 14px; line-height: 1.25; overflow-wrap: break-word; hyphens: auto;
    display: -webkit-box; -webkit-box-orient: vertical; -webkit-line-clamp: 4; overflow: hidden; }
  .ev.live { box-shadow: inset 0 0 0 2px var(--c); }
  .done { opacity: 0.45; }

  /* ---- controls ---- */
  .controls { grid-area: controls; display: grid; gap: 8px; grid-template-columns: repeat(var(--controls-columns), minmax(0, 1fr)); }

  /* ---- notification bar ----
     Full-bleed along the bottom edge, inverted against the page (dark on a
     light theme, light on a dark one) so it stands out, like the E1001's
     black banner. */
  .banner { grid-area: banner; margin: 0 -12px; min-height: 56px; box-sizing: border-box;
    display: flex; align-items: center; gap: 28px; padding: 8px 20px; overflow: hidden;
    background: var(--primary-text-color); color: var(--card-background-color, #fff);
    --mdc-icon-size: 28px; }
  .banner[hidden] { display: none; }
  .banner .item { display: inline-flex; align-items: center; gap: 8px; flex: none;
    font-size: 21px; font-weight: 500; white-space: nowrap; }
  .banner .item.shrink { flex: 0 1 auto; min-width: 0; }
  .banner .item > span { overflow: hidden; text-overflow: ellipsis; }

  /* ---- event detail ---- */
  .detail { position: absolute; inset: 0; z-index: 5; display: grid; place-items: center; padding: 24px;
    background: rgba(0, 0, 0, 0.45); }
  .detail[hidden] { display: none; }
  .sheet { position: relative; width: min(560px, 100%); max-height: 80%; overflow-y: auto; box-sizing: border-box;
    padding: 24px; border-radius: 16px; background: var(--card-background-color); border-top: 8px solid var(--c);
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3); }
  .close { position: absolute; top: 12px; right: 12px; width: 44px; height: 44px; border-radius: 50%;
    display: grid; place-items: center; }
  .d-cal { display: flex; align-items: center; gap: 8px; font-size: 15px; color: var(--secondary-text-color); }
  .sheet h2 { margin: 8px 48px 16px 0; font-size: 26px; font-weight: 600; line-height: 1.2; }
  .d-row { display: flex; gap: 12px; align-items: flex-start; font-size: 18px; margin-bottom: 10px; --mdc-icon-size: 22px; }
  .d-row ha-icon { color: var(--secondary-text-color); margin-top: 1px; }
  .d-desc { margin-top: 12px; font-size: 16px; line-height: 1.45; white-space: pre-line; color: var(--secondary-text-color);
    overflow-wrap: anywhere; }
`;

customElements.define("family-calendar-card", FamilyCalendarCard);

window.customCards = window.customCards || [];
window.customCards.push({
  type: "family-calendar-card",
  name: "Family calendar",
  description: "Full-screen touchscreen family calendar: clock, weather outlook, toggleable calendars, week grid and controls.",
});
