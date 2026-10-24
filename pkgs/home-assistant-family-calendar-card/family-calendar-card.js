// family-calendar-card — a full-screen Lovelace card for a touchscreen family
// calendar (the kitchen's Kiosk Satellite tablet), meant as the only card of a
// panel view. It fills the screen (portrait first; landscape moves the clock,
// weather and controls into a side column) with:
//
//   - a clock and date,
//   - current conditions and today/tomorrow outlook lines, worded exactly as
//     on the E1001 e-ink panel (shared ../lovelace-weather/weather.js),
//   - chips that show/hide each configured calendar (remembered per device),
//   - a multi-day week grid: all-day and multi-day events as bars across the
//     days, timed events listed under each day; tap an event for details,
//     page with the arrows (it returns to the current week when left idle),
//   - a strip of ordinary Lovelace cards (`cards`) for room controls.
//
// Config:
//   type: custom:family-calendar-card
//   calendars:                         # chips, in this order
//     - entity: calendar.family
//       name: Family                   # default: friendly name
//       color: "#039be5"               # default: from a built-in palette
//       hidden: false                  # start with the chip off (until tapped)
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
//   height: auto                       # card height (CSS); auto fills the window
//                                      # below the card's top edge
//
// Events come from HA's calendar REST API (GET /api/calendars/<entity>) for
// every configured calendar, hidden or not, so toggling a chip is instant.

const { Weather, CONDITION_TEXT, num, temp } = await import(
  new URL(`./weather.js${new URL(import.meta.url).search}`, import.meta.url).href
);

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
    this._wx.configure(this._config);
    this._built = false;
    this._html = {};
    this._fetchedRange = null;
    this._buildCards();
    this._schedule();
  }

  set hass(hass) {
    this._wx.setHass(hass);
    this._hass = hass;
    for (const card of this._cards) card.hass = hass;
    if (this.isConnected) this._wx.ensureSubscriptions();
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
    if (this._hass) this._wx.ensureSubscriptions();
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
    const start = `${addDays(days[0], -1)}T00:00:00Z`;
    const end = `${addDays(days[days.length - 1], 2)}T00:00:00Z`;
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
    for (const cal of this._calendars) {
      if (this._hidden.has(cal.entity)) continue;
      for (const ev of this._events[cal.entity] || []) {
        if (ev.endDay < first || ev.startDay > last) continue;
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
    return { bars, perDay, lanes: laneEnds.length };
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
          <div class="heads"></div>
          <div class="week"></div>
        </section>
        <section class="controls"></section>
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
    root.querySelector(".week").addEventListener("click", (e) => {
      const el = e.target.closest("[data-ev]");
      if (el) this._showDetail(this._shown[Number(el.dataset.ev)]);
    });
    root.querySelector(".detail").addEventListener("click", (e) => {
      if (e.target.closest(".close") || !e.target.closest(".sheet")) this._hideDetail();
    });
    this._built = true;
    this._mountCards();
    if (!this._onResize) {
      this._onResize = () => this._schedule();
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

    const { bars, perDay, lanes } = this._layout(days);
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
    this._shown = shown;
    this._set(
      ".week",
      `${lanes ? `<div class="bars" style="grid-template-rows: repeat(${lanes}, auto)">${barHtml}</div>` : ""}<div class="cols">${cols}</div>`,
    );
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
  /* Landscape: calendar on the left, clock/weather/controls in a side column. */
  @media (orientation: landscape) {
    .screen {
      grid-template-columns: minmax(0, 1fr) clamp(300px, 30%, 420px);
      grid-template-rows: auto minmax(0, 1fr);
      grid-template-areas: "calendar top" "calendar controls";
      --controls-columns: 2 !important;
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
