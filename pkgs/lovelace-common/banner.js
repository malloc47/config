// banner.js — the notification banner's content, shared by the in-repo
// Lovelace cards (eink-dashboard-card's footer banner on the living room's
// E1001 and family-calendar-card's bottom bar in the kitchen), so both show the
// same notices. Plain ES module with no dependencies; each card package
// installs a copy next to its own script and imports it relative to itself.
//
// Items, in order, each { icon: <MDI name>, text, shrink }:
//   - the banner message (a Jinja template, default input_text.eink_banner),
//   - calendar alerts: matching calendar events today, up all day,
//   - running appliances ("Dishwasher 40%", "... paused"),
//   - open doors: the on members of a group (or one summary item, compact).
// shrink items may ellipsize when the bar is full; the others keep their size.
//
// Config (the card's own config object is passed straight through):
//   banner_template: "{{ ... }}"                 # Jinja message; default: input_text.eink_banner
//   calendar_alerts:                             # items all day while a calendar has a
//     - entity: calendar.<name>                  # matching event today (all-day or timed)
//       match: "^Humira$"                        # regex on the event title (case-insensitive)
//       icon: needle                             # default bell-outline
//       text: Humira                             # default: the event's title
//   appliances:                                  # items while an appliance is running
//     - name: Dishwasher
//       icon: dishwasher                         # dishwasher / washing-machine / tumble-dryer
//       state_entity: sensor.dishwasher_operation_state
//       running_states: [run]                    # default [run]; "pause" shows as paused
//       progress_entity: sensor.dishwasher_program_progress   # optional "N%"
//   doors_entity: binary_sensor.perimeter_doors  # group whose open members are listed

// Free-text message shown first (empty hides it).
export const DEFAULT_BANNER = `{%- set msg = states('input_text.eink_banner') -%}
{{ msg if msg not in ['', 'unknown', 'unavailable'] else '' }}`;

const num = (v) => {
  const n = parseFloat(v);
  return Number.isFinite(n) ? n : null;
};

// "YYYY-MM-DD" plus n days.
const shift = (date, n) => {
  const d = new Date(`${date}T12:00:00Z`);
  d.setUTCDate(d.getUTCDate() + n);
  return d.toISOString().slice(0, 10);
};

export class Banner {
  // onChange runs when the message or the calendar alerts change; localDate(ts)
  // gives a timestamp's "YYYY-MM-DD" in HA's time zone.
  constructor(onChange, localDate) {
    this._onChange = onChange;
    this._localDate = localDate;
    this._message = "";
    this.config = {};
    this.hass = null;
  }

  configure(config) {
    this.config = { banner_template: DEFAULT_BANNER, calendar_alerts: [], appliances: [], ...config };
    this._calAlerts = this.config.calendar_alerts.map((a) => {
      try {
        return { ...a, re: new RegExp(a.match ?? "", "i") };
      } catch (err) {
        throw new Error(`calendar_alerts: bad match ${JSON.stringify(a.match)}: ${err.message}`);
      }
    });
    this._message = "";
    this._calItems = [];
    this._calItemsDay = null;
    this._calFetched = null;
  }

  setHass(hass) {
    this.hass = hass;
  }

  // The message template's subscription, in Weather.ensureSubscriptions()'s
  // `extra` format, so it shares the forecasts' retries.
  subscriptions() {
    if (!this.config.banner_template) return {};
    return {
      banner: {
        msg: { type: "render_template", template: this.config.banner_template, strict: false, report_errors: false },
        onMessage: (m) => {
          if ("result" in m) this._message = String(m.result ?? "").trim();
        },
      },
    };
  }

  // Everything to show now. compact folds the open doors into one item (for
  // a bar that overflowed).
  items(compact = false) {
    this._refreshCalendarAlerts();
    const doors = this.openDoors();
    return [
      ...(this._message ? [{ icon: "bell-outline", text: this._message, shrink: true }] : []),
      ...(this._calItemsDay === this._localDate(Date.now()) ? this._calItems : []),
      ...this.appliances(),
      ...(compact && doors.length > 1
        ? [{ icon: "door-open", text: `${doors.length} open: ${doors.map((d) => d.text).join(", ")}`, shrink: true }]
        : doors),
    ];
  }

  // Open members of doors_entity (a group): an open-door icon (garage/window
  // by device class) and the name without a trailing "Door". Gates use the
  // door icon too: MDI's gate glyph breaks up at 22px in 1-bit.
  openDoors() {
    const group = this.config.doors_entity && this.hass.states[this.config.doors_entity];
    const members = group?.attributes?.entity_id || [];
    return members
      .map((id) => this.hass.states[id])
      .filter((st) => st && st.state === "on")
      .map((st) => {
        const name = st.attributes.friendly_name || st.entity_id;
        const dc = st.attributes.device_class;
        const ic = dc === "garage_door" ? "garage-open-variant" : dc === "window" ? "window-open-variant" : "door-open";
        return { icon: ic, text: name.replace(/\s+door$/i, "") };
      });
  }

  // Running appliances: icon + "<name> <progress>%" (with "paused"
  // mid-cycle). Nothing when idle, finished or unavailable.
  appliances() {
    return (this.config.appliances || [])
      .map((a) => {
        const st = this.hass.states[a.state_entity]?.state;
        const running = (a.running_states || ["run"]).includes(st);
        const paused = st === "pause";
        if (!running && !paused) return null;
        const pct = a.progress_entity ? num(this.hass.states[a.progress_entity]?.state) : null;
        const parts = [a.name, paused ? "paused" : null, pct !== null ? `${Math.round(pct)}%` : null];
        return { icon: a.icon || "dishwasher", text: parts.filter(Boolean).join(" ") };
      })
      .filter(Boolean);
  }

  // Calendar events today that match a calendar_alerts rule, fetched from HA's
  // calendar API and refreshed every 15 minutes and when the day changes; the
  // item stays up for the whole day of the event.
  _refreshCalendarAlerts() {
    if (!this._calAlerts?.length || !this.hass) return;
    const today = this._localDate(Date.now());
    const f = this._calFetched;
    if (f && (f.pending || (f.day === today && Date.now() - f.at < 15 * 60000))) return;
    this._calFetched = { day: today, at: Date.now(), pending: true };
    // Pad a day either side (UTC) and decide "today" in HA's time zone.
    const q = `?start=${encodeURIComponent(`${shift(today, -1)}T00:00:00Z`)}&end=${encodeURIComponent(`${shift(today, 2)}T00:00:00Z`)}`;
    const entities = [...new Set(this._calAlerts.map((a) => a.entity))];
    Promise.all(
      entities.map((e) =>
        this.hass.callApi("GET", `calendars/${e}${q}`).then(
          (evs) => [e, evs],
          (err) => {
            console.warn(`banner: fetching ${e} failed`, err);
            return [e, null];
          },
        ),
      ),
    ).then((results) => {
      const byEntity = Object.fromEntries(results);
      const failed = results.some(([, evs]) => evs === null);
      const items = [];
      for (const a of this._calAlerts) {
        for (const ev of byEntity[a.entity] || []) {
          const allDay = !!ev.start?.date;
          const first = allDay ? ev.start.date : this._localDate(Date.parse(ev.start.dateTime));
          const last = allDay
            ? shift(ev.end?.date || shift(ev.start.date, 1), -1)
            : this._localDate(Math.max(Date.parse(ev.start.dateTime), Date.parse(ev.end?.dateTime) - 1 || 0));
          if (first > today || last < today || !a.re.test(ev.summary || "")) continue;
          const text = a.text || ev.summary;
          if (!items.some((it) => it.text === text)) items.push({ icon: a.icon || "bell-outline", text, shrink: true });
        }
      }
      this._calItems = items;
      this._calItemsDay = today;
      // Retry a failed fetch (e.g. HA still starting) after a minute.
      this._calFetched = { day: today, at: failed ? Date.now() - 14 * 60000 : Date.now() };
      this._onChange();
    });
  }
}
