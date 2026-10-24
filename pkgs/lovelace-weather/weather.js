// weather.js — forecast model shared by the in-repo Lovelace cards
// (eink-dashboard-card and family-calendar-card), so both describe the weather
// the same way. Plain ES module with no dependencies; each card package installs
// a copy next to its own script (identical files, so the cards can share HA's
// nixos-lovelace-modules directory) and imports it relative to itself.
//
// Weather subscribes to the hourly and daily (or twice_daily) forecasts of a
// primary weather entity and an optional fallback, picks whichever source is
// usable, and summarises it: outlookRow("today" | "tomorrow") gives the
// E1001's "what to wear" outlook line (rain timing or sky, temperature range
// and a clothing hint) for the configured outdoor hours.
//
// Config (the card's own config object is passed straight through):
//   weather_entity: weather.nws_khpn
//   fallback_weather_entity: weather.forecast_home  # optional
//   rain_probability: 50     # % at which an hour counts as rainy (when given)
//   outdoor_hours: [7, 22]   # local hours "outside" for the outlook
//   cold: 45                 # °F below which it's coat weather
//   hot: 85                  # °F at/above which it's hot

export const CONDITION_ICON = {
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

export const CONDITION_TEXT = {
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
export const CONDITION_RANK = {
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


export const num = (v) => {
  const n = parseFloat(v);
  return Number.isFinite(n) ? n : null;
};

export const temp = (v) => (num(v) === null ? "–" : `${Math.round(num(v))}°`);

export const precip = (v, unit) => {
  const n = num(v);
  if (n === null) return "–";
  if (n < 0.005) return `0 ${unit}`;
  const s = n < 1 ? n.toFixed(2).replace(/0+$/, "").replace(/\.$/, "") : n.toFixed(1);
  return `${s} ${unit}`;
};

export class Weather {
  // onChange runs whenever new forecast data arrives.
  constructor(onChange) {
    this._onChange = onChange;
    // Forecast subscriptions' latest data, keyed "<entity_id>|<forecast_type>".
    this._fc = {};
    // Active subscriptions by key: a promise of the unsubscribe function, or a
    // retry deadline after a failed subscribe.
    this._subs = {};
    this._subscribedFor = null;
    this._srcCache = null;
    this.config = {};
    this.hass = null;
  }

  configure(config) {
    this.config = { rain_probability: 50, outdoor_hours: [7, 22], cold: 45, hot: 85, ...config };
    this.unsubscribe();
  }

  setHass(hass) {
    if (this._subscribedFor && this._subscribedFor !== hass.connection) this.unsubscribe();
    this.hass = hass;
  }

  // Drop the cached source choice; call at the start of each render.
  invalidate() {
    this._srcCache = null;
  }

  unsubscribe() {
    for (const sub of Object.values(this._subs)) {
      if (sub.unsub) sub.unsub.then((f) => f && f()).catch(() => {});
    }
    this._subs = {};
    this._subscribedFor = null;
  }

  // (Re)subscribe anything not currently subscribed. Call on every hass
  // update, so a subscription that failed (e.g. the weather entity not loaded
  // yet right after an HA restart) is retried instead of leaving the card
  // permanently without forecasts; Puppet and wall tablets keep one
  // long-lived page, so the card has to heal itself. `extra` adds the card's
  // own subscriptions, as { key: { msg, onMessage } }, under the same retries.
  ensureSubscriptions(extra = {}) {
    if (!this.hass) return;
    const conn = this.hass.connection;
    this._subscribedFor = conn;
    // Hourly plus daily forecasts for the primary and fallback weather entities,
    // using twice_daily (day/night periods, e.g. NWS) where daily isn't offered.
    // Entities not loaded yet are picked up on a later update.
    const wanted = { ...extra };
    for (const entity of [this.config.weather_entity, this.config.fallback_weather_entity].filter(Boolean)) {
      const sf = this.hass.states[entity]?.attributes?.supported_features;
      if (sf === undefined) continue;
      const types = [];
      if (sf & 2) types.push("hourly");
      if (sf & 1) types.push("daily");
      else if (sf & 4) types.push("twice_daily");
      for (const type of types) {
        const key = `${entity}|${type}`;
        wanted[key] = {
          msg: { type: "weather/subscribe_forecast", forecast_type: type, entity_id: entity },
          onMessage: (m) => {
            this._fc[key] = m.forecast || [];
          },
        };
      }
    }
    const now = Date.now();
    for (const [key, { msg, onMessage }] of Object.entries(wanted)) {
      const sub = this._subs[key];
      if (sub && (sub.unsub || now < sub.retryAt)) continue;
      const unsub = conn.subscribeMessage((m) => {
        onMessage(m);
        this._onChange();
      }, msg);
      this._subs[key] = { unsub };
      unsub.catch((err) => {
        console.warn(`weather: ${key} subscription failed, retrying`, err);
        if (this._subs[key]?.unsub === unsub) this._subs[key] = { retryAt: Date.now() + 15000 };
      });
    }
  }

  // Local date ("YYYY-MM-DD") and hour for a timestamp in HA's time zone, so the
  // render doesn't depend on the screenshot browser's own TZ.
  local(ts) {
    const tz = this.hass?.config?.time_zone;
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

  weekday(ts, style = "short") {
    return new Intl.DateTimeFormat("en-US", { timeZone: this.hass?.config?.time_zone, weekday: style }).format(
      new Date(ts),
    );
  }

  clockTime(ts) {
    return new Intl.DateTimeFormat("en-US", {
      timeZone: this.hass?.config?.time_zone,
      hour: "numeric",
      minute: "2-digit",
    }).format(new Date(ts));
  }

  nightIcon(condition) {
    if (condition === "sunny") return "weather-night";
    if (condition === "partlycloudy") return "weather-night-partly-cloudy";
    return CONDITION_ICON[condition] || "alert-circle-outline";
  }

  conditionIcon(condition, night) {
    return night ? this.nightIcon(condition) : CONDITION_ICON[condition] || "alert-circle-outline";
  }

  // The weather source for this render: the primary entity, or the fallback
  // while the primary is unavailable or has no forecasts yet. Daily forecasts
  // are normalized to one entry per day (twice_daily periods paired up).
  src() {
    if (this._srcCache) return this._srcCache;
    const pick = (entity) => {
      const state = entity && this.hass.states[entity];
      if (!state) return null;
      const hourly = this._fc[`${entity}|hourly`] || [];
      const daily = this._fc[`${entity}|daily`] || this.pairTwiceDaily(this._fc[`${entity}|twice_daily`] || []);
      const ok = !["unavailable", "unknown"].includes(state.state) && hourly.length > 0 && daily.length > 0;
      return { entity, state, hourly, daily, ok };
    };
    const primary = pick(this.config.weather_entity);
    const fallback = pick(this.config.fallback_weather_entity);
    this._srcCache = (primary?.ok ? primary : fallback?.ok ? fallback : primary) || {
      entity: this.config.weather_entity,
      state: undefined,
      hourly: [],
      daily: [],
      ok: false,
    };
    return this._srcCache;
  }

  // Day/night periods -> daily entries: the day period's high and condition,
  // the following night's low, and the higher precipitation probability.
  pairTwiceDaily(periods) {
    const days = new Map();
    for (const p of periods) {
      const date = this.local(p.datetime).date;
      if (!days.has(date)) days.set(date, { datetime: p.datetime });
      const d = days.get(date);
      if (p.is_daytime) {
        Object.assign(d, { datetime: p.datetime, condition: p.condition, temperature: p.temperature });
      } else {
        d.templow = p.temperature;
        d.condition = d.condition || p.condition;
      }
      if (p.precipitation_probability != null) {
        d.precipitation_probability = Math.max(d.precipitation_probability ?? 0, p.precipitation_probability);
      }
    }
    return [...days.values()];
  }

  // A day's / bucket's precipitation: probability when the source gives one,
  // else the amount.
  precipText(prob, amount) {
    if (prob != null) return `${Math.round(prob)}%`;
    return precip(amount, this.src().state?.attributes?.precipitation_unit || "");
  }

  // Next three morning/afternoon/night buckets from the hourly forecast,

  // ---- "what to wear" outlook ------------------------------------------

  hourLabel(h) {
    const hh = ((h % 24) + 24) % 24;
    if (hh === 0) return "midnight";
    if (hh === 12) return "noon";
    return `${hh % 12 || 12} ${hh < 12 ? "AM" : "PM"}`;
  }

  // "1–3 PM" rather than "1 PM–3 PM" when both ends share AM/PM.
  hourRange(a, b) {
    const la = this.hourLabel(a);
    const lb = this.hourLabel(b);
    const [na, sa] = la.split(" ");
    const sb = lb.split(" ")[1];
    return sa && sa === sb ? `${na}–${lb}` : `${la}–${lb}`;
  }

  // °F for classification, whatever the weather entity reports in.
  toF(t) {
    const unit = this.src().state?.attributes?.temperature_unit;
    return unit === "°C" ? (t * 9) / 5 + 32 : t;
  }

  isWet(f) {
    // With a probability (e.g. NWS), it's rainy when likely enough; otherwise
    // go by amount or a wet condition.
    if (f.precipitation_probability != null) return f.precipitation_probability >= this.config.rain_probability;
    const unit = this.src().state?.attributes?.precipitation_unit;
    const min = unit === "mm" ? 0.25 : 0.01;
    return (
      (num(f.precipitation) || 0) >= min ||
      ["rainy", "pouring", "lightning-rainy", "snowy", "snowy-rainy", "hail"].includes(f.condition)
    );
  }

  // Hourly forecast entries for one outlook window, tagged with a continuous
  // hour number (hours after midnight run past 24 for "tonight").
  window(kind) {
    const [start, end] = this.config.outdoor_hours;
    const now = this.local(Date.now());
    const d = new Date(`${now.date}T12:00:00Z`);
    d.setUTCDate(d.getUTCDate() + 1);
    const tomorrow = d.toISOString().slice(0, 10);
    const hours = this.src().hourly.map((f) => {
      const l = this.local(f.datetime);
      return { ...f, h: l.date === now.date ? l.hour : l.date === tomorrow ? l.hour + 24 : 99 };
    });
    if (kind === "tomorrow") return hours.filter((f) => f.h >= 24 + start && f.h < 24 + end);
    if (now.hour < end - 1) return hours.filter((f) => f.h >= Math.max(now.hour, start) && f.h < end);
    // Late evening: describe the night instead (until 6 AM).
    return hours.filter((f) => f.h >= now.hour && f.h < 30);
  }

  outlookRow(kind) {
    const hours = this.window(kind);
    const now = this.local(Date.now());
    const [, end] = this.config.outdoor_hours;
    const tonight = kind === "today" && now.hour >= end - 1;
    const label = kind === "tomorrow" ? "Tomorrow" : tonight ? "Tonight" : "Today";
    if (!hours.length) return { label, icon: "alert-circle-outline", text: "No forecast", range: "", wear: null };

    const unit = this.src().state?.attributes?.precipitation_unit || "";
    const temps = hours.map((f) => num(f.temperature)).filter((t) => t !== null);
    const lo = Math.min(...temps);
    const hi = Math.max(...temps);
    const conds = hours.map((f) => f.condition);
    const worst = conds.reduce((a, b) => ((CONDITION_RANK[b] || 0) > (CONDITION_RANK[a] || 0) ? b : a));
    // Most common condition, with "wet" conditions read as cloudy: for a dry
    // window it's a better icon/description than the single worst hour (NWS
    // tags slight-chance hours as rainy).
    const WET = ["rainy", "pouring", "lightning-rainy", "lightning", "snowy", "snowy-rainy", "hail", "exceptional"];
    const counts = {};
    for (const c of conds) {
      const k = WET.includes(c) ? "cloudy" : c;
      counts[k] = (counts[k] || 0) + 1;
    }
    const usual = Object.entries(counts).sort((a, b) => b[1] - a[1])[0][0];
    const pops = hours.map((f) => f.precipitation_probability).filter((v) => v != null);
    const maxPop = pops.length ? Math.max(...pops) : null;
    const nightish = tonight;

    // Rain spans: consecutive wet hours, bridging single dry hours.
    const wet = hours.filter((f) => this.isWet(f));
    const spans = [];
    for (const f of wet) {
      const last = spans[spans.length - 1];
      if (last && f.h - last.end <= 1) last.end = f.h + 1;
      else spans.push({ start: f.h, end: f.h + 1 });
    }
    const first = hours[0].h;
    const lastH = hours[hours.length - 1].h + 1;
    // Wordings from most to least detailed; the render uses the first that
    // fits its row (see _fitText).
    let texts;
    if (spans.length) {
      const kindWord = conds.some((c) => c === "snowy")
        ? "Snow"
        : conds.some((c) => c === "snowy-rainy" || c === "hail")
          ? "Sleet"
          : conds.some((c) => c.startsWith("lightning"))
            ? "Storms"
            : "Rain";
      // terse: "till" for "until", and a later span without "again".
      const describe = (sp, lead, terse) => {
        const w = lead ? `${kindWord} ` : terse ? "" : "again ";
        if (sp.start <= first && sp.end >= lastH) return lead ? `${kindWord} ${tonight ? "all night" : "all day"}` : "";
        if (sp.start <= first) return `${w}${terse ? "till" : "until"} ${this.hourLabel(sp.end)}`;
        if (sp.end >= lastH) return `${w}from ${this.hourLabel(sp.start)}`;
        return `${w}${this.hourRange(sp.start, sp.end)}`;
      };
      const phrase = (terse) =>
        spans
          .slice(0, 2)
          .map((sp, i) => describe(sp, i === 0, terse))
          .filter(Boolean)
          .join(", ") + (spans.length > 2 ? ", …" : "");
      const total = wet.reduce((sum, f) => sum + (num(f.precipitation) || 0), 0);
      const probs = wet.map((f) => f.precipitation_probability).filter((v) => v != null);
      const amount = probs.length
        ? ` (${Math.max(...probs)}%)`
        : total >= 0.005
          ? ` (${precip(total, unit)})`
          : "";
      texts = [`${phrase(false)}${amount}`, phrase(false), phrase(true)];
    } else {
      const sky = {
        sunny: tonight ? "clear" : "sunny",
        "clear-night": "clear",
        partlycloudy: "partly cloudy",
        cloudy: "cloudy",
        fog: "foggy",
        windy: "windy",
        "windy-variant": "windy",
      }[usual];
      if (maxPop !== null && maxPop >= 20) {
        const Sky = sky ? sky[0].toUpperCase() + sky.slice(1) : "Mostly dry";
        // A bare percentage reads as the chance of precipitation, as in the
        // rain spans' "(73%)". When the peak chance is confined to one stretch
        // of at most half the window, say when: "Cloudy (39% until 11 PM)".
        // Hours within 5 points of the peak count toward it (NWS steps its
        // probabilities in blocks).
        const peak = hours.filter((f) => (f.precipitation_probability ?? 0) >= maxPop - 5);
        const start = peak[0].h;
        const end = peak[peak.length - 1].h + 1;
        let when = "";
        if (peak.length === end - start && end - start <= (lastH - first) / 2) {
          when =
            start <= first
              ? ` until ${this.hourLabel(end)}`
              : end >= lastH
                ? ` from ${this.hourLabel(start)}`
                : ` ${this.hourRange(start, end)}`;
        }
        texts = [`${Sky} (${maxPop}%${when})`, `${Sky} (${maxPop}%)`];
      } else {
        texts = [sky ? `Dry, ${sky}` : "Dry"];
      }
    }

    // Dress for the coldest outdoor hour; flag heat separately.
    const loF = this.toF(lo);
    const hiF = this.toF(hi);
    const { cold, hot } = this.config;
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
      icon: this.conditionIcon(spans.length ? worst : usual, nightish),
      text: texts[0],
      texts: [...new Set(texts)],
      range: Math.round(lo) === Math.round(hi) ? temp(lo) : `${Math.round(lo)}–${temp(hi)}`,
      wear,
    };
  }

}
