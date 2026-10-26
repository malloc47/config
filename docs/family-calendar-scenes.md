# Family calendar: sky scenes

Animated, composable time-of-day + weather "scenes" and holiday props for the
kitchen family-calendar dashboard (`theme: sky` of
`pkgs/home-assistant-family-calendar-card`, dashboard
`hosts/aida/home-assistant/family-calendar-dashboard.yaml`). This doc is the
plan and the progress log: pick up from the first unchecked task.

## Decisions (2026-10-25)

- Build every scene/variation from the reviewed breakdown (below).
- Less glass blur so the scenes read through the panels.
- Planets: real positions (computed in-card). Aurora: driven by real solar
  activity (planetary Kp from NOAA SWPC) with a low threshold, so it shows on
  active days rather than only when visible at 41°N.
- Rare events, configurable, defaults: lightning every 30–90 s in storms,
  a shooting star every 3–5 min on clear dark nights, birds about hourly.
- Rainbow after rain, only if the "rain stopped recently" signal is simple,
  at most once a day.
- Holidays: Halloween (the whole week before, several props including the
  Headless Horseman — we're in Sleepy Hollow), Thanksgiving, Christmas /
  winter holidays, New Year's Eve, Valentine's Day, 4th of July, and Peru's
  Fiestas Patrias (Peruvian flag prop).
- Variations rotate deterministically per day and phase (stable within a
  day, different across a static-weather week).

## Architecture

Layers, back to front, each a small module reading one shared context:

1. **sky** — gradient + palette (`sky.js`, existing).
2. **celestial** — sun glow, moon (phase + position), stars, planets, Milky
   Way, aurora, meteors.
3. **clouds** — decks, cumulus, wisps, contrails, fog banks.
4. **precipitation** — rain, snow, sleet, hail.
5. **events** — rare, timed: lightning, shooting stars, birds.
6. **edge** — effects on the calendar sheet's top edge: splashes, drips,
   snow cap, frost.
7. **props** — holiday/seasonal accents at anchors (corners, sheet top),
   reacting to the context (darkness, wind, rain, snow, lightning).

**Context**: phase (dawn/day/golden/dusk/night/late from sun elevation,
rising, hour), darkness, sun alt/az, moon phase/illumination/alt/az,
weather group + condition, wind, temperature, date, season, active
holidays, screensaver state.

**Scene** = id + `when` (weather group or condition, phases) + a list of
`[layer, params]`. Candidates are the enabled scenes whose `when` matches;
one is picked by a hash of (date, phase, group).

**DOM**: `.scene` (behind everything), `.scene-edge` (pinned to the
calendar's top edge), `.scene-front` (above content, for flashes); props
live in their own containers. Animations are CSS transforms/opacity only,
capped particle counts, and pause while the tablet's screensaver is on.

Files: `pkgs/home-assistant-family-calendar-card/scenes.js` (context,
registry, picker, layers), `sky.js` (gradient + palette),
`family-calendar-card.js` (hosts the layers, timers, config).

## Config (card YAML)

```yaml
theme: sky
glass_blur: 8                 # px; less = scenes show through more
glass_opacity: 0.8
screensaver_entity: switch.family_calendar_screensaver_active   # pauses animation
sky_scenes:
  disable: [aurora]           # scene ids to skip
  map:                        # condition or group -> scene ids (remap triggers)
    lightning-rainy: [storm-distant, storm-close]
  define:                     # extra scenes
    my-scene:
      when: { group: rain, phase: [night, late] }
      layers: [[clouds, { deck: low }], [rain, { density: 0.3 }]]
  rates:                      # seconds between rare events [min, max]
    lightning: [30, 90]
    shooting_star: [180, 300]
    birds: [3000, 4200]
sky_accents:                  # props (props.js); built-in accents by id
  disable: [halloween-ghost]
  define:                     # extra or replacement accents
    my-pumpkins:
      prop: jack-o-lantern
      when: { dates: "10-24..10-31" }   # MM-DD..MM-DD (may wrap the year)
      # or: when: { calendar: calendar.united_states_ny, match: "^Thanksgiving",
      #             days_before: 3, days_after: 0, dark: true }
      place: edge             # edge (on the sheet's top edge) | ledge (same,
                              # behind text and glass) | sky
      params: { side: right, count: 3 }
  # timed props (event:) fire at sky_scenes.rates[<event>], e.g. horseman
```

## Scene breakdown (reviewed)

Weather groups: clear (`sunny`, `clear-night`), partly (`partlycloudy`),
cloudy (`cloudy`, `exceptional`), fog, rain (`rainy`), pouring, storm
(`lightning`, `lightning-rainy`), snow (`snowy`), sleet (`snowy-rainy`,
`hail`), wind (`windy`, `windy-variant`).

| Combination | Variations |
|---|---|
| Clear · dawn | first light (horizon band); Venus morning star; pastel cirrus |
| Clear · day | crisp blue (lens shimmer); high contrails; fair-weather wisps |
| Clear · golden | long light rays; gilded glass edges; flock of birds (rare) |
| Clear · dusk | blue hour (stars fade in); evening star (Venus/Jupiter); afterglow band |
| Clear · night | moonlit (real phase); twinkling starfield; planets (real); shooting stars (rare) |
| Clear · late | Milky Way (dark-moon nights); moon high, dim warm; aurora (Kp-driven) |
| Partly · day | cumulus parade; sun peeking; cloud shadows |
| Partly · dawn/golden/dusk | lit cloud undersides; crepuscular rays |
| Partly · night | moon through clouds; patchy stars |
| Cloudy · day | soft overcast deck; bright overcast; brooding |
| Cloudy · night | city-glow overcast; moon halo |
| Fog · day | rolling fog; burn-off; fogged glass edge |
| Fog · night | streetlamp haze; blurred moon |
| Rain · day | animated rain + edge splashes; drizzle; window drops |
| Rain · night | lamp-lit rain; puddle shimmer |
| Pouring | downpour; gusty sheets; edge runoff drips |
| Storm · day | distant flashes (rare); close strike with bolt (rarer) |
| Storm · night | night strike; heat lightning |
| Snow · day | gentle snow; flurries; snow cap building on the edge |
| Snow · night | snow under lamps; quiet snow |
| Sleet / hail | sleet mix; hail bouncing off the edge |
| Wind · day | racing clouds; blown leaves/petals (seasonal); swaying props |
| Wind · night | racing clouds over the moon |

Cross-cutting: real moon phase/position (fewer stars when bright);
seasonal modifiers (spring petals, summer heat shimmer >85°F, autumn leaves
when windy, winter frost <32°F); rainbow after rain.

## Tasks

Each task is deliverable on its own (deploy, check on a scratch dashboard
and the tablet, commit). Status: `[ ]` todo, `[~]` in progress, `[x]` done.

- [x] **1. Scene engine.** Context, scene registry + config (`disable`,
  `map`, `define`, `rates`), daily seeded picker, layer hosts (`.scene`,
  `.scene-edge`, `.scene-front`), rare-event scheduler, pause on
  screensaver, reduced/configurable blur. Port today's overlays (stars,
  clouds, rain, snow, fog) to layers at parity.
- [x] **2. Props framework + Halloween.** Anchors, context-reactive props,
  `accents` config (date ranges, calendar match, days before). Halloween
  week: jack-o'-lanterns (lit by darkness, flicker in wind, sputter in
  rain, snow cap), Headless Horseman (silhouette riding past, rare),
  bats, crow, ghost.
- [x] **3. Moon + stars.** Moon phase/illumination/position (in-card),
  phase-shaded disc, star density by moon and clouds, twinkle, Milky Way,
  shooting stars.
- [ ] **4. Planets.** Real positions (simplified orbital elements →
  alt/az), bright planets above the horizon, labels; Venus morning/evening
  star variants.
- [ ] **5. Aurora.** NOAA SWPC planetary Kp (REST sensor in HA, polled
  hourly), low threshold (default Kp ≥ 4), slow curtains.
- [ ] **6. Clouds + fog.** Cumulus parade, decks (soft/bright/brooding),
  wisps, contrails, cloud shadows, lit undersides, crepuscular rays, moon
  through clouds / halo, city glow, rolling fog, burn-off, lamp haze.
- [ ] **7. Rain.** Animated rain by intensity, splashes and beads on the
  sheet edge, drizzle, window drops, lamp-lit rain, puddle shimmer,
  downpour, gusty sheets, edge drips.
- [ ] **8. Storms.** Sky flashes, forked bolts, night strikes, heat
  lightning (rates from config).
- [ ] **9. Snow, sleet, hail.** Gentle snow, flurries, snow cap building on
  the edge, lamp-lit snow, quiet snow, sleet, hail bounces, frost.
- [ ] **10. Wind.** Racing clouds, blown leaves/petals by season, swaying
  props.
- [ ] **11. Dawn/day/golden/dusk extras.** First light band, pastel cirrus,
  lens shimmer, long light rays, gilded edges, birds (rare), blue hour,
  afterglow.
- [ ] **12. Seasonal modifiers + rainbow.** Petals, heat shimmer, autumn
  leaves, frost; rainbow after rain (HA history of the weather entity,
  once a day).
- [ ] **13. Other holidays.** Thanksgiving, Christmas/winter (string
  lights), New Year's Eve (fireworks), Valentine's Day, 4th of July
  (fireworks), Fiestas Patrias (Peruvian flag).
- [ ] **14. Tuning.** Device performance pass, density/opacity tuning,
  docs.

## Testing

- Scratch dashboard (never the live one first): copy the kitchen card
  config into views with `theme: sky` and `sky_override: {elevation,
  rising, condition, hour}` (add `date` for holidays/moon once supported),
  render each with Puppet (`ssh aida curl
  'http://127.0.0.1:10000/<dash>/<view>?viewport=720x1280&format=png&wait=7000'`),
  delete it after.
- Animations: Playwright against an `ssh -L 18123:127.0.0.1:8123 aida`
  tunnel (fake clock, screenshots over time).
- The tablet: waking it for testing is fine (turn off
  `switch.family_calendar_screensaver_active`), screenshot via
  `button.family_calendar_take_screenshot` / `camera.family_calendar_screenshot`.

## Log

- 2026-10-25: plan agreed; doc created.
- 2026-10-25: task 1 done. `scenes.js` (context, registry, picker, layers,
  events scaffold), hosts in the card, `sky_scenes` config, pause on
  `screensaver_entity`, blur 22px -> `glass_blur` 8px, opacity 0.84 -> 0.8.
  Today's overlays ported as scenes `clear-day`, `clear-night`,
  `partly-day/night`, `cloudy-day/night`, `fog`, `rain`, `pouring`, `storm`,
  `snow`, `sleet`, `wind-day/night` (one each; variations come in later
  tasks). `EVENTS` is empty until task 3/8/11 add shooting stars,
  lightning and birds. Verified on a scratch dashboard (each group's scene,
  `disable`/`map`/`define`) and the tablet. Next: task 2 (props +
  Halloween, it's Halloween week).
- 2026-10-25: task 2 done. `props.js`: props (jack-o-lantern, crow, bats,
  ghost, horseman), accents by id (`sky_accents` disable/define; triggers
  by date range, calendar event match with days before/after, dark), anchors
  edge / ledge / sky, timed props via the event scheduler (horseman rides
  the ledge behind the header, default every 15–30 min, rate key
  `horseman`). Props react via `data-wx` (sputter in rain, snow caps),
  `--lit` (darkness) and `--flick` (wind). Halloween = 10-24..10-31; ghost
  10-29..10-31 after dark; bats after dark. Card `_fireEvent(name)` fires
  an event on demand (testing). Tablet CPU read 40% right after a reload
  with props + stars: measure steady state in task 14.
- 2026-10-25: task 3 done. `astro.js` (moon position/phase/illumination,
  low-precision formulas; reusable for planets). Layers `stars` (fixed
  pseudo-random field of 140, count by `density` × moonlight, fade in
  through twilight, a third twinkle), `moon` (real alt/az → screen: east
  right, altitude 0 at the sheet's top edge; phase-shaded disc, earthshine,
  halo by illumination; `glow`, `size`), `milkyway`; event
  `shooting_star`. Scenes: `clear-dusk`, `clear-night-moonlit`,
  `clear-night-starfield`, `clear-late-milkyway` (when `moon: dark`),
  `clear-late-moon`; moon added to partly/wind nights. Scene `when` gains
  `moon: dark|bright|up`; `sky_override.time` places the moon for previews.
  The card now updates scene/prop markup per item (`_renderItems`), so
  animations aren't restarted by small changes. Night header text got a
  dark halo for legibility over the moon. Tablet CPU ~30% with
  stars/moon/props/bats (task 14).
