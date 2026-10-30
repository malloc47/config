# Wall Kindle weather dashboard (aida)

A jailbroken Kindle Paperwhite 3 (1072x1448, 300 dpi, 16 grays; KOReader
installed, SSH as `root@192.168.1.164`) shows a Puppet render of a Home
Assistant dashboard, like the E1001 panel but with no clock or notices, so
it can sleep between hourly updates.

| What | Where |
|---|---|
| Card (the whole screen) | `pkgs/home-assistant-kindle-weather-card` (`custom:kindle-weather-card`) |
| Dashboard (reference copy; HA keeps it in `.storage`) | `hosts/aida/home-assistant/kindle-weather-dashboard.yaml`, URL `kindle-weather`, view `weather` |
| Image URL (LAN only, via Caddy) | `https://eink.home.malloc47.com/kindle-weather/weather?viewport=1072x1448&colors=<16 grays>&grayscale` |
| Device loop + settings | `hosts/aida/kindle-weather/weatherdash.sh`, `weatherdash.conf` → `/mnt/us/weatherdash/` |
| Library launcher | `hosts/aida/kindle-weather/Weather Dashboard.sh` → `/mnt/us/documents/` |
| Upload helper | `hosts/aida/kindle-weather/deploy.sh [--start] [user@host]` |
| Battery in HA | `sensor.kindle_weather_battery` (webhook template sensor in `nixos/modules/home-automation.nix`) |
| Device log | `/mnt/us/weatherdash/weatherdash.log` |

## How it works

`weatherdash.sh` stops the Kindle UI (`stop lab126_gui`), keeps powerd from
suspending on its own (`preventScreenSaver 1`), and loops:

1. wait for Wi-Fi to reassociate (`com.lab126.wifid cmState` = `CONNECTED`);
2. fetch the image with `If-None-Match`; Puppet answers 304 if the render is
   unchanged, and the Kindle then skips the redraw. `&next=<secs>` lets
   Puppet preload the page just before the next wake;
3. on a new image, full-refresh it with the stock `eips -g … -f` (GC16).
   KOReader's bundled `fbink` is built without image support, and `eips`
   reads PNG bytes as gray levels (an RGBA PNG comes out stretched 4x), so
   the URL asks Puppet for a single-channel PNG with `&grayscale` (a patch in
   `pkgs/ha-puppet`);
4. POST `{"battery": N, "charging": 0|1}` to the HA webhook;
5. arm the RTC alarm (`/sys/class/rtc/rtc1/wakealarm`) and
   `echo mem > /sys/power/state`.

A wake takes about 7 s (mostly Wi-Fi). Wakes are hourly at :02, with none
from 23:00 to 05:00 (`INTERVAL`, `OFFSET`, `QUIET_*` in `weatherdash.conf`).
Both PMIC RTCs (`rtc0`, `rtc1`) were tested to wake the PW3 from `mem`;
`rtc1` is used so the system clock's alarm stays untouched.

The card's grays are the panel's 16 levels (`#000000`, `#111111` … `#ffffff`):
temperature is shaded on a fixed 14-step ramp (7°F bands, darker = colder)
under the hourly curve, in each day's low–high bar and in the legend;
precipitation chance darkens its bars; night hours are a pale band. A render
depends only on the forecast and the current hour, so the ETag holds while
nothing changes. If fetches fail for `STALE_AFTER` (2 h), the loop prints a
one-line "Not updated since …" at the bottom so stale weather isn't taken as
current; the next good fetch redraws over it.

## Starting and stopping

- **Start:** open "Weather Dashboard" in the Kindle library, or
  `hosts/aida/kindle-weather/deploy.sh --start`. The loop first waits for
  the Kindle UI to finish booting and sit at home
  (`com.lab126.appmgrd activeApp` = `com.lab126.KPPMainApp`), then stops it
  and waits for its processes to exit before drawing: stopping it mid-boot
  leaves the boot splash over the dashboard, stopping it from the scriptlet
  launcher makes it report "Application Error" when it returns, and it
  blanks the screen as it shuts down.
- **Refresh now:** press the power button once (the touchscreen is off in
  suspend, so taps can't wake it). "Refreshing - press again to exit" shows
  at the bottom; after `DOUBLE_PRESS` (3) seconds the loop fetches and
  redraws, then sleeps until the next scheduled wake.
- **Stop:** press the power button twice, the second within 3 s. The loop
  clears the screen and restarts the Kindle UI, which takes about 4 minutes
  to come back. Presses are counted by the button's release interrupt
  (`max77696-onkey_rel` in `/proc/interrupts`), since a press made while the
  device is still resuming loses its press interrupt. Even so, two presses
  in very quick succession from sleep register as one; leave a beat between
  them. Holding the power button to reboot also works: nothing starts the
  loop at boot.
- SSH only works while the device is awake, so to deploy or debug, stop the
  loop with a double press first.

## Crash dumps

The home app (`KPPMainApp`, upstart job `kppmainapp`) aborts whenever the UI
is stopped under it, even on its own `stop`. The jailbreak leaves
`/PRE_GM_DEBUGGING_FEATURES_ENABLED__REMOVE_AT_GMC` in place, which turns on
Amazon's developer crash handling: `stackdumpd` runs `dump-stack`, which
writes a crash bundle into the library (`KPPMainAppV2_*_crash_*.txt/.tgz`,
listed as books) and a ~200 MB core to `/mnt/us`. So the loop pauses
`stackdumpd` while it stops the UI, and removes `/tmp/kppmainapp_started` so
`KPPMainAppCrashRecovery` logs a normal exit instead of counting toward its
crash-loop recovery screen. `/mnt/us/DISABLE_CORE_DUMP` and
`DISABLE_CORE_DUMP_ALERT` (created by hand; Amazon's own opt-outs) keep any
other crash from writing cores. A suspended `dump-stack` finishes on the next
wake, so leftovers show up only after exiting; delete them with
`rm -rf /mnt/us/documents/KPPMainAppV2_*_crash_* /mnt/us/*.core`.

## Changing it

```sh
$EDITOR pkgs/home-assistant-kindle-weather-card/kindle-weather-card.js
git add -A pkgs/home-assistant-kindle-weather-card
nixos-deploy aida            # new card; restarts Puppet (and HA)

$EDITOR hosts/aida/kindle-weather/weatherdash.conf   # or weatherdash.sh
hosts/aida/kindle-weather/deploy.sh --start          # loop stopped (double press)
```

Card config changes (entities, `days`, `hours`) go in the dashboard's raw
configuration editor; keep `kindle-weather-dashboard.yaml` in step. Render
the dashboard without the Kindle to check a change:

```sh
ssh aida "curl -s -o /tmp/kw.png 'http://127.0.0.1:10000/kindle-weather/weather?viewport=1072x1448&colors=000000,111111,222222,333333,444444,555555,666666,777777,888888,999999,AAAAAA,BBBBBB,CCCCCC,DDDDDD,EEEEEE,FFFFFF&format=png&grayscale&wait=3000'"
scp aida:/tmp/kw.png .
```

For a short-interval test on the device, copy the loop and a modified
config to `/tmp` and launch that copy (it logs next to itself), e.g.
`INTERVAL=120`, `OFFSET=0`, `QUIET_START` = `QUIET_END`.
