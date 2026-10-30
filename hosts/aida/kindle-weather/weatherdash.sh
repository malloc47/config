#!/bin/sh
# weatherdash.sh — the wall Kindle's dashboard loop (docs/kindle-weather.md).
#
# Stops the Kindle UI, then repeats: wait for Wi-Fi, download the Puppet
# render of the kindle-weather dashboard (If-None-Match, so an unchanged image
# isn't redrawn), draw it with a full refresh, report the battery, set an RTC
# alarm for the next wake and suspend to RAM. E-ink keeps the picture without
# power, so the device is only awake for the few seconds each update takes.
#
# Pressing the power button ends it: the press wakes the device, the loop sees
# the button's interrupt count change and exits, and the EXIT trap restores
# the Kindle UI. Started by "Weather Dashboard.sh" in the library (which
# deploy.sh installs); nothing is installed to start it at boot, so a reboot
# also returns to the stock UI.

DIR=$(cd "$(dirname "$0")" && pwd)
# shellcheck source=weatherdash.conf
. "$DIR/weatherdash.conf"
export TZ

FBINK=/mnt/us/koreader/fbink
RTC=/sys/class/rtc/$RTC_DEV
RUN=/tmp/weatherdash
LOG=$DIR/weatherdash.log
PIDFILE=/tmp/weatherdash.pid

log() {
	echo "$(date '+%F %T') $*" >>"$LOG"
}

# Power-button presses so far (the PMIC's onkey interrupt).
onkey() {
	awk '/onkey_press/ { print $2 }' /proc/interrupts
}

cleanup() {
	trap - EXIT TERM INT HUP
	log "exiting; restoring the Kindle UI"
	echo 0 >"$RTC/wakealarm"
	lipc-set-prop com.lab126.powerd preventScreenSaver 0
	rm -f "$PIDFILE"
	$FBINK -q -c -f
	cd / && start lab126_gui
}

# Wait for the Kindle UI to be idle at home (the library, KPPMainApp on 5.16)
# before stopping it. Stopping it
# mid-boot (e.g. relaunching right after a previous run restored it) leaves
# the boot splash painting over the dashboard, and stopping it while the
# library's scriptlet launcher is the active app makes the UI report that app
# as failed ("Application Error") once it comes back; so wait for appmgrd to
# answer (empty while booting), go home, and give home a moment to settle.
ui_home() {
	initctl status lab126_gui 2>/dev/null | grep -q "^lab126_gui stop/" && return 0
	asked=0
	i=0
	while [ "$i" -lt 300 ]; do
		app=$(lipc-get-prop com.lab126.appmgrd activeApp 2>/dev/null)
		case "$app" in
		com.lab126.KPPMainApp | com.lab126.booklet.home)
			sleep 5
			return 0
			;;
		"") ;;
		*)
			[ "$asked" = 0 ] && lipc-set-prop com.lab126.appmgrd start app://com.lab126.booklet.home 2>/dev/null
			asked=1
			;;
		esac
		sleep 2
		i=$((i + 2))
	done
	log "UI not idle at home after 300s (active app: ${app:-none}); stopping it anyway"
}

stop_ui() {
	# Stopping lab126_gui signals its processes, which can include whatever
	# launched us; ignore TERM until it's down (as koreader.sh does).
	trap "" TERM
	stop lab126_gui >/dev/null 2>&1
	# The UI blanks the screen as it shuts down, well after `stop` returns on
	# 5.16, and anything drawn before that is lost: wait for its processes
	# (the Java framework, cvm, and the KPP app) to be gone.
	i=0
	while [ "$i" -lt 60 ] && pidof cvm KPPMainApp >/dev/null 2>&1; do
		sleep 1
		i=$((i + 1))
	done
	sleep 2
	trap 'exit 0' TERM
}

wifi_up() {
	i=0
	while [ "$i" -lt "$WIFI_TIMEOUT" ]; do
		[ "$(lipc-get-prop com.lab126.wifid cmState 2>/dev/null)" = CONNECTED ] && return 0
		sleep 1
		i=$((i + 1))
	done
	return 1
}

in_quiet() {
	[ "$QUIET_START" = "$QUIET_END" ] && return 1
	h=$(date -d "@$1" +%H)
	h=${h#0}
	if [ "$QUIET_START" -lt "$QUIET_END" ]; then
		[ "$h" -ge "$QUIET_START" ] && [ "$h" -lt "$QUIET_END" ]
	else
		[ "$h" -ge "$QUIET_START" ] || [ "$h" -lt "$QUIET_END" ]
	fi
}

# Next wake: the next INTERVAL boundary plus OFFSET, skipping quiet hours.
next_wake() {
	t=$((($(date +%s) / INTERVAL + 1) * INTERVAL + OFFSET))
	while in_quiet "$t"; do
		t=$((t + INTERVAL))
	done
	echo "$t"
}

# The stock eips, full GC16 refresh: KOReader's fbink is built without image
# support. eips reads PNG bytes as gray levels, hence Puppet's &grayscale.
draw() {
	out=$(eips -g "$RUN/current.png" -f 2>&1) || log "eips failed: $out"
}

# Fetch and (if new) draw the dashboard; 0 on success (new or unchanged).
update() {
	etag=""
	[ -s "$RUN/etag" ] && [ -s "$RUN/current.png" ] && etag="--etag-compare $RUN/etag"
	# shellcheck disable=SC2086
	code=$(curl -s -o "$RUN/new.png" -w '%{http_code}' $etag --etag-save "$RUN/etag.new" \
		--connect-timeout 15 --max-time 60 --retry 2 --retry-delay 3 --retry-all-errors "$URL&next=$1")
	case "$code" in
	200)
		mv "$RUN/new.png" "$RUN/current.png"
		mv "$RUN/etag.new" "$RUN/etag"
		draw
		stale_shown=0
		log "drew new image"
		;;
	304)
		rm -f "$RUN/new.png" "$RUN/etag.new"
		# Redraw to clear the stale notice, if it's up.
		[ "$stale_shown" = 1 ] && draw && stale_shown=0
		log "unchanged"
		;;
	*)
		rm -f "$RUN/new.png" "$RUN/etag.new"
		log "fetch failed (HTTP $code)"
		return 1
		;;
	esac
}

report_battery() {
	[ -n "$BATTERY_WEBHOOK" ] || return 0
	b=$(lipc-get-prop com.lab126.powerd battLevel 2>/dev/null)
	c=$(lipc-get-prop com.lab126.powerd isCharging 2>/dev/null)
	curl -s -o /dev/null --max-time 15 -X POST -H 'Content-Type: application/json' \
		-d "{\"battery\": ${b:-0}, \"charging\": ${c:-0}}" "$BATTERY_WEBHOOK"
}

show_stale() {
	[ "$STALE_AFTER" -gt 0 ] && [ "$stale_shown" = 0 ] || return 0
	[ $(($(date +%s) - last_ok)) -ge "$STALE_AFTER" ] || return 0
	$FBINK -q -m -y -2 "Not updated since $(date -d "@$last_ok" '+%a %-I:%M %p')" >/dev/null 2>&1
	stale_shown=1
	log "stale notice shown"
}

# Suspend until $1 (epoch); returns 1 if the power button woke the device.
# Other early wakes (e.g. plugging in USB) just go back to sleep.
sleep_until() {
	while :; do
		left=$(($1 - $(date +%s)))
		[ "$left" -le 5 ] && return 0
		echo 0 >"$RTC/wakealarm"
		echo $(($(cat "$RTC/since_epoch") + left)) >"$RTC/wakealarm"
		sync
		k=$(onkey)
		if ! echo mem >/sys/power/state 2>/dev/null; then
			log "suspend failed; retrying"
			sleep 10
		fi
		[ "$(onkey)" = "$k" ] || return 1
	done
}

main() {
	mkdir -p "$RUN"
	# The launcher's start-stop-daemon writes our own pid here first.
	if [ -s "$PIDFILE" ] && [ "$(cat "$PIDFILE")" != $$ ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
		echo "already running (pid $(cat "$PIDFILE"))"
		exit 1
	fi
	echo $$ >"$PIDFILE"
	# The screen shows the Kindle UI now, so the first fetch must draw: drop
	# the previous run's ETag so it can't come back 304.
	rm -f "$RUN/etag"
	[ -f "$LOG" ] && [ "$(wc -c <"$LOG")" -gt 262144 ] && tail -n 500 "$LOG" >"$LOG.tmp" && mv "$LOG.tmp" "$LOG"

	ui_home
	stop_ui
	trap cleanup EXIT
	trap 'exit 0' INT HUP
	lipc-set-prop com.lab126.powerd preventScreenSaver 1
	log "started (interval ${INTERVAL}s, quiet ${QUIET_START}-${QUIET_END}, $RTC_DEV)"

	pressed=$(onkey)
	last_ok=$(date +%s)
	stale_shown=0
	while :; do
		next=$(next_wake)
		if wifi_up; then
			update $((next - $(date +%s))) && last_ok=$(date +%s)
			report_battery
		else
			log "no Wi-Fi after ${WIFI_TIMEOUT}s"
		fi
		show_stale
		[ "$(onkey)" = "$pressed" ] || break
		log "sleeping until $(date -d "@$next" '+%F %T')"
		sleep_until "$next" || break
	done
	log "power button pressed"
}

main "$@"
