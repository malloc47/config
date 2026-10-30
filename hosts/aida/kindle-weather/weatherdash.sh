#!/bin/sh
# weatherdash.sh — the wall Kindle's dashboard loop (docs/kindle-weather.md).
#
# Stops the Kindle UI, then repeats: wait for Wi-Fi, download the Puppet
# render of the kindle-weather dashboard (If-None-Match, so an unchanged image
# isn't redrawn), draw it with a full refresh, set an RTC alarm for the next
# wake and suspend to RAM. E-ink keeps the picture without power, so the
# device is only awake for the few seconds each update takes. On each wake it
# also reads its settings from Home Assistant and reports its status, over
# MQTT (nixos/modules/kindle-weather.nix).
#
# The power button (the only wake source besides the RTC; the touchscreen is
# off in suspend): one press refreshes now, a second within DOUBLE_PRESS
# seconds ends it, and the EXIT trap restores the Kindle UI. Presses are
# counted from the button's interrupts in /proc/interrupts. Started by
# "Weather Dashboard.sh" in the library (which
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
BATT=/sys/class/power_supply/max77696-battery
MQTT_PASSWORD=$(cat "$DIR/mqtt.secret" 2>/dev/null)

log() {
	echo "$(date '+%F %T') $*" >>"$LOG"
}

# Power-button presses so far, counted by the PMIC's release interrupt: a
# press made while the device is still resuming loses its press interrupt
# but not its release.
onkey() {
	awk '/onkey_rel/ { print $2 }' /proc/interrupts
}

# shellcheck disable=SC2329 # run by the EXIT trap
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
	# The home app (KPPMainApp, its own kppmainapp job) aborts on the way
	# down, even when stopped on its own. stackdumpd catches that and, with
	# the jailbreak's /PRE_GM_DEBUGGING_FEATURES_ENABLED__REMOVE_AT_GMC flag,
	# writes a crash bundle into the library (KPPMainAppV2_*_crash_*) and a
	# ~200 MB core to /mnt/us (unless /mnt/us/DISABLE_CORE_DUMP exists). So
	# pause stackdumpd while the UI goes down, and clear the app's started
	# flag so its post-stop crash recovery logs a normal exit rather than
	# counting a crash toward its crash-loop recovery screen.
	stop stackdumpd >/dev/null 2>&1
	rm -f /tmp/kppmainapp_started
	stop kppmainapp >/dev/null 2>&1
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
	start stackdumpd >/dev/null 2>&1
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
		result=new
		refreshed=$(date +%s)
		log "drew new image"
		;;
	304)
		rm -f "$RUN/new.png" "$RUN/etag.new"
		# Redraw to clear the stale notice, if it's up.
		[ "$stale_shown" = 1 ] && draw && stale_shown=0
		result=unchanged
		log "unchanged"
		;;
	*)
		rm -f "$RUN/new.png" "$RUN/etag.new"
		result=failed
		log "fetch failed (HTTP $code)"
		return 1
		;;
	esac
}

mqtt() {
	[ -n "$MQTT_HOST" ] && [ -n "$MQTT_PASSWORD" ]
}

# Settings from Home Assistant, retained on kindle-weather/set/#. curl's MQTT
# subscribe never ends on its own, so it reads for MQTT_READ_TIME (retained
# messages arrive at once) and prints each message as a 2-byte topic length,
# the topic and the payload, back to back; payloads are short text, so a NUL
# (the length's high byte) starts the next message.
read_settings() {
	want_exit=0
	mqtt || return 0
	curl -s --max-time "$MQTT_READ_TIME" -u "kindle:$MQTT_PASSWORD" \
		"mqtt://$MQTT_HOST:$MQTT_PORT/kindle-weather/set/%23" >"$RUN/set" 2>/dev/null
	od -An -v -tu1 "$RUN/set" | awk '
		{ for (i = 1; i <= NF; i++) b[++n] = $i }
		END {
			i = 1
			while (i < n && b[i] == 0) {
				t = ""
				for (j = i + 2; j < i + 2 + b[i + 1] && j <= n; j++) t = t sprintf("%c", b[j])
				p = ""
				for (; j <= n && b[j] != 0; j++) p = p sprintf("%c", b[j])
				sub(/.*\//, "", t)
				print t "=" p
				i = j
			}
		}' >"$RUN/set.txt"
	while IFS='=' read -r key val; do
		val=${val%%.*}
		case "$key" in
		interval) [ "$val" -ge 5 ] 2>/dev/null && [ "$val" -le 720 ] && INTERVAL=$((val * 60)) ;;
		quiet_start) [ "$val" -ge 0 ] 2>/dev/null && [ "$val" -le 23 ] && QUIET_START=$val ;;
		quiet_end) [ "$val" -ge 0 ] 2>/dev/null && [ "$val" -le 23 ] && QUIET_END=$val ;;
		maintenance) [ "$val" = ON ] && maintenance=ON || maintenance=OFF ;;
		exit) [ -n "$val" ] && want_exit=1 ;;
		esac
	done <"$RUN/set.txt"
}

# Status for Home Assistant (kindle-weather/state; aida keeps it retained).
# $1 is the next scheduled wake.
publish_status() {
	mqtt || return 0
	s=$(lipc-get-prop com.lab126.wifid signalStrength 2>/dev/null)
	c=$(lipc-get-prop com.lab126.powerd isCharging 2>/dev/null)
	# Firmware ("Kindle 5.16.2.1.1 (...)"): a change means an OTA update,
	# which can undo the jailbreak or the ad removal. Crash dumps: cores in
	# /mnt/us and crash bundles (.tgz) in the library (see stop_ui).
	fw=$(awk '{ print $2; exit }' /etc/prettyversion.txt 2>/dev/null)
	dumps=$(find /mnt/us /mnt/us/documents -maxdepth 1 \( -name '*.core' -o -name '*.tgz' \) 2>/dev/null | wc -l)
	json=$(printf '{"battery":%d,"charging":%s,"voltage":%d,"current":%d,"temperature":%d,"cycles":%d,"wifi":%d,"wake":%d,"next":%d,"refreshed":%d,"fetched":%d,"boot":%d,"firmware":"%s","storage_free":%d,"dumps":%d,"reason":"%s","result":"%s","awake":%d,"interval":%d,"quiet_start":%d,"quiet_end":%d,"maintenance":"%s","exit_seen":%s}' \
		"$(cat $BATT/capacity)" "$([ "$c" = 1 ] && echo true || echo false)" \
		"$(cat $BATT/voltage_now)" "$(cat $BATT/current_now)" "$(cat $BATT/temp)" \
		"$(cat $BATT/cycle_count)" "${s%%/*}" "$wake_at" "$1" "$refreshed" "$fetched" \
		"$(awk '/^btime/ { print $2 }' /proc/stat)" "$fw" \
		"$(df -k /mnt/us | awk 'NR == 2 { print int($4 / 1024) }')" "$dumps" "$reason" "$result" \
		$(($(date +%s) - wake_at)) $((INTERVAL / 60)) "$QUIET_START" "$QUIET_END" "$maintenance" \
		"$([ "$want_exit" = 1 ] && echo true || echo false)" 2>/dev/null)
	curl -s -o /dev/null --max-time 5 -u "kindle:$MQTT_PASSWORD" -d "$json" \
		"mqtt://$MQTT_HOST:$MQTT_PORT/kindle-weather/state" || log "status publish failed"
}

# Maintenance mode (switched on in HA): stay awake, reachable over SSH,
# re-reading the settings every 30 s, until it's switched off, the next update
# ($1) is due, the power button is pressed or HA asks to exit.
maintenance_wait() {
	log "maintenance mode: staying awake"
	while [ "$maintenance" = ON ] && [ "$(date +%s)" -lt "$1" ]; do
		i=0
		while [ "$i" -lt 30 ]; do
			sleep 1
			[ "$(onkey)" = "$pressed" ] || return 0
			i=$((i + 1))
		done
		lipc-set-prop com.lab126.powerd preventScreenSaver 1
		read_settings
		[ "$want_exit" = 1 ] && return 0
		publish_status "$1"
	done
	log "maintenance mode: off"
}

show_stale() {
	[ "$STALE_AFTER" -gt 0 ] && [ "$stale_shown" = 0 ] || return 0
	[ $(($(date +%s) - last_ok)) -ge "$STALE_AFTER" ] || return 0
	$FBINK -q -m -y -2 "Not updated since $(date -d "@$last_ok" '+%a %-I:%M %p')" >/dev/null 2>&1
	stale_shown=1
	log "stale notice shown"
}

# Suspend until $1 (epoch); returns 1 if the power button woke the device,
# with the press count from before it slept in $slept_at. Other early wakes
# (e.g. plugging in USB) just go back to sleep.
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
		slept_at=$k
		[ "$(onkey)" = "$k" ] || return 1
	done
}

# After a power-button press: say so at the bottom of the screen, then wait
# DOUBLE_PRESS seconds for a second one, counting from $1, the count before
# the first (a quick double press can land entirely while the device is still
# resuming). Returns 1 on a second press (exit), else 0 with the next fetch
# forced to redraw.
handle_press() {
	$FBINK -q -m -y -2 "Refreshing - press again to exit" >/dev/null 2>&1
	i=0
	while [ $(($(onkey) - $1)) -lt 2 ]; do
		[ "$i" -ge $((DOUBLE_PRESS * 10)) ] && break
		usleep 100000
		i=$((i + 1))
	done
	if [ $(($(onkey) - $1)) -ge 2 ]; then
		log "power button pressed twice"
		return 1
	fi
	pressed=$(onkey)
	rm -f "$RUN/etag"
	forced=1
	log "power button: refreshing"
}

# deploy.sh sends HUP after uploading: re-run the new script in place (same
# pid, Kindle UI still stopped) instead of exiting and restoring the UI.
# shellcheck disable=SC2329 # run by the HUP trap
reload() {
	log "reloading"
	trap - EXIT
	export WEATHERDASH_RELOAD=1
	exec /bin/sh "$DIR/weatherdash.sh"
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

	if [ "$WEATHERDASH_RELOAD" != 1 ]; then
		ui_home
		stop_ui
	fi
	trap cleanup EXIT
	trap 'exit 0' INT
	trap reload HUP
	log "started (interval ${INTERVAL}s, quiet ${QUIET_START}-${QUIET_END}, $RTC_DEV${WEATHERDASH_RELOAD:+, reloaded})"

	pressed=$(onkey)
	last_ok=$(date +%s)
	refreshed=$last_ok
	fetched=0
	stale_shown=0
	forced=0
	maintenance=OFF
	reason=start
	while :; do
		wake_at=$(date +%s)
		result=no_wifi
		# Without this powerd times out into its screensaver, which draws its
		# artwork over the dashboard; re-asserted on each wake in case
		# anything else reset it.
		lipc-set-prop com.lab126.powerd preventScreenSaver 1
		if wifi_up; then
			read_settings
			# An exit left over from before this start (e.g. pressed while the
			# Kindle UI was up) is acknowledged but not acted on.
			if [ "$want_exit" = 1 ] && [ "$reason" != start ]; then
				result="exit"
				publish_status 0
				log "exit requested from Home Assistant"
				break
			fi
			next=$(next_wake)
			update $((next - $(date +%s))) && last_ok=$(date +%s) && fetched=$last_ok
		else
			next=$(next_wake)
			log "no Wi-Fi after ${WIFI_TIMEOUT}s"
		fi
		# A failed refresh still has to clear the "Refreshing" line.
		[ "$forced" = 1 ] && [ "$result" != new ] && draw
		forced=0
		show_stale
		publish_status "$next"
		# Pressed while awake (e.g. during the fetch), or woke by the button.
		if [ "$(onkey)" != "$pressed" ]; then
			handle_press "$pressed" || break
			reason=button
			continue
		fi
		if [ "$maintenance" = ON ]; then
			maintenance_wait "$next"
			reason=maintenance
			continue
		fi
		log "sleeping until $(date -d "@$next" '+%F %T')"
		if sleep_until "$next"; then
			reason=timer
			continue
		fi
		handle_press "$slept_at" || break
		reason=button
	done
}

# On one line, so the shell has read the whole file before running any of it
# and deploy.sh can replace the file under a running loop.
main "$@"; exit
