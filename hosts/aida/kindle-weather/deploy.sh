#!/usr/bin/env bash
# deploy.sh — upload the wall Kindle's weather dashboard (docs/kindle-weather.md).
#
#   hosts/aida/kindle-weather/deploy.sh [--start] [user@host]
#
# Copies weatherdash.sh and weatherdash.conf to /mnt/us/weatherdash and the
# "Weather Dashboard" launcher to the library (/mnt/us/documents). A running
# loop reloads the new files in place; otherwise --start launches it (as
# tapping the launcher would). The Kindle only answers SSH while awake: while
# the dashboard runs, turn on its maintenance mode in Home Assistant (it stays
# awake from its next wake) or double-press the power button (which exits to
# the Kindle UI).
set -euo pipefail

cd "$(dirname "$0")"

start=0
if [ "${1:-}" = "--start" ]; then
  start=1
  shift
fi
host=${1:-${KINDLE_HOST:-root@192.168.1.164}}
ssh=(ssh -o ConnectTimeout=10 "$host")

if ! "${ssh[@]}" true 2>/dev/null; then
  echo "can't reach $host; wake the Kindle (power button) and retry" >&2
  exit 1
fi

# A loop that's running (awake: maintenance mode, or within a wake) is told to
# reload the new files in place after the upload, keeping the Kindle UI
# stopped; weatherdash.sh is safe to replace under it.
# shellcheck disable=SC2016 # expands on the Kindle
running=$("${ssh[@]}" '[ -s /tmp/weatherdash.pid ] && kill -0 "$(cat /tmp/weatherdash.pid)" 2>/dev/null && cat /tmp/weatherdash.pid' || true)

# Streamed over ssh rather than scp: the launcher's name has a space, which
# scp's remote quoting handles differently in its SFTP and legacy modes.
put() {
  "${ssh[@]}" "cat > '$2' && chmod 755 '$2'" <"$1"
  echo "  $1 -> $2"
}

echo "deploying to $host"
"${ssh[@]}" 'mkdir -p /mnt/us/weatherdash'
put weatherdash.sh /mnt/us/weatherdash/weatherdash.sh
put weatherdash.conf /mnt/us/weatherdash/weatherdash.conf
put "Weather Dashboard.sh" "/mnt/us/documents/Weather Dashboard.sh"

if [ -n "$running" ]; then
  "${ssh[@]}" "kill -HUP $running"
  echo "reloaded the running loop (pid $running); log: /mnt/us/weatherdash/weatherdash.log"
elif [ "$start" = 1 ]; then
  "${ssh[@]}" 'sh "/mnt/us/documents/Weather Dashboard.sh"'
  echo "started; log: /mnt/us/weatherdash/weatherdash.log"
fi
