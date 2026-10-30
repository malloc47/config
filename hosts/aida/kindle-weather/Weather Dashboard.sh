#!/bin/sh
# Name: Weather Dashboard
# Author: malloc47
# Icon:
# Library launcher for the wall weather dashboard (docs/kindle-weather.md),
# installed into /mnt/us/documents by deploy.sh. Detaches the loop into its
# own session so it outlives the Kindle UI it stops; the pidfile (shared with
# weatherdash.sh) keeps a second launch from starting another loop. Press the
# power button once to refresh, twice to return to the Kindle UI.
start-stop-daemon -S -b -q -m -p /tmp/weatherdash.pid -x /bin/sh -- /mnt/us/weatherdash/weatherdash.sh
