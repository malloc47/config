# Home Assistant operations (aida)

Ongoing/operational guide for the home-automation stack on `aida`. The services
themselves are declarative in `nixos/modules/home-automation.nix`; this doc
covers the manual, UI-side tasks that aren't captured in Nix.

For first-time bring-up (secrets, onboarding, connecting MQTT) see
`docs/bootstrap/aida.md` step 9.

## The stack

| Piece | Where |
|---|---|
| Home Assistant | `https://home.malloc47.com` (its own login) |
| zigbee2mqtt frontend | `https://zigbee.home.malloc47.com` (behind Authelia) |
| MQTT broker (mosquitto) | `127.0.0.1:1883`, user `mqtt`, loopback only |
| Zigbee coordinator | SMLIGHT SLZB-MR5U over LAN — `tcp://192.168.1.124:6638`, `ember` adapter |

**Healthy baseline (no devices yet):** z2m connects to the broker
(`"mqtt":{"connected":true}` in its `bridge/health` messages) and to the
coordinator (`zh:ember` NCP/ASH counter lines in the log). Once you see both,
the plumbing is ready and pairing will work. Quick check from anywhere:

```bash
ssh aida "journalctl -u zigbee2mqtt -b -o cat | grep -i 'bridge/health' | tail -1"
# look for "mqtt":{"connected":true} and "devices":{...}
```

## Pairing your first Zigbee device

Preconditions (one-time, done during bootstrap): HA onboarded and the **MQTT**
integration added (Settings → Devices & Services shows MQTT with no error).

1. **Confirm z2m is online.** Open `https://zigbee.home.malloc47.com`; the
   header should show it connected. To watch live during pairing, tail the log:
   ```bash
   ssh aida "journalctl -u zigbee2mqtt -f -o cat"
   ```
2. **Enable permit join (time-limited).** In the z2m frontend click
   **Permit join** — it starts a countdown (default ~254s). For the very first
   device, joining via the coordinator ("All") is fine.
3. **Put the device into pairing mode.** Device-specific — usually hold a button
   5–10s until an LED blinks, or follow the reset steps on the z2m
   "Supported devices" page for that model. Keep it within a couple meters of the
   coordinator for the first join.
4. **Wait for the interview.** The frontend shows the new device and runs an
   "interview"; wait until it resolves to a real vendor/model (not
   "Unsupported"). Battery devices may need a button press to stay awake.
5. **Confirm in Home Assistant.** Within seconds of a successful interview, MQTT
   discovery creates the device under Settings → Devices & Services → **MQTT**,
   with its entities populated automatically — no YAML.
6. **Name & place it.** Set a `friendly_name` in z2m and/or assign an Area in HA.
7. **Disable permit join.** Click **Disable join** (or let the countdown lapse) —
   leaving the network open is a security risk.

## Troubleshooting

- **Won't pair:** re-trigger pairing mode, move closer to the coordinator, and
  make sure permit join is still counting down.
- **Interview stalls / "Unsupported":** wake battery devices (press a button) so
  z2m can finish the interview; a second attempt often succeeds.
- **Coordinator offline:** `ssh aida systemctl status zigbee2mqtt`, and verify
  the SLZB at `192.168.1.124` is reachable (ping / its own web UI). If its LAN IP
  changes, update `slzb.host` in `nixos/modules/home-automation.nix` and redeploy.
- **Range:** mains-powered Zigbee devices act as routers — add one near the edge
  of coverage before placing distant battery sensors.
- **Firmware:** device OTA updates are handled in z2m's OTA tab; the SLZB
  coordinator's own firmware is updated through its web UI, not z2m.

## Adding another MQTT "add-on" (the pattern)

To bring in another bridge (esphome, etc.), edit
`nixos/modules/home-automation.nix`: enable the service, point its MQTT client at
`mqtt://127.0.0.1:1883` with the `mqtt` account, enable its HA/MQTT discovery,
and add a Caddy vhost (behind Authelia unless it needs its own auth). HA
auto-discovers it — no broker or HA changes required.
