# ESPHome devices (aida)

Device firmware configs live in the repo, not in the Device Builder
(esphome.home.malloc47.com). The wiring is in `nixos/modules/home-automation.nix`.

| What | Where |
|---|---|
| Device configs | `hosts/aida/esphome/<device>.yaml` (each file is one device) |
| Shared boilerplate (Wi-Fi, fallback AP, API, OTA, logger, project stamp) | `hosts/aida/esphome/common/base.yaml`, pulled in via `packages:` |
| Secrets (`secrets.yaml`) | `secrets/esphome-secrets.yaml.age` (agenix; aida + malloc47) |
| On aida | `/var/lib/esphome/<device>.yaml`, `common/` → root-owned copies installed on switch (read-only to the Builder); `secrets.yaml` → symlink to `/run/agenix/esphome-secrets` |
| Flash unit | `esphome-deploy@<device>.service` (oneshot; compile + OTA) |
| Workstation helper | `esphome-deploy [-H host] <device>... \| --all` |

The Builder sees the configs through read-only bind mounts, so saving in its
editor fails instead of drifting; if a config is replaced anyway, the next
switch warns, prints the discarded diff and reinstalls the repo copy. (They
are copies, not store symlinks, because the Builder rejects configs that
resolve outside its config dir.) The Builder's secrets editor won't open
`secrets.yaml` for the same reason, which is intended.
The Builder is still useful for online status, live logs and first-time
browser (Web Serial) flashing. Its own git "version history" is turned off
(a UI preference in `.device-builder-preferences.json`, not Nix-managed); the
repo is the history.

## Changing a device

```sh
$EDITOR hosts/aida/esphome/e1001-clock.yaml
git add -A hosts/aida/esphome      # new files must be staged for the flake
nixos-deploy aida                  # installs the YAML on aida; does not flash
esphome-deploy e1001-clock         # compiles and OTA-flashes, streaming the log
```

`esphome-deploy` refuses to run if aida's copy differs from the working tree
(forgot `nixos-deploy`). Flashing is deliberately not part of a switch: it
takes minutes and devices may be asleep or offline.

Each build is stamped with `fw_rev`: a hash of the device YAML, `common/` and
the ESPHome version (not secrets). It appears as the device's firmware version
in HA (`malloc47.<device>`) and in the unit's first log line, so a device whose
HA version does not match its unit's `fw_rev` is running stale firmware.

## Bumping ESPHome

Bump the `esphome` package in `home-automation.nix`, `nixos-deploy aida`, then
`esphome-deploy --all`. Every device's `fw_rev` changes, so until each is
reflashed HA shows the old revision.

## Adding a device

1. Write `hosts/aida/esphome/<name>.yaml`: `substitutions:` for `name`,
   `friendly_name`, `api_key`, `ota_password`, `ap_password` (the last three
   as `!secret <name_with_underscores>_…`), then `packages: base: !include
   common/base.yaml`, then the device-specific config. Copy `e1001-clock.yaml`.
2. Add the secrets: `cd secrets && agenix -e esphome-secrets.yaml.age`. Generate
   the API key with `openssl rand -base64 32`; the passwords can be anything.
3. `nixos-deploy aida`.
4. First flash has to be over serial. Easiest: in the Builder, *Install →
   Manual download* (it compiles the repo config), then flash the factory image
   from a workstation with a USB cable at https://web.esphome.io. (Plugging the
   device into aida doesn't work as-is: the services only allow
   `/dev/ttyS*`/`/dev/ttyUSB*`, and native-USB ESP32-S3/C3 boards enumerate as
   `ttyACM`.) After that, `esphome-deploy <name>` works over OTA.
5. Adopt it in HA (Settings → Devices → discovered ESPHome device) using the
   API key. HA config entries are still UI-managed.

## Removing a device

Delete its YAML and secrets entries, `nixos-deploy aida` (its installed copy is
removed), then delete the device in HA.

## Rotating secrets

- **API key:** edit the secret, deploy, `esphome-deploy <name>`, then
  reconfigure the device in HA with the new key.
- **OTA password:** the upload authenticates with the password in the config
  being flashed, so changing the secret alone locks you out of OTA. Use
  ESPHome's two-step workaround: keep the old password, give the `ota:` entry
  an `id`, add an `on_boot` lambda `id(<ota_id>).set_auth_password("<new>");`
  and flash; then put the new password in the secret, drop the lambda and
  flash again. (Or just reflash over serial.)
- **Wi-Fi:** edit `wifi_ssid`/`wifi_password`, deploy, `esphome-deploy --all`
  *before* changing the network, or the devices fall back to their AP.
