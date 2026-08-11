# Home automation stack for aida.
#
# This is native NixOS, not Home Assistant OS, so there is no "add-on store".
# The equivalent of an add-on here is another NixOS service wired to Home
# Assistant over an MQTT broker. mosquitto is the integration bus: peripheral
# services publish to it and Home Assistant auto-discovers their entities.
#
# The pattern for adding a future "add-on":
#   1. Enable the service below.
#   2. Point its MQTT client at mqtt://127.0.0.1:1883 with the shared `mqtt`
#      account (password via the mqtt-password* agenix secrets).
#   3. Turn on its Home Assistant / MQTT discovery so HA picks it up.
#   4. If it has a web UI, add a Caddy vhost (behind Authelia unless it ships a
#      mobile app / API that needs its own auth).
# No broker or HA changes are needed for a new add-on.
#
# Secrets (declared in hosts/aida.nix, recipients in secrets/secrets.nix):
#   mqtt-password      — plaintext MQTT password (mosquitto passwordFile)
#   mqtt-password-env  — ZIGBEE2MQTT_CONFIG_MQTT_PASSWORD=<same password>
#
# The Zigbee coordinator is an SMLIGHT SLZB-MR5U reached over the LAN, so
# zigbee2mqtt talks to it over TCP — no USB/serial device or udev rules.

{
  config,
  pkgs,
  lib,
  ...
}:

let
  # LAN address of the SLZB-MR5U Zigbee coordinator. 6638 is SMLIGHT's default
  # Zigbee TCP socket port; `ember` is the driver for its EFR32 radio.
  slzb = {
    host = "192.168.1.124";
    port = 6638;
  };

  # UI-editable HA config files: repo baseline keyed by runtime filename. HA's
  # editors write these and require a matching `!include` in the (nix-owned,
  # read-only) configuration.yaml to load them — see the seed/drift wiring below.
  haFiles = {
    "automations.yaml" = ../../hosts/aida/home-assistant/automations.yaml;
    "scenes.yaml" = ../../hosts/aida/home-assistant/scenes.yaml;
    "scripts.yaml" = ../../hosts/aida/home-assistant/scripts.yaml;
  };
in
{
  # mosquitto — the MQTT hub. Loopback only, no anonymous access.
  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "127.0.0.1";
        port = 1883;
        settings.allow_anonymous = false;
        users.mqtt = {
          passwordFile = config.age.secrets.mqtt-password.path;
          acl = [ "readwrite #" ];
        };
      }
    ];
  };

  # Home Assistant — its own auth (no Authelia forward_auth, which would break
  # the companion app and long-lived API tokens). Listens on loopback; Caddy
  # terminates TLS and forwards.
  services.home-assistant = {
    enable = true;
    extraComponents = [
      "analytics"
      "google_translate"
      "met"
      "radio_browser"
      "shopping_list"
      "isal" # faster websocket compression
      "mqtt" # broker connection is added via the onboarding UI
      "wiz" # WiZ lights auto-discovered on the LAN (packages pywizlight)
      "reolink" # Reolink doorbell/cameras (packages reolink-aio)
      "zwave_js" # Z-Wave; connects to the zwave-js server below (added via UI)
    ];
    config = {
      default_config = { };
      # Point the UI automation/scene/script editors at writable include files.
      # The module unquotes leading-bang strings, so these become real YAML
      # `!include` tags. Without them, the UI saves the file but HA never loads
      # it and "New automation setup" times out. The files themselves stay
      # HA-owned/writable and are backed up via the seed/drift wiring below.
      automation = "!include automations.yaml";
      scene = "!include scenes.yaml";
      script = "!include scripts.yaml";
      http = {
        server_host = "127.0.0.1";
        trusted_proxies = [ "127.0.0.1" ];
        use_x_forwarded_for = true;
      };
    };
  };

  # Seed each UI-editable include from the repo baseline only when ABSENT, in
  # home-assistant's own preStart (runs as `hass` after StateDirectory is set up
  # and before HA starts) — HA hard-fails on a missing `!include`, so seeding
  # must be guaranteed to complete first. Existing files are never overwritten,
  # so UI edits always win. `mkAfter` keeps this after the module's own preStart.
  systemd.services.home-assistant.preStart = lib.mkAfter (
    lib.concatStrings (
      lib.mapAttrsToList (name: src: ''
        if [ ! -e /var/lib/hass/${name} ]; then
          cp ${src} /var/lib/hass/${name}
          chmod u+w /var/lib/hass/${name}
        fi
      '') haFiles
    )
  );

  # Warn (never clobber) when a live HA include has drifted from the baseline,
  # on every switch. Capture drift with `ha-foldin aida`, then commit.
  system.activationScripts.homeAssistantDriftCheck.text = lib.concatStrings (
    lib.mapAttrsToList (name: src: ''
      live=/var/lib/hass/${name}
      if [ -e "$live" ] && ! ${pkgs.diffutils}/bin/diff -q ${src} "$live" >/dev/null 2>&1; then
        echo "warning: home-assistant ${name} has drifted from the nix baseline" \
             "(kept as-is; run 'ha-foldin aida' to capture it):" >&2
        ${pkgs.diffutils}/bin/diff ${src} "$live" >&2 || true
      fi
    '') haFiles
  );

  # Home Assistant's bluetooth integration (pulled in by default_config) needs a
  # running BlueZ stack to drive aida's onboard adapter over DBus; without it,
  # habluetooth only sees the raw hci0 device and fails to manage it.
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  # zigbee2mqtt — bridges the SLZB coordinator onto MQTT with HA discovery.
  services.zigbee2mqtt = {
    enable = true;
    settings = {
      homeassistant.enabled = true;
      frontend = {
        enabled = true;
        host = "127.0.0.1";
        port = 8080;
      };
      mqtt = {
        server = "mqtt://127.0.0.1:1883";
        user = "mqtt";
        # password comes from the EnvironmentFile below, so it stays out of the
        # world-readable generated configuration.yaml in the nix store.
      };
      serial = {
        port = "tcp://${slzb.host}:${toString slzb.port}";
        adapter = "ember";
        # Values recommended by the SLZB-MR5U's own z2m config generator.
        # baudrate is inert over a TCP coordinator but kept to match the snippet.
        baudrate = 115200;
        disable_led = false;
      };
      advanced = {
        # Drive the SLZB radio at its max output power (dBm) for range.
        transmit_power = 20;
      };
    };
  };

  systemd.services.zigbee2mqtt.serviceConfig.EnvironmentFile =
    config.age.secrets.mqtt-password-env.path;

  # --- Declarative baseline for the mutable Z2M config (the clickops surface) ---
  #
  # The module regenerates only configuration.yaml from `settings` above;
  # devices.yaml (friendly-name renames, per-device options) and groups.yaml
  # (group definitions) are Z2M-owned and survive restarts. That makes them the
  # UI-editable surface — but it also means they live only on aida's disk unless
  # captured here.
  #
  # To make a deploy a restore point without clobbering UI edits, following the
  # same seed-then-warn philosophy as home/modules/drift-check.nix:
  #   * seed each file from the repo baseline only when it is ABSENT (fresh box /
  #     disaster recovery), via tmpfiles `C` (copy-if-missing);
  #   * on every switch, diff the live file against the baseline and WARN on
  #     drift — never overwrite the running copy.
  # Capture drift back into the repo with `z2m-foldin aida` (see shell-personal),
  # then commit: the commit is the backup.
  #
  # NOTE: this captures naming/grouping config only. Device pairings live in
  # database.db / coordinator_backup.json (binary) and are out of scope here.
  systemd.tmpfiles.rules =
    let
      seed = name: src: "C /var/lib/zigbee2mqtt/${name} 0600 zigbee2mqtt zigbee2mqtt - ${src}";
    in
    [
      "d /var/lib/zigbee2mqtt 0700 zigbee2mqtt zigbee2mqtt -"
      (seed "devices.yaml" ../../hosts/aida/zigbee2mqtt/devices.yaml)
      (seed "groups.yaml" ../../hosts/aida/zigbee2mqtt/groups.yaml)
    ];

  system.activationScripts.zigbee2mqttDriftCheck.text =
    let
      baseline = {
        "devices.yaml" = ../../hosts/aida/zigbee2mqtt/devices.yaml;
        "groups.yaml" = ../../hosts/aida/zigbee2mqtt/groups.yaml;
      };
      check = name: src: ''
        live=/var/lib/zigbee2mqtt/${name}
        if [ -e "$live" ] && ! ${pkgs.diffutils}/bin/diff -q ${src} "$live" >/dev/null 2>&1; then
          echo "warning: zigbee2mqtt ${name} has drifted from the nix baseline" \
               "(kept as-is; run 'z2m-foldin aida' to capture it):" >&2
          ${pkgs.diffutils}/bin/diff ${src} "$live" >&2 || true
        fi
      '';
    in
    lib.concatStrings (lib.mapAttrsToList check baseline);

  # zwave-js — Z-Wave JS server driving the Nabu Casa ZWA-2 (USB, enumerates as
  # a CDC-ACM device). HA's `zwave_js` integration (added via the onboarding UI)
  # connects to it over the websocket server on 127.0.0.1:3002 (the zwave-js
  # default 3000 is taken by AdGuardHome's UI). The by-id path is stable across
  # reboots, unlike /dev/ttyACM0. The four S0/S2 security keys come from the
  # agenix secret and are merged into the driver config at runtime via systemd
  # LoadCredential, so they never land in the world-readable nix store.
  services.zwave-js = {
    enable = true;
    port = 3002;
    serialPort = "/dev/serial/by-id/usb-Nabu_Casa_ZWA-2_1CDBD4AD2A04-if00";
    secretsConfigFile = config.age.secrets.zwave-js-keys.path;
  };

  # Reverse-proxy vhosts (merge with the services.caddy block in aida.nix).
  services.caddy.virtualHosts = {
    # Home Assistant on the bare home.malloc47.com; HA handles its own auth.
    "home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:8123
      '';
    };

    # zigbee2mqtt admin UI behind Authelia — it has no mobile app, so SSO is fine.
    "zigbee.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        handle {
          forward_auth http://127.0.0.1:9091 {
            uri /api/authz/forward-auth
            copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
          }
          reverse_proxy http://127.0.0.1:8080
        }
      '';
    };
  };
}
