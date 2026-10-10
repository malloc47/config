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
# The Zigbee coordinator is a Sonoff Zigbee 3.0 USB Dongle Plus MG24
# (EFR32MG24, EmberZNet) on USB — see services.zigbee2mqtt.serial below.
# (Replaced the LAN SLZB-MR5U, which failed with recurring radio faults.)

{
  config,
  pkgs,
  pkgs-unstable,
  lib,
  ...
}:

let
  # UI-editable HA config files: repo baseline keyed by runtime filename. HA's
  # editors write these and require a matching `!include` in the (nix-owned,
  # read-only) configuration.yaml to load them — see the seed/drift wiring below.
  haFiles = {
    "automations.yaml" = ../../hosts/aida/home-assistant/automations.yaml;
    "scenes.yaml" = ../../hosts/aida/home-assistant/scenes.yaml;
    "scripts.yaml" = ../../hosts/aida/home-assistant/scripts.yaml;
  };

  # ESPHome device configs: every hosts/aida/esphome/<device>.yaml is a device,
  # and common/ holds the packages they `!include`. See the esphome wiring below.
  esphomeDir = ../../hosts/aida/esphome;
  esphomeCommon = esphomeDir + "/common";
  esphomeCommonFiles = builtins.attrNames (builtins.readDir esphomeCommon);
  esphomeDevices =
    lib.mapAttrs' (f: _: lib.nameValuePair (lib.removeSuffix ".yaml" f) (esphomeDir + "/${f}"))
      (
        lib.filterAttrs (f: type: type == "regular" && lib.hasSuffix ".yaml" f) (
          builtins.readDir esphomeDir
        )
      );
  # Firmware revision stamped into each device (esphome.project.version, shown as
  # the firmware version in HA): a hash of everything that goes into its build
  # except secrets, so a device whose HA version differs from this is stale.
  esphomeFwRev =
    file:
    builtins.substring 0 8 (
      builtins.hashString "sha256" (
        lib.concatStrings (
          [
            esphome.version
            (builtins.readFile file)
          ]
          ++ map (f: builtins.readFile (esphomeCommon + "/${f}")) esphomeCommonFiles
        )
      )
    );
  # Body of esphome-deploy@<device>.service: compile and OTA-flash one device.
  esphomeDeploy = pkgs.writeShellScript "esphome-deploy" ''
    case "$1" in
    ${
      lib.concatStrings (
        lib.mapAttrsToList (name: file: "  ${name}) rev=${esphomeFwRev file} ;;\n") esphomeDevices
      )
    }  *) echo "esphome-deploy: no device '$1' in hosts/aida/esphome" >&2; exit 1 ;;
    esac
    echo "esphome-deploy: flashing $1 (fw_rev $rev, ESPHome ${esphome.version})"
    exec esphome -s fw_rev "$rev" run --no-logs --device OTA "/var/lib/esphome/$1.yaml"
  '';

  # ESPHome 2026.10 beta, for epaper_spi's `full_update_next` action (PR
  # esphome/esphome#19213). nixpkgs tops out at 2026.8.0 (unstable) / 2026.5.1
  # (26.05), so bump unstable's package. 2026.9+ adds `ninja` and widens the
  # wheel pin; other deps are a few patch/minor releases behind upstream's pins
  # in unstable, which pythonRelaxDeps tolerates. The skipped tests only fail in
  # the build sandbox (no network, $HOME or serial port) or exercise dev
  # tooling. Move to 2026.10.0 once released, and drop this once
  # nixpkgs-unstable's esphome catches up.
  esphome = pkgs-unstable.esphome.overridePythonAttrs (old: rec {
    version = "2026.10.0b2";
    src = pkgs-unstable.fetchFromGitHub {
      owner = "esphome";
      repo = "esphome";
      tag = version;
      hash = "sha256-/OYRbGA79JZi5V3KlAaCq5gC2gHp9sWnvLMYooXcEWU=";
    };
    postPatch = builtins.replaceStrings [ "<0.48" ] [ "<0.49" ] old.postPatch;
    dependencies = old.dependencies ++ [ pkgs-unstable.python3Packages.ninja ];
    disabledTests = old.disabledTests ++ [
      "test_make_registry_client_skips_private_package_probe"
      "test_patch_registry_private_packages_skips_account_probe"
      "test_make_registry_client_creates_http_cache_dir"
      # Download ESP-IDF archives / probe a real serial port / run a helper
      # script without esphome on PYTHONPATH (the wrapper adds it at runtime).
      "test_run_reconfigure_flip_into_skip_mode_cleans_up"
      "test_run_reconfigure_skip_steady_state_cleans_nothing"
      "test_upload_using_esptool_arduino_toolchain"
      "test_install_tool_archives_extracts_pending_in_parallel"
      # Simulates an Apple-silicon host.
      "test_pch_script_gcc10_wrapper_on_apple_silicon"
    ];
    disabledTestPaths = old.disabledTestPaths ++ [
      "tests/component_tests/lvgl/test_list_outside_block.py"
      # Dev-tooling tests (dependency-pin sync, protobuf codegen) that need
      # yamlrocks / a newer aioesphomeapi than unstable has; not used at runtime.
      "tests/script/test_sync_dependency_versions.py"
      "tests/unit_tests/components/api/test_api_protobuf_generator.py"
    ];
  });
  # ESPHome removed its built-in dashboard in 2026.7; the Device Builder is now a
  # separate app that drives the `esphome` CLI (wired to the package above).
  # Bumped from unstable's 1.14.9 to the version ESPHome's own container pins
  # for this release (docker/Dockerfile), along with the frontend it pins.
  esphome-device-builder-frontend =
    pkgs-unstable.esphome-device-builder.frontend.overridePythonAttrs
      (old: rec {
        version = "0.1.366";
        src = pkgs-unstable.fetchFromGitHub {
          owner = "esphome";
          repo = "device-builder-frontend";
          tag = version;
          hash = "sha256-GbiWj4yVYCVRu4OD6MSetQJaPXlPfdS+b8OnorxVPTE=";
        };
        pnpmDeps = pkgs-unstable.fetchPnpmDeps {
          inherit (old) pname;
          inherit version src;
          pnpm = pkgs-unstable.pnpm_11;
          fetcherVersion = 4;
          hash = "sha256-84zvZEy4ptVLy/B1WbIVh/hcdi4e/xkXEfd5uldR2wg=";
        };
      });
  esphome-device-builder =
    (pkgs-unstable.esphome-device-builder.override { inherit esphome; }).overridePythonAttrs
      (old: rec {
        version = "1.22.0";
        src = pkgs-unstable.fetchFromGitHub {
          owner = "esphome";
          repo = "device-builder";
          tag = version;
          hash = "sha256-CVEet29wYX/0TzVIRkaNNdKNg03YTk4ERDglJqjaBVA=";
        };
        dependencies = map (
          d:
          if (d.pname or "") == "esphome-device-builder-frontend" then esphome-device-builder-frontend else d
        ) old.dependencies;
        # 1.22's MCP tests import jsonschema (a test-only extra).
        nativeCheckInputs = old.nativeCheckInputs ++ [ pkgs-unstable.python3Packages.jsonschema ];
      });
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
    # Custom (non-core) integrations from nixpkgs' home-assistant-custom-components.
    # emporia_vue (magico13) covers the Emporia Vue energy monitors AND the Emporia
    # EV charger (charge on/off + charge-rate control). Emporia has no local API, so
    # it is cloud-only: added via the UI config flow with the Emporia app credentials,
    # which land in HA's own state dir (not the world-readable nix store). Packages
    # pyemvue. Without this, "Emporia Vue" never appears in Add Integration.
    customComponents = [
      pkgs.home-assistant-custom-components.emporia_vue
      # yoto_ha (cdnninja) — community integration for the kids' Yoto players:
      # media_player controls plus battery / now-playing / card-slot sensors, over
      # the Yoto cloud (config-flow login, cloud_polling; packages yoto-api). Added
      # via the UI. HA core gained a native `yoto` integration in 2026.6; once
      # nixpkgs advances past it this can become extraComponents = [ "yoto" ] and
      # drop the custom package.
      pkgs.home-assistant-custom-components.yoto_ha
      # toniebox (git4sim/HA-Toniebox) — unofficial integration for the kids'
      # Toniebox over the Tonie Cloud (config-flow login, cloud_push over MQTT;
      # packages paho-mqtt). Not in nixpkgs, so packaged in-repo at
      # pkgs/home-assistant-toniebox and exposed via overlays.default.
      pkgs.home-assistant-toniebox
      # gtfs2 (vingerha/gtfs2) — packaged in-repo (pkgs/home-assistant-gtfs2).
      # Real-time transit arrivals for the Westchester Bee-Line, which the MTA
      # integration does NOT cover (separate agency). Loads the full GTFS
      # timetable into SQLite and overlays GTFS-RT, so a stop shows its SCHEDULED
      # arrival when there is no live data and upgrades to the RT prediction when
      # a bus is in range. (Replaced the realtime-ONLY bcpearce gtfs-realtime,
      # which read "Unknown" whenever the RT feed had no entry for a stop.) Set up
      # via the UI config flow (agency -> route -> origin -> destination), then
      # enable Real-time in the integration's options with the trip-updates URL
      # (no API key; accept the county ToS):
      #   static: https://westchester-win.gmv.com:8443/repository/gtfs-public/GTFS_GMV_WCDOT.zip
      #   rt:     https://westchester.gmv.com/gtfsrtapi/api/tripupdates
      pkgs.home-assistant-gtfs2
      # adaptive_lighting (basnijholt) — circadian control: intercepts
      # light.turn_on and adapts brightness + color temp (optionally RGB) by sun
      # position, per-area config entries, with sleep mode and take-over-control
      # detection. The de-facto standard (a rewrite of the older circadian_lighting;
      # more capable than core `flux`). Added/configured via the UI config flow —
      # just enabling the component controls no lights until a light group is set
      # up. Will own color temp for its lights, so retire manual color-temp
      # automations on those lights when adopting it.
      pkgs.home-assistant-custom-components.adaptive_lighting
    ];
    # Custom Lovelace (frontend) cards, registered as dashboard resources.
    # Used by the LD2410 mmWave tuning dashboard: with the sensor's engineering
    # mode on, plot each distance gate's live move/still energy so the per-gate
    # sensitivities can be dialed in visually. plotly-chart-card gives the
    # radar-like energy-vs-distance bar chart; apexcharts-card is kept for
    # general time-series graphs. Card YAML is added on a dashboard via the UI.
    customLovelaceModules = [
      pkgs.home-assistant-custom-lovelace-modules.apexcharts-card
      pkgs.home-assistant-custom-lovelace-modules.plotly-chart-card
      # flex-table-card — packaged in-repo (pkgs/home-assistant-flex-table-card).
      # Renders gtfs2's next_departures* list attributes as a table of the next N
      # upcoming trips (there is no dedicated gtfs2 card). Added on a dashboard as
      # `custom:flex-table-card` via the UI.
      pkgs.home-assistant-flex-table-card
    ];
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
      "esphome" # ESPHome devices added via UI (packages aioesphomeapi)
      # Matter controller integration — added via the UI, connects to the
      # matter-server below on ws://127.0.0.1:5580. This is HA commissioning and
      # controlling real Matter devices (inbound), distinct from the matter-hub
      # that exposes HA to Google (outbound). See services.matter-server below.
      "matter"
      # Send commands/broadcasts to Google Assistant from HA (packages
      # gassist-text). OAuth is set up in the UI via Application Credentials;
      # without this component the config flow 500s with "Invalid handler".
      "google_assistant_sdk"
      # MTA New York City Transit — real-time NYC subway/bus arrivals via the
      # MTA's GTFS-RT feeds (core since 2026.3; packages py-nymta). Added via the
      # UI config flow: subway needs no key; bus tracking needs an MTA Bus Time
      # API key (from bustime.mta.info) entered in the flow. Creates arrival
      # sensors per stop; cloud_polling (~30s default).
      "mta"
      # Bosch/Siemens Home Connect appliances (dishwasher, etc.) via the Home
      # Connect cloud API (packages aiohomeconnect). Cloud OAuth: register an app
      # at developer.home-connect.com (Authorization Code Grant, redirect URI
      # https://my.home-assistant.io/redirect/oauth), then add its client ID/secret
      # under Settings -> Devices & Services -> Application Credentials before
      # adding the integration via the UI. Without this component the config flow
      # 500s with "Invalid handler".
      "home_connect"
      # LG ThinQ appliances via the cloud API (packages thinqconnect).
      # Added via the UI config flow; credentials stay in HA's state directory.
      "lg_thinq"
      # Integrations for devices already auto-discovered on the LAN. Without the
      # component bundled, default_config's discovery still finds the device but
      # its config-flow load fails with "No module named '<dep>'" (harmless log
      # noise); enabling each bundles the dep so the device is actually usable and
      # the errors clear. All are added/confirmed via the UI config flow.
      "cast" # Google/Nest speakers (packages pychromecast)
      "smlight" # SLZB-MR5U Zigbee coordinator diagnostics (packages pysmlight)
      "shelly" # Shelly devices (packages aioshelly)
      "androidtv_remote" # Google/Android TV remote (packages androidtvremote2)
      "ipp" # network printer via IPP (packages pyipp)
      "brother" # Brother network printer (packages brother)
      "thread" # Thread border router mgmt (packages python-otbr-api)
    ];
    config = {
      default_config = { };
      # Home/installation display name (Settings -> System -> General -> Name).
      homeassistant.name = "Berry Patch";
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
        # Sonoff Zigbee 3.0 USB Dongle Plus MG24 (EFR32MG24, EmberZNet) plugged
        # into aida directly, replacing the failed LAN SLZB-MR5U. Same `ember`
        # stack, so z2m restores the existing network from coordinator_backup.json
        # (no re-pairing).
        port = "/dev/serial/by-id/usb-SONOFF_SONOFF_Dongle_Plus_MG24_9ecc6e632b8cf0119f9e2eb9d9065118-if00-port0";
        adapter = "ember";
        baudrate = 115200;
        disable_led = false;
      };
      advanced = {
        # Drive the SLZB radio at its max output power (dBm) for range.
        transmit_power = 20;
      };
    };
  };

  systemd.services.zigbee2mqtt = {
    serviceConfig = {
      EnvironmentFile = config.age.secrets.mqtt-password-env.path;
      # z2m runs with DevicePolicy=closed; grant access to the USB Zigbee dongle
      # (the module already adds the dialout group). Append to keep its entries.
      DeviceAllow = lib.mkAfter [
        "char-ttyUSB rw"
        "char-ttyACM rw"
      ];
      # The coordinator being briefly unavailable (a reboot, a
      # transient wedge) makes z2m exit on startup. Treat that as transient:
      # keep restarting forever on a fixed interval rather than crashlooping
      # fast. z2m is load-bearing, so we never want it to stop trying.
      Restart = lib.mkForce "always";
      RestartSec = lib.mkForce 30;
    };
    # Disable systemd's start-rate limiter (default 5 starts / 10s) so repeated
    # failed starts while the coordinator is down never trip 'start-limit-hit',
    # which would leave z2m permanently dead until a manual reset-failed. Now it
    # waits indefinitely for the SLZB-MR5U to come back. Trade-off: a genuine
    # misconfig also loops instead of failing fast — the gatus watchdog surfaces
    # that.
    startLimitIntervalSec = 0;
  };

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

  # matter-server — HA's INBOUND Matter controller (python-matter-server). This
  # is the opposite direction from home-assistant-matter-hub below: the hub
  # EXPOSES HA's Zigbee/Z-Wave to Google as a Matter bridge (outbound), while
  # this SERVER lets HA commission and control real Matter devices itself
  # (inbound). HA's `matter` integration (added via the UI, see extraComponents)
  # connects over the websocket on 127.0.0.1:5580 — loopback only, so
  # openFirewall stays off; only HA needs to reach it.
  #
  # Purpose here: adopt the Matter-over-Wi-Fi bulbs that were previously only in
  # Google Home. They are Wi-Fi, not Thread, so there is NO Thread border router
  # / OTBR in play — commissioning is plain LAN + mDNS (the UDP 5353 rule below,
  # opened for the hub, also covers controller discovery). Migration is a clean
  # break: factory-reset each bulb (which decommissions it from Google's fabric),
  # then commission it fresh into HA; afterwards add it to a matter-hub bridge so
  # Google stays a pure voice layer. Matter needs IPv6 + unfiltered LAN multicast
  # — see the hub notes below; do not disable either.
  services.matter-server = {
    enable = true;
    # Pin CHIP's link-local IPv6 traffic to the wired LAN (eno1). aida has a
    # DOWN, unused Wi-Fi iface (wlp1s0) that CHIP otherwise latches onto ("Got
    # WiFi interface: wlp1s0"); since the LAN has no global IPv6, Matter
    # commissioning/operational traffic rides link-local IPv6, which is
    # interface-scoped — sending it out wlp1s0 makes PASE time out ("Secure
    # Pairing Failed"). Not enabling --bluetooth-adapter: aida's BT adapter is
    # owned by HA's bluetooth integration, so commission Wi-Fi Matter devices
    # over the network (or via the HA phone app's BLE), not server-side BLE.
    extraArgs.primary-interface = "eno1";
  };

  # home-assistant-matter-hub — exposes HA entities to Matter controllers so the
  # legacy Google Home/Nest speakers (already Matter hubs) can voice-control them.
  # It is not an "add-on" on the MQTT bus: it talks to HA directly over the
  # websocket API using a long-lived access token. The token is delivered as a
  # systemd credential (module reads accessTokenFile -> HAMH_HOME_ASSISTANT_ACCESS_TOKEN),
  # so it never lands in the world-readable nix store.
  #
  # nixpkgs packages the actively-maintained RiDDiX fork (upstream t0bst4r was
  # archived Jan 2026). The bridges and their entity filters are created in the
  # web UI (port 8482, proxied below) and persist as mutable state under
  # /var/lib/home-assistant-matter-hub — that is the clickops surface, analogous
  # to z2m's devices.yaml; Nix owns only the service wiring, not the bridge list.
  #
  # Commissioning needs the LAN reachable on UDP/TCP 5540 (openFirewall) AND
  # mDNS on UDP 5353 (matter.js runs its own responder, so no avahi) — the
  # module's openFirewall does NOT cover 5353, so it is opened explicitly below.
  # Matter also requires IPv6 and unfiltered multicast on the LAN; do not disable
  # either. aida is on the same L2 as the Google devices, so discovery works.
  services.home-assistant-matter-hub = {
    enable = true;
    openFirewall = true; # UDP/TCP 5540 (Matter commissioning)
    accessTokenFile = config.age.secrets.home-assistant-matter-hub-token.path;
    settings = {
      homeAssistantUrl = "http://127.0.0.1:8123";
      httpPort = 8482;
    };
  };

  # ESPHome Device Builder — the dashboard for writing, compiling and OTA-flashing
  # ESPHome firmware (the HA OS "ESPHome" add-on). Separate from HA's `esphome`
  # integration (see extraComponents), which talks to the devices directly over
  # their native API; this only builds and flashes. The toolchains and build
  # caches live under /var/lib/esphome. Loopback only; the web UI is proxied
  # below. Device online status comes from mDNS (UDP 5353, opened below).
  #
  # The repo, not the Device Builder, owns the device configs: each device YAML
  # (and common/) is a root-owned copy the Builder sees through a read-only bind
  # mount, and secrets.yaml a symlink to the agenix secret, so saving in the
  # Builder's editor fails rather than drifting.
  # The Builder stays useful for status, logs and first-time browser flashing.
  # Flashing is a separate step from a switch (it takes minutes and devices may
  # be offline): `esphome-deploy <device>` from a workstation starts
  # esphome-deploy@<device>.service below. See docs/esphome.md.
  #
  # The 26.05 module still launches the pre-2026.7 `esphome dashboard`, so keep
  # it for the user, state dir and sandboxing but start the Device Builder
  # instead (nixpkgs#550245 converts the module upstream).
  #
  # Since 2026.9, ESP32 builds default to ESPHome's native ESP-IDF toolchain
  # rather than PlatformIO (which nixpkgs wraps in an FHS env). It downloads
  # generic-Linux cmake/gcc binaries into /var/lib/esphome/.cache, which NixOS
  # cannot run, so give this service alone a nix-ld loader at the standard
  # /lib64 path (a private bind mount, not system-wide programs.nix-ld) plus
  # the libraries those tools link against.
  services.esphome = {
    enable = true;
    package = esphome;
    address = "127.0.0.1";
    port = 6052;
    environment = {
      NIX_LD = pkgs.stdenv.cc.bintools.dynamicLinker;
      NIX_LD_LIBRARY_PATH = lib.makeLibraryPath (
        with pkgs;
        [
          stdenv.cc.cc
          zlib
          zstd
          libusb1
          systemd # libudev
          ncurses
          expat
          bzip2
          xz
          openssl
          libffi
        ]
      );
    };
  };
  systemd.services.esphome = {
    description = lib.mkForce "ESPHome Device Builder";
    # nixpkgs' ninja spawns build commands via `sh` from PATH, not /bin/sh.
    path = [ pkgs.bash ];
    serviceConfig = {
      ExecStart = lib.mkForce (
        lib.escapeShellArgs [
          (lib.getExe esphome-device-builder)
          "--host"
          config.services.esphome.address
          "--port"
          (toString config.services.esphome.port)
          # The remote-build peer listener otherwise binds 0.0.0.0:6055, which
          # is reachable over the tailnet; nothing offloads builds here.
          "--remote-build-host"
          "127.0.0.1"
          "/var/lib/esphome"
        ]
      );
      BindReadOnlyPaths = [ "${pkgs.nix-ld}/libexec/nix-ld:/lib64/ld-linux-x86-64.so.2" ];
      # The repo owns these (installed by the esphomeConfigs activation script
      # below). The Builder saves via write-to-temp + rename, which would replace
      # a merely root-owned file in its own state dir; a read-only bind mount
      # makes the save fail instead. `-`: skip any not yet installed.
      ReadOnlyPaths = [
        "-/var/lib/esphome/common"
      ]
      ++ map (n: "-/var/lib/esphome/${n}.yaml") (builtins.attrNames esphomeDevices);
    };
  };

  # Compile + OTA-flash one device from its repo config, in the same toolchain
  # environment and sandbox as the Device Builder (which shares the build cache).
  systemd.services."esphome-deploy@" =
    let
      builder = config.systemd.services.esphome;
    in
    {
      description = "Compile and OTA-flash ESPHome device %i";
      inherit (builder) path;
      # PATH comes from `path`; copying it too would define it twice.
      environment = removeAttrs builder.environment [ "PATH" ];
      serviceConfig =
        removeAttrs builder.serviceConfig [
          "ExecStart"
          "Restart"
        ]
        // {
          Type = "oneshot";
          ExecStart = "${esphomeDeploy} %i";
          TimeoutStartSec = "30min";
        };
    };

  # secrets.yaml can stay a symlink (only the Builder's secrets editor resolves
  # it, and that refusing is fine); the device configs cannot — see below.
  systemd.tmpfiles.settings."10-esphome" = {
    "/var/lib/esphome".d = {
      user = "esphome";
      group = "esphome";
      mode = "0750";
    };
    "/var/lib/esphome/secrets.yaml"."L+".argument = config.age.secrets.esphome-secrets.path;
  };

  # Install the repo's device configs and common/ into /var/lib/esphome as
  # root-owned copies. They can't be store symlinks: the Builder rejects any
  # config whose resolved path leaves its config dir ("Invalid configuration
  # filename"), which breaks its status and logs pages too. Files are rewritten
  # in place (same inode) so the Builder's read-only bind mounts of them
  # (ReadOnlyPaths above) see updates without a restart. A file not owned by
  # root was replaced outside the repo: warn and show what is discarded. Configs
  # of devices dropped from the repo are removed; ones created in the Builder
  # are left alone with a warning.
  system.activationScripts.esphomeConfigs = {
    deps = [ "users" ];
    text =
      let
        diff = "${pkgs.diffutils}/bin/diff";
      in
      ''
        esphomeInstall() {
          if [ -L "$2" ]; then rm -f "$2"; fi
          if [ -e "$2" ] && [ "$(stat -c %U "$2")" != root ]; then
            echo "warning: esphome $2 was modified outside the repo and is being replaced; discarding:" >&2
            ${diff} "$1" "$2" >&2 || true
            rm -f "$2"
          fi
          cat "$1" > "$2"
          chown root:esphome "$2"
          chmod 0640 "$2"
        }
        install -d -o esphome -g esphome -m 0750 /var/lib/esphome
        for live in /var/lib/esphome/*.yaml; do
          [ -e "$live" ] || [ -L "$live" ] || continue
          case "$(basename "$live" .yaml)" in
            secrets ${lib.concatMapStrings (n: "| ${n} ") (builtins.attrNames esphomeDevices)}) ;;
            *)
              if [ -L "$live" ] || [ "$(stat -c %U "$live")" = root ]; then
                rm -f "$live"
              else
                echo "warning: esphome $live is not in hosts/aida/esphome (created in the Builder?)" >&2
              fi
              ;;
          esac
        done
        if [ -L /var/lib/esphome/common ]; then rm -f /var/lib/esphome/common; fi
        install -d -o root -g esphome -m 0750 /var/lib/esphome/common
        for live in /var/lib/esphome/common/*; do
          case "$(basename "$live")" in
            ${lib.concatStringsSep " | " esphomeCommonFiles}) ;;
            *) rm -rf "$live" ;;
          esac
        done
      ''
      + lib.concatMapStrings (
        f: "esphomeInstall ${esphomeCommon + "/${f}"} /var/lib/esphome/common/${f}\n"
      ) esphomeCommonFiles
      + lib.concatStrings (
        lib.mapAttrsToList (
          name: file: "esphomeInstall ${file} /var/lib/esphome/${name}.yaml\n"
        ) esphomeDevices
      );
  };

  # mDNS for Matter device discovery/commissioning (not opened by openFirewall).
  networking.firewall.allowedUDPPorts = [ 5353 ];

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

    # matter-hub admin/commissioning UI behind Authelia — this is where the
    # bridges and their pairing codes live. Note: commissioning itself is Matter
    # over the LAN (5540/mDNS), not this HTTP UI, so SSO here is fine.
    "matter.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        handle {
          forward_auth http://127.0.0.1:9091 {
            uri /api/authz/forward-auth
            copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
          }
          reverse_proxy http://127.0.0.1:8482
        }
      '';
    };

    # ESPHome Device Builder behind Authelia — browser-only, no app/API clients
    # (HA reaches devices directly, not through the dashboard).
    "esphome.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        handle {
          forward_auth http://127.0.0.1:9091 {
            uri /api/authz/forward-auth
            copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
          }
          reverse_proxy http://127.0.0.1:6052
        }
      '';
    };
  };
}
