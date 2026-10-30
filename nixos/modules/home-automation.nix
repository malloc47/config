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
            # Local external components and fonts (installed under
            # components/ and fonts/).
            (builtins.unsafeDiscardStringContext (toString epaperSpiG4))
            (builtins.unsafeDiscardStringContext (toString esphomeFonts))
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
  # epaper_spi with the 4-gray UC8179 model from esphome/esphome#18470 (open;
  # validated by its author on the reTerminal E1001), as a local external
  # component for e1001-clock: this ESPHome's own epaper_spi plus the PR's three
  # new files, pinned by commit. Installed at /var/lib/esphome/components by the
  # esphomeConfigs activation below. The model registers without
  # partial_update, which this ESPHome's validator requires for
  # full_update_every > 1, so that default is patched in. Drop once the model is
  # upstream.
  epaperSpiG4 =
    let
      rev = "1aff3ccf70de920bcb2f90dd305a5a331775b1ab";
      fetchPr =
        file: hash:
        pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/Daniel085/esphome/${rev}/esphome/components/epaper_spi/${file}";
          inherit hash;
        };
    in
    pkgs.runCommand "esphome-epaper-spi-g4-${esphome.version}" { } ''
      mkdir -p $out
      cp -r ${esphome}/lib/python3*/site-packages/esphome/components/epaper_spi $out/epaper_spi
      chmod -R u+w $out
      find $out -name __pycache__ -prune -exec rm -rf {} +
      cp ${fetchPr "epaper_spi_uc8179_g4.cpp" "sha256-sZ5Csf71998PxSwBpwD0GB3UZXP8g77ranOZS6zGDjQ="} \
        $out/epaper_spi/epaper_spi_uc8179_g4.cpp
      cp ${fetchPr "epaper_spi_uc8179_g4.h" "sha256-UQ0gR4QCUgkBdzA6voCl2L9c1NjwSTqHWBrbm8uzPuA="} \
        $out/epaper_spi/epaper_spi_uc8179_g4.h
      cp ${fetchPr "models/uc8179_g4.py" "sha256-+mv+65KCXyTBNJldtgvuKAoWAWTAp7ieM+D2puNaQsQ="} \
        $out/epaper_spi/models/uc8179_g4.py
      sed -i 's/^\(        \)super().__init__(name, "EPaperUC8179G4", \*\*defaults)/\1defaults.setdefault("partial_update", True)\n&/' \
        $out/epaper_spi/models/uc8179_g4.py
      grep -q 'defaults.setdefault("partial_update", True)' $out/epaper_spi/models/uc8179_g4.py
      # Full 4-gray refreshes came out inverted on this E1001 (as on the
      # Waveshare HAT reported in the PR's review): use the plane encoding of
      # Waveshare's EPD_7IN5_V2_Display_4Gray sample (white=(0,0),
      # black=(1,1)). The partial/fast-flip packing is separate and unchanged.
      sed -i \
        -e 's/LEVEL_BITS_DTM1\[4\] = {0, 0, 1, 1};/LEVEL_BITS_DTM1[4] = {1, 0, 1, 0};/' \
        -e 's/LEVEL_BITS_DTM2\[4\] = {0, 1, 0, 1};/LEVEL_BITS_DTM2[4] = {1, 1, 0, 0};/' \
        $out/epaper_spi/epaper_spi_uc8179_g4.cpp
      grep -q 'LEVEL_BITS_DTM1\[4\] = {1, 0, 1, 0};' $out/epaper_spi/epaper_spi_uc8179_g4.cpp
      grep -q 'LEVEL_BITS_DTM2\[4\] = {1, 1, 0, 0};' $out/epaper_spi/epaper_spi_uc8179_g4.cpp
      # Same inversion on the 1-bit partial/fast-flip path: with CDI 0xA9 a 1
      # bit is white (as the in-tree mono driver relies on), but the PR packs
      # dark=1, so black->white strokes hit the toward-black LUT and old clock
      # digits never cleared. Pack white=1 instead (stored previous plane too).
      sed -i \
        -e 's/PLANE_BIT\[4\] = {1, 1, 0, 0};/PLANE_BIT[4] = {0, 0, 1, 1};/' \
        -e 's/PLANE_BIT_INV\[4\] = {0, 0, 1, 1};/PLANE_BIT_INV[4] = {1, 1, 0, 0};/' \
        $out/epaper_spi/epaper_spi_uc8179_g4.cpp
      grep -q 'PLANE_BIT\[4\] = {0, 0, 1, 1};' $out/epaper_spi/epaper_spi_uc8179_g4.cpp
      grep -q 'PLANE_BIT_INV\[4\] = {1, 1, 0, 0};' $out/epaper_spi/epaper_spi_uc8179_g4.cpp
    '';
  # Font files device configs reference as fonts/<name> (installed at
  # /var/lib/esphome/fonts by the esphomeConfigs activation below): e1001-clock
  # draws its battery indicator with the dashboard card's DejaVu Sans Bold and
  # MDI's battery glyphs.
  esphomeFonts = pkgs.runCommand "esphome-fonts" { } ''
    mkdir $out
    cp ${pkgs.dejavu_fonts}/share/fonts/truetype/DejaVuSans-Bold.ttf $out/
    cp ${pkgs.material-design-icons}/share/fonts/truetype/materialdesignicons-webfont.ttf $out/
  '';
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

  # Exterior doors/gates (plus the attic) for the e1001-clock panel: listed in
  # its banner while open (via binary_sensor.perimeter_doors) and pushing a dashboard
  # refresh when they change. Interior doors (kids' room, stairs closet) are
  # deliberately left out.
  perimeterDoors = [
    "binary_sensor.front_door_contact"
    "binary_sensor.dining_room_sliding_door_contact"
    "binary_sensor.side_door_contact"
    "binary_sensor.utility_room_door_contact"
    "binary_sensor.basement_door_contact"
    "binary_sensor.attic_door_contact"
    "binary_sensor.west_gate_contact"
    "binary_sensor.east_gate_contact"
  ];
in
{
  imports = [ ./ha-puppet.nix ];

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

  # Local speech for HA's Assist pipeline (voice satellites such as the
  # living-room Fire tablet running Kiosk Satellite): speech-to-text and Piper
  # text-to-speech as Wyoming servers on loopback, wired into HA through the
  # wyoming integration. The STT model is left to wyoming-faster-whisper's
  # per-language default; models and voices download into each service's
  # state directory on first start.
  services.wyoming.faster-whisper.servers.main = {
    enable = true;
    language = "en";
    uri = "tcp://127.0.0.1:10300";
    zeroconf.enable = false;
  };
  services.wyoming.piper.servers.main = {
    enable = true;
    voice = "en_US-lessac-medium";
    uri = "tcp://127.0.0.1:10200";
    zeroconf.enable = false;
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
      # eink-dashboard-card — packaged in-repo (pkgs/home-assistant-eink-dashboard-card).
      # The whole 800x480 screen of the e1001-clock panel, rendered by Puppet
      # (services.ha-puppet below); used as the only card of a panel view.
      pkgs.home-assistant-eink-dashboard-card
      # family-calendar-card — packaged in-repo (pkgs/home-assistant-family-calendar-card).
      # The kitchen touchscreen's whole screen (clock, weather outlook, week
      # calendar with per-calendar chips, controls); the only card of a panel
      # view (hosts/aida/home-assistant/family-calendar-dashboard.yaml).
      pkgs.home-assistant-family-calendar-card
      # kindle-weather-card — packaged in-repo (pkgs/home-assistant-kindle-weather-card).
      # The whole 1072x1448 screen of the wall Kindle, rendered by Puppet;
      # the only card of a panel view (hosts/aida/home-assistant/
      # kindle-weather-dashboard.yaml, docs/kindle-weather.md).
      pkgs.home-assistant-kindle-weather-card
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
      # Wyoming protocol: connects HA's Assist pipeline to the local
      # speech-to-text and text-to-speech servers below (services.wyoming).
      # Added via the UI config flow, one entry per server (127.0.0.1:10300
      # and :10200).
      "wyoming"
      # Matter controller integration — added via the UI, connects to the
      # matter-server below on ws://127.0.0.1:5580. This is HA commissioning and
      # controlling real Matter devices (inbound), distinct from the matter-hub
      # that exposes HA to Google (outbound). See services.matter-server below.
      "matter"
      # Send commands/broadcasts to Google Assistant from HA (packages
      # gassist-text). OAuth is set up in the UI via Application Credentials;
      # without this component the config flow 500s with "Invalid handler".
      "google_assistant_sdk"
      # Google Calendar (packages gcal-sync): a calendar.* entity per calendar
      # with upcoming events, for the family-calendar kiosk dashboard. Same
      # Application Credentials OAuth flow as google_assistant_sdk above; the
      # Google Cloud project needs the Google Calendar API enabled. Calendars
      # are picked via the UI config flow.
      "google"
      # Holidays as a calendar (packages holidays): computed locally, no
      # account, for the family-calendar kiosk dashboard. Added via the UI
      # config flow (country US, subdivision NY; options: the "unofficial"
      # category too, e.g. Halloween, Mother's Day), giving
      # calendar.united_states_ny.
      "holiday"
      # RESTful sensors (packages jsonpath, xmltodict): the planetary K-index
      # below, for the family-calendar kiosk's aurora scene.
      "rest"
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
      # National Weather Service forecasts (packages pynws): the e-ink panel's
      # primary weather source (7 days of day/night periods and hourly
      # forecasts with precipitation probability; met.no stays as its
      # fallback). Added via the config flow; its "API key" is just a contact
      # string NWS asks clients to send.
      "nws"
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
      # Planetary K-index (geomagnetic activity, 0-9) from NOAA SWPC, the
      # latest 3-hour value, polled hourly: drives the family-calendar
      # kiosk's aurora scene (docs/family-calendar-scenes.md), which shows
      # on active days (default Kp >= 4) rather than only when an aurora
      # would actually be visible at our latitude (Kp ~7+).
      sensor = [
        {
          platform = "rest";
          name = "Planetary K-index";
          unique_id = "planetary_k_index";
          resource = "https://services.swpc.noaa.gov/products/noaa-planetary-k-index.json";
          value_template = "{{ value_json[-1].Kp | float(0) }}";
          state_class = "measurement";
          scan_interval = 3600;
        }
      ];
      # The wall Kindle's battery, POSTed by its dashboard loop on each wake
      # ({"battery": 87, "charging": 0}; hosts/aida/kindle-weather). Nothing
      # is drawn on the Kindle itself; this is for knowing when to charge it
      # and how fast it drains. local_only: HA sees the Kindle's LAN address
      # through Caddy (trusted_proxies), and the id isn't a secret.
      template = [
        {
          trigger = [
            {
              trigger = "webhook";
              webhook_id = "kindle-weather-battery";
              allowed_methods = [ "POST" ];
              local_only = true;
            }
          ];
          sensor = [
            {
              name = "Kindle weather battery";
              unique_id = "kindle_weather_battery";
              device_class = "battery";
              unit_of_measurement = "%";
              state_class = "measurement";
              state = "{{ trigger.json.battery | int(0) }}";
              attributes.charging = "{{ trigger.json.charging | int(0) == 1 }}";
            }
          ];
        }
      ];
      # Home/installation display name (Settings -> System -> General -> Name).
      homeassistant.name = "Berry Patch";
      # HA's own address, for URLs it hands to devices (e.g. Assist reply
      # audio for voice satellites, /api/tts_proxy/...). Unset, HA falls back
      # to http://<LAN IP>:8123, but it only listens on loopback behind Caddy,
      # so devices couldn't fetch them.
      homeassistant.internal_url = "https://home.malloc47.com";
      homeassistant.external_url = "https://home.malloc47.com";
      # "Kiosk": layout tweaks for small wall tablets, applied per view (the
      # living-room Fire HD 8 dashboard, hosts/aida/home-assistant/
      # living-room-dashboard.yaml). Its WebView is 960x600 CSS px, where the
      # sections view's default 320px minimum column width and 32px gaps fit
      # only two columns. Empty light/dark modes keep HA's default colours and
      # the device's light/dark switching.
      frontend.themes.Kiosk = {
        ha-view-sections-column-min-width = "260px";
        ha-view-sections-column-gap = "16px";
        modes = {
          light = { };
          dark = { };
        };
      };
      # Point the UI automation/scene/script editors at writable include files.
      # The module unquotes leading-bang strings, so these become real YAML
      # `!include` tags. Without them, the UI saves the file but HA never loads
      # it and "New automation setup" times out. The files themselves stay
      # HA-owned/writable and are backed up via the seed/drift wiring below.
      automation = "!include automations.yaml";
      scene = "!include scenes.yaml";
      script = "!include scripts.yaml";
      # Free-text message for the e1001-clock panel's banner; the card's default
      # banner template shows it when set, else open doors/windows. Set it from
      # automations (input_text.set_value) or the entity's more-info dialog.
      input_text.eink_banner = {
        name = "E-ink banner";
        max = 255;
        initial = "";
      };
      # The perimeter list as a first-class HA entity, binary_sensor.perimeter_doors:
      # Open while any member is open, usable like any door sensor in automations
      # (members via its entity_id attribute / expand()). The panel's card reads
      # its members (doors_entity) to list open doors. The unique_id lets its
      # area/icon be set in the UI; membership stays here.
      binary_sensor = [
        {
          platform = "group";
          name = "Perimeter doors";
          unique_id = "perimeter_doors";
          device_class = "door";
          entities = perimeterDoors;
        }
      ];
      # Push dashboard refreshes to the e1001-clock panel (Puppet re-render +
      # download) when something it shows changes, instead of waiting for its
      # 15-minute poll: weather updates (state or attributes), the dishwasher
      # starting/stopping and every 10% of its progress, the banner message and
      # perimeter doors (state changes), and HA start (deploys restart HA and
      # Puppet; wait until entities have loaded). All go out as flash-free
      # partial updates; the panel's only scheduled full refresh is its own
      # hourly one (e1001-clock.yaml). `restart` + a short delay coalesces
      # bursts. The call is skipped while the panel is disconnected (its
      # entities go unavailable, e.g. after an HA restart or during a flash):
      # its action isn't registered then, so the call would fail and raise an
      # "unknown action" repair, and the panel re-fetches everything itself
      # when it reconnects.
      # Nix-managed alongside the UI's automations.yaml.
      "automation eink" =
        let
          panelConnected = {
            condition = "template";
            value_template = "{{ not is_state('button.e1001_clock_refresh_dashboard', 'unavailable') }}";
          };
        in
        [
          {
            id = "eink_panel_refresh";
            alias = "E-ink panel: push dashboard refresh";
            mode = "restart";
            triggers = [
              {
                # NWS (the panel's primary source) and met.no (its fallback).
                trigger = "state";
                entity_id = [
                  "weather.nws_khpn"
                  "weather.forecast_home"
                ];
              }
              {
                trigger = "state";
                entity_id = "sensor.dishwasher_operation_state";
              }
              {
                trigger = "state";
                entity_id = "sensor.dishwasher_program_progress";
                id = "dishwasher_progress";
              }
              {
                # State changes only, not attribute updates.
                trigger = "state";
                entity_id = perimeterDoors ++ [ "input_text.eink_banner" ];
                to = null;
              }
              {
                trigger = "homeassistant";
                event = "start";
                id = "start";
              }
            ];
            # Progress only refreshes when it crosses a 10% step.
            conditions = [
              {
                condition = "template";
                value_template = "{{ trigger.id != 'dishwasher_progress' or trigger.from_state is none or trigger.to_state is none or (trigger.to_state.state | int(0)) // 10 != (trigger.from_state.state | int(0)) // 10 }}";
              }
            ];
            actions = [
              { delay = "{{ '00:01:30' if trigger.id == 'start' else '00:00:05' }}"; }
              panelConnected
              {
                action = "esphome.e1001_clock_refresh_dashboard";
                data.full = false;
                continue_on_error = true;
              }
            ];
          }
        ];
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
      # Firmware builds (~1 min, all cores, ~1G) yield to aida's other services.
      # Inherited by esphome-deploy@ below.
      CPUWeight = "idle";
      Nice = 19;
      IOSchedulingClass = "idle";
      MemoryHigh = "3G";
      # The repo owns these (installed by the esphomeConfigs activation script
      # below). The Builder saves via write-to-temp + rename, which would replace
      # a merely root-owned file in its own state dir; a read-only bind mount
      # makes the save fail instead. `-`: skip any not yet installed.
      ReadOnlyPaths = [
        "-/var/lib/esphome/common"
        "-/var/lib/esphome/components"
        "-/var/lib/esphome/fonts"
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
        # Local external components and fonts (built by Nix; see epaperSpiG4
        # and esphomeFonts): replaced wholesale with root-owned copies, like
        # common/.
        esphomeInstallTree() {
          rm -rf "$2"
          install -d -o root -g esphome -m 0750 "$2"
          cp -r --no-preserve=mode,ownership "$1"/. "$2"/
          chown -R root:esphome "$2"
          find "$2" -type d -exec chmod 0750 {} +
          find "$2" -type f -exec chmod 0640 {} +
        }
        esphomeInstallTree ${epaperSpiG4} /var/lib/esphome/components
        esphomeInstallTree ${esphomeFonts} /var/lib/esphome/fonts
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

  # Puppet — screenshots the e-ink dashboard (eink-dashboard-card in a panel
  # view at /eink-panel/clock) for the e1001-clock panel, which downloads it as
  # its background and draws the time/date over it on-device. The token belongs
  # to a dedicated non-admin HA user (`eink`), in secrets/ha-puppet-token.age
  # (its age.secrets entry in hosts/aida.nix follows this flag).
  services.ha-puppet = {
    enable = true;
    tokenFile = config.age.secrets.ha-puppet-token.path;
    # The panel runs in 4-gray, so text is anti-aliased into the gray levels
    # (Puppet's colors=000000,555555,AAAAAA,FFFFFF). For a 1-bit panel, turn
    # this back on: anti-aliased grey thresholded to black/white comes out
    # blobby, hinted 1-bit text doesn't.
    monochromeText = false;
  };
  # Puppet keeps one browser page open, which only loads Lovelace resources on
  # a full page load: restart it with HA (whose restarts otherwise leave the
  # page on stale, failed subscriptions) and whenever a card it renders changes.
  systemd.services.ha-puppet = {
    partOf = [ "home-assistant.service" ];
    restartTriggers = [
      pkgs.home-assistant-eink-dashboard-card
      pkgs.home-assistant-kindle-weather-card
    ];
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

    # The e-ink panels' images, from Puppet: the E1001's background and the
    # wall Kindle's whole screen (docs/kindle-weather.md). No Authelia (neither
    # device can log in), so only their dashboard paths are reachable, and only
    # from the LAN; everything else Puppet could render stays blocked.
    "eink.home.malloc47.com" = lib.mkIf config.services.ha-puppet.enable {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        @panel {
          path /eink-panel/* /kindle-weather/*
          remote_ip 192.168.1.0/24
        }
        handle @panel {
          reverse_proxy http://127.0.0.1:10000
        }
        handle {
          respond 403
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
