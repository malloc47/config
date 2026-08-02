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

{ config, ... }:

let
  # LAN address of the SLZB-MR5U Zigbee coordinator. 6638 is SMLIGHT's default
  # Zigbee TCP socket port; `ember` is the driver for its EFR32 radio.
  slzb = {
    host = "192.168.1.124";
    port = 6638;
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
    ];
    config = {
      default_config = { };
      http = {
        server_host = "127.0.0.1";
        trusted_proxies = [ "127.0.0.1" ];
        use_x_forwarded_for = true;
      };
    };
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
