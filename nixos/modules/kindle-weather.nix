# The wall Kindle (docs/kindle-weather.md) as a Home Assistant device over
# MQTT, riding on its hourly wakes.
#
# On each wake the Kindle publishes one status message (kindle-weather/state)
# and reads its settings (retained on kindle-weather/set/#), through a LAN
# listener of its own, with a `kindle` user that can do only that. Its curl
# can't publish retained messages, so aida carries the retained parts: this
# module publishes the device's discovery configs, copies each status to a
# retained kindle-weather/status (which the entities read, so they keep their
# values across HA restarts), and clears the one-shot exit button's retained
# message once the Kindle reports seeing it. Settings changed in HA are
# retained and take effect at the Kindle's next wake; the number and switch
# entities show the values the Kindle last reported applying.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Mosquitto's LAN listener for the Kindle (1883 is the loopback listener
  # HA and zigbee2mqtt use).
  port = 1884;
  status = "kindle-weather/status";
  set = "kindle-weather/set";

  device = {
    identifiers = [ "kindle_weather" ];
    name = "Kindle weather";
    manufacturer = "Amazon";
    model = "Kindle Paperwhite 3";
  };

  entity = component: id: cfg: {
    topic = "homeassistant/${component}/kindle_weather/${id}/config";
    payload = builtins.toJSON (
      {
        inherit device;
        unique_id = "kindle_weather_${id}";
      }
      // cfg
    );
  };
  sensor =
    id: name: value_template: extra:
    entity "sensor" id (
      {
        inherit name value_template;
        state_topic = status;
      }
      // extra
    );
  timestamp =
    id: name: field:
    # 0 (e.g. no next wake after an exit) renders "None", which MQTT sensors read as unknown.
    sensor id name
      "{{ (value_json.${field} | int) | as_datetime if value_json.${field} | int > 0 else None }}"
      {
        device_class = "timestamp";
      };
  number =
    id: name: extra:
    entity "number" id (
      {
        inherit name;
        state_topic = status;
        value_template = "{{ value_json.${id} }}";
        command_topic = "${set}/${id}";
        retain = true;
        mode = "box";
        entity_category = "config";
      }
      // extra
    );
  diagnostic = {
    entity_category = "diagnostic";
  };

  entities = [
    (sensor "battery" "Battery" "{{ value_json.battery }}" {
      device_class = "battery";
      unit_of_measurement = "%";
      state_class = "measurement";
    })
    (entity "binary_sensor" "charging" {
      name = "Charging";
      state_topic = status;
      value_template = "{{ 'ON' if value_json.charging else 'OFF' }}";
      device_class = "battery_charging";
    })
    (sensor "voltage" "Battery voltage" "{{ (value_json.voltage / 1000) | round(3) }}" (
      diagnostic
      // {
        device_class = "voltage";
        unit_of_measurement = "V";
        state_class = "measurement";
      }
    ))
    (sensor "current" "Battery current" "{{ value_json.current }}" (
      diagnostic
      // {
        device_class = "current";
        unit_of_measurement = "mA";
        state_class = "measurement";
      }
    ))
    (sensor "temperature" "Battery temperature" "{{ value_json.temperature }}" (
      diagnostic
      // {
        device_class = "temperature";
        unit_of_measurement = "°F";
        state_class = "measurement";
      }
    ))
    (sensor "cycles" "Battery charge cycles" "{{ value_json.cycles }}" (
      diagnostic
      // {
        icon = "mdi:battery-sync";
        state_class = "total_increasing";
      }
    ))
    (sensor "wifi" "Wi-Fi signal" "{{ value_json.wifi }}" (
      diagnostic
      // {
        icon = "mdi:wifi";
        unit_of_measurement = "bars";
        state_class = "measurement";
      }
    ))
    (timestamp "last_wake" "Last wake" "wake")
    (timestamp "next_wake" "Next wake" "next")
    (timestamp "last_image" "Last new image" "refreshed")
    (sensor "wake_reason" "Wake reason" "{{ value_json.reason }}" (
      diagnostic
      // {
        device_class = "enum";
        options = [
          "start"
          "timer"
          "button"
          "maintenance"
        ];
      }
    ))
    (sensor "last_result" "Last result" "{{ value_json.result }}" {
      device_class = "enum";
      options = [
        "new"
        "unchanged"
        "failed"
        "no_wifi"
        "exit"
      ];
    })
    (sensor "awake_time" "Awake time" "{{ value_json.awake }}" (
      diagnostic
      // {
        device_class = "duration";
        unit_of_measurement = "s";
        state_class = "measurement";
      }
    ))
    (number "interval" "Update interval" {
      min = 5;
      max = 720;
      step = 5;
      unit_of_measurement = "min";
    })
    (number "quiet_start" "Quiet hours start" {
      min = 0;
      max = 23;
      unit_of_measurement = "h";
    })
    (number "quiet_end" "Quiet hours end" {
      min = 0;
      max = 23;
      unit_of_measurement = "h";
    })
    (entity "switch" "maintenance" {
      name = "Maintenance mode";
      state_topic = status;
      value_template = "{{ value_json.maintenance }}";
      command_topic = "${set}/maintenance";
      retain = true;
      icon = "mdi:wrench";
      entity_category = "config";
    })
    (entity "button" "exit" {
      name = "Exit dashboard";
      command_topic = "${set}/exit";
      payload_press = "PRESS";
      retain = true;
      icon = "mdi:exit-run";
    })
  ];

  publishDiscovery = lib.concatMapStrings (
    e:
    "mosquitto_pub -r -t ${lib.escapeShellArg e.topic} -f ${pkgs.writeText "kindle-weather-discovery.json" e.payload}\n"
  ) entities;
in
{
  services.mosquitto.listeners = [
    {
      inherit port;
      settings.allow_anonymous = false;
      users.kindle = {
        passwordFile = config.age.secrets.kindle-mqtt-password.path;
        acl = [
          "write kindle-weather/state"
          "read ${set}/#"
        ];
      };
    }
  ];
  networking.firewall.interfaces.eno1.allowedTCPPorts = [ port ];

  systemd.services.kindle-weather-mqtt = {
    description = "Kindle weather: MQTT discovery and status relay";
    after = [ "mosquitto.service" ];
    requires = [ "mosquitto.service" ];
    partOf = [ "mosquitto.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [
      pkgs.mosquitto
      pkgs.jq
    ];
    serviceConfig = {
      DynamicUser = true;
      LoadCredential = "mqtt:${config.age.secrets.mqtt-password.path}";
      RuntimeDirectory = "kindle-weather-mqtt";
      RuntimeDirectoryMode = "0700";
      Restart = "always";
      RestartSec = 10;
    };
    script = ''
      # The clients read default options from $XDG_CONFIG_HOME, which keeps
      # the broker password off their command lines.
      export XDG_CONFIG_HOME=$RUNTIME_DIRECTORY
      printf -- '-h 127.0.0.1\n-u mqtt\n-P %s\n' "$(cat "$CREDENTIALS_DIRECTORY/mqtt")" \
        > "$XDG_CONFIG_HOME/mosquitto_pub"
      cp "$XDG_CONFIG_HOME/mosquitto_pub" "$XDG_CONFIG_HOME/mosquitto_sub"

      ${publishDiscovery}
      mosquitto_sub -t kindle-weather/state | while IFS= read -r msg; do
        mosquitto_pub -r -t ${status} -m "$msg"
        if [ "$(jq -r '.exit_seen // false' <<<"$msg" 2>/dev/null)" = true ]; then
          mosquitto_pub -r -n -t ${set}/exit
        fi
      done
    '';
  };
}
