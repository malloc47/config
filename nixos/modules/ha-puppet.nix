# Puppet — renders Home Assistant pages to PNGs for e-ink panels (see
# pkgs/ha-puppet). Listens on 127.0.0.1:10000 only; expose a narrow slice of it
# (e.g. one dashboard path) through the reverse proxy for devices that fetch it.
#
# The HA access token is read from tokenFile via a systemd credential and
# written into Puppet's options JSON at start, so it never reaches the store.
# Anyone who can reach Puppet can screenshot whatever the token's user can see,
# so use a token from a dedicated non-admin HA user.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.ha-puppet;
in
{
  options.services.ha-puppet = {
    enable = lib.mkEnableOption "Puppet, a Home Assistant screenshot server for e-ink displays";
    package = lib.mkPackageOption pkgs "ha-puppet" { };
    tokenFile = lib.mkOption {
      type = lib.types.path;
      description = "File containing a Home Assistant long-lived access token.";
    };
    homeAssistantUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://127.0.0.1:8123";
      description = "Home Assistant URL Puppet's browser loads pages from.";
    };
    keepBrowserOpen = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Keep Chromium running between requests (faster renders, more memory).";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.ha-puppet = {
      description = "Puppet Home Assistant screenshot server";
      after = [
        "network-online.target"
        "home-assistant.service"
      ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      environment = {
        PUPPET_OPTIONS_FILE = "/run/ha-puppet/options.json";
        PUPPET_LISTEN_HOST = "127.0.0.1";
        HOME = "/var/lib/ha-puppet";
      };
      serviceConfig = {
        ExecStartPre = pkgs.writeShellScript "ha-puppet-options" ''
          ${lib.getExe pkgs.jq} -n \
            --rawfile token "$CREDENTIALS_DIRECTORY/token" \
            --arg url ${lib.escapeShellArg cfg.homeAssistantUrl} \
            --argjson keep ${lib.boolToString cfg.keepBrowserOpen} \
            '{home_assistant_url: $url, access_token: ($token | rtrimstr("\n")), keep_browser_open: $keep}' \
            > "$RUNTIME_DIRECTORY/options.json"
        '';
        ExecStart = lib.getExe cfg.package;
        LoadCredential = "token:${cfg.tokenFile}";
        DynamicUser = true;
        StateDirectory = "ha-puppet";
        RuntimeDirectory = "ha-puppet";
        RuntimeDirectoryMode = "0700";
        Restart = "on-failure";
        RestartSec = 5;

        # Chromium runs with --no-sandbox (Puppet's default), so lean on systemd.
        # No MemoryDenyWriteExecute: V8's JIT needs W+X pages.
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
          "AF_NETLINK"
        ];
        LockPersonality = true;
        CapabilityBoundingSet = "";
      };
    };
  };
}
