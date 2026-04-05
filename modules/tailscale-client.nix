{ config, lib, ... }:

with lib;

let
  cfg = config.services.tailscale-client;
in
{
  options.services.tailscale-client = {
    enable = mkEnableOption "plain Tailscale client (no routes, no exit node)";

    loginServer = mkOption {
      type = types.str;
      description = "Headscale/Tailscale login server URL.";
      example = "https://hs.malloc47.com";
    };
  };

  config = mkIf cfg.enable {
    services.tailscale = {
      enable = true;
      openFirewall = true;
      extraUpFlags = [
        "--login-server"
        cfg.loginServer
        "--reset"
      ];
    };
  };
}
