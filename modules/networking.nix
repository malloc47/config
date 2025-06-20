{ config, pkgs, lib, ... }:
{
  imports = [ ./settings.nix ];
  config = {
    networking.networkmanager.enable = true;
    networking.firewall.enable = false;
    networking.nameservers = ["8.8.8.8" "8.8.4.4"];

    services.openssh.enable = lib.mkIf (config.settings.vm) true;
    programs.ssh.startAgent = true;

    settings.extraGroups = ["networkmanager"];
  };
}
