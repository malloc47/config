{ config, lib, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/lxc-container.nix"
  ];

  # https://discourse.nixos.org/t/howto-setup-lxd-on-nixos-with-nixos-guest-using-unmanaged-bridge-network-interface/21591
  boot.isContainer = true;
  environment.noXlibs = false;
  programs.command-not-found.enable = true;
  documentation.enable = true;
  environment.variables.NIX_REMOTE = lib.mkForce "";
  systemd.services."console-getty".enable = false;
  systemd.services."getty@".enable = false;


}
