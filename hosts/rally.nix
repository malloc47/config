{ config, pkgs, ... }:

{
  imports = [
    ../nixos/configuration.nix
    ../hardware/vmware-fusion.nix
    ../modules/settings.nix
  ];

  settings = {
    vm = true;
    username = "jwaggoner";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "rally";
  networking.networkmanager.enable = true;

  services.xserver.autoRepeatDelay = 250;

  #services.xserver.dpi = 128;

  home-manager.users.${config.settings.username}.settings = config.settings;
}
