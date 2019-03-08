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
    terminal = "WINIT_HIDPI_FACTOR=3 alacritty";
    fontSize = 9;
    profile = "rally";
  };

  services.vmwareGuest.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "rally";
  networking.networkmanager.enable = true;

  services.xserver.autoRepeatDelay = 250;

  services.xserver.dpi = 277;

  fonts.fontconfig.dpi = 277;

  home-manager.users.${config.settings.username}.settings = config.settings;
}
