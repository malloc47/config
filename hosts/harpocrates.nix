{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ../nixos/configuration.nix
    ../modules/settings.nix
    ../hardware/lxc.nix
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontSize = 9.0;
    xkbFile = "vm";
    terminal = "kitty";
  };

  networking.hostName = "harpocrates";
  networking.firewall.enable = false;
  networking.nameservers = ["8.8.8.8" "8.8.4.4"];

  services.xserver.dpi = 277;

  hardware.video.hidpi.enable = true;

  # HiDPI fix for alacritty
  environment.variables.WINIT_HIDPI_FACTOR = "3";
  # Chrome scaling fix
  environment.variables.GDK_SCALE = "3.0";
  environment.variables.GDK_DPI_SCALE="0.25";

  # Used for file sharing between host and guest
  services.openssh.enable = true;
  users.users.${config.settings.username} = {
    openssh.authorizedKeys.keys = [
      (builtins.readFile (../personal/ssh + "/${config.settings.profile}/id_rsa.pub"))
    ];
  };

  home-manager.users.${config.settings.username} = {
    home.pointerCursor.size = 64;
    xresources.properties."Xft.dpi" = 277;
  };
}
