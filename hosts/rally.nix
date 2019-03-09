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
    fontSize = 9;
    profile = "rally";
    xkbFile = "vm";
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
    settings = config.settings;
    xsession.pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 64;
    };
  };
}
