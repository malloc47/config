{ config, pkgs, ... }:

{
  imports = [
    ../nixos/configuration.nix
    ../hardware/vmware-fusion.nix
    ../modules/settings.nix
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontSize = 9.0;
    xkbFile = "vm";
    terminal = "kitty";
  };

  virtualisation.vmware.guest.enable = true;

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  networking.hostName = "salome";
  networking.networkmanager.enable = true;
  networking.firewall.enable = false;
  networking.nameservers = ["8.8.8.8" "8.8.4.4"];

  services.xserver.autoRepeatDelay = 250;

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

  services.journald.extraConfig = ''
      SystemMaxUse=2G
  '';

  home-manager.users.${config.settings.username} = {
    settings = config.settings;
    home.pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 64;
    };

    xresources.properties = {
      "Xft.dpi" = 277;
    };

    # Work around VMWare bug:
    # https://github.com/vmware/open-vm-tools/issues/287
    programs.ssh.matchBlocks."*".extraOptions.IPQoS="throughput";
    programs.ssh.matchBlocks."*".user = "jarrell.waggoner";
  };
}
