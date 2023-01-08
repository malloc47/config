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
    fontSize = 9.0;
    profile = "rally";
    xkbFile = "vm";
    terminal = "kitty";
  };

  virtualisation.vmware.guest.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "rally";
  networking.networkmanager.enable = true;
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

    # Work around VMWare bug:
    # https://github.com/vmware/open-vm-tools/issues/287
    programs.ssh.matchBlocks."*".extraOptions.IPQoS="throughput";
    programs.ssh.matchBlocks."*".user = "jarrell.waggoner";

    programs.i3status.modules = {
      "ethernet _first_" = {
        position = 3;
        settings = {
          format_up = "E: %ip";
          format_down = "E";
        };
      };
      "battery all" = {
        position = 4;
        settings = {
          format = "%status %percentage %remaining";
	        format_down = "⚡";
        };
      };
    };
  };
}
