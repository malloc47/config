{ config, pkgs, ... }:

{
  imports = [
    ../nixos/configuration.nix
    ../hardware/vmware-fusion-arm.nix
    ../modules/settings.nix
    ../modules/vmware-guest.nix
  ];

  settings = {
    vm = true;
    username = "jwaggon9";
    fontSize = 9.0;
    profile = "rally";          # Keeping same SSH key
    xkbFile = "vm";
    terminal = "kitty";
  };


  # Needed to make VMWare Fusion + M1 work
  networking.interfaces.ens160.useDHCP = true;
  disabledModules = [ "virtualisation/vmware-guest.nix" ];
  virtualisation.vmware.guest.enable = true;
  nixpkgs.config.allowUnsupportedSystem = true;
  nixpkgs.overlays = [(import ../pkgs/aarch64.nix)];

  services.xserver = {
    dpi = 277;
    displayManager = {
      sessionCommands = ''
        ${pkgs.xorg.xset}/bin/xset r rate 200 40
        ${pkgs.xorg.xrandr}/bin/xrandr -s 3840x2400
      '';
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "optum";
  networking.networkmanager.enable = true;

  # HiDPI fix for alacritty
  environment.variables.WINIT_HIDPI_FACTOR = "3";
  # Chrome scaling fix
  environment.variables.GDK_SCALE = "3.0";
  environment.variables.GDK_DPI_SCALE="0.25";

  environment.systemPackages = with pkgs; [
    gtkmm3
  ];

  # Used for file sharing between host and guest
  services.openssh.enable = true;
  users.users.${config.settings.username} = {
    openssh.authorizedKeys.keys = [
      (builtins.readFile (../personal/ssh + "/${config.settings.profile}/id_rsa.pub"))
    ];
  };

  networking.firewall.enable = false;

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
