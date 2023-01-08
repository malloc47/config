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
  };

  networking.hostName = "harpocrates";
  networking.firewall.enable = false;
  networking.nameservers = ["8.8.8.8" "8.8.4.4"];

  services.xserver.dpi = 277;
  # Launch xorg on display :1 so it does not collide with host display
  # Note: These only work with startx
  services.xserver.display = 1;
  services.xserver.tty = 8;
  services.xserver.exportConfiguration = true;

  # # Attempt to use lightdm but not working yet :(
  # services.xserver.displayManager.lightdm.extraConfig = ''
  #   minimum-display-number=1
  #   minimum-vt=8
  #   logind-check-graphical=true
  # '';

  # udev does not work in LXC, so input has to be specified manually

  environment.systemPackages = with pkgs; [
    xorg.xf86videovmware
    xorg.xf86inputevdev
  ];

  services.xserver.videoDrivers = ["vmware"];

  services.xserver.config = ''
    Section "InputDevice"
      Identifier  "Keyboard0"
      Driver      "evdev"
      Option      "Device" "/dev/input/event0"
    EndSection

    Section "InputDevice"
      Identifier  "Mouse0"
      Driver      "evdev"
      Option      "Device" "/dev/input/event3"
    EndSection
  '';

  services.xserver.serverLayoutSection = ''
    InputDevice "Keyboard0" "CoreKeyboard"
    InputDevice "Mouse0" "CoreMouse"
  '';

  services.xserver.serverFlagsSection = ''
    Option "AutoAddDevices" "False"
  '';

  services.xserver.displayManager.startx.enable = true;

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
    # startx does not use .xsession but all the contents are valid for
    # an .xinitrc
    xsession.scriptPath = ".xinitrc";

    # # Use the host's xsession
    # home.sessionVariables = {
    #   DISPLAY = ":0";
    # };

    programs.i3status.modules = {
      "ethernet _first_" = {
        position = 3;
        settings = {
          format_up = "E: %ip";
          format_down = "E";
        };
      };
    };
  };
}
