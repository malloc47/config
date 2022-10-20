{ config, pkgs, ... }:

{
  imports = [
    ../nixos/configuration.nix
    ../hardware/macbook-air.nix
    ../modules/settings.nix
  ];

  settings = {
    xkbFile = "macbook-modified";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "tosca";
  networking.networkmanager.enable = true;

  # networking.firewall.allowedTCPPorts = [8280];

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    tapButtons = false;
    accelFactor = "0.001";
    buttonsMap = [ 1 3 2 ];
    palmDetect = true;
    minSpeed = "0.70";
    maxSpeed = "1.20";
    additionalOptions = ''
      Option "VertScrollDelta" "-480"
      Option "HorizScrollDelta" "-480"
      Option "FingerLow" "40"
      Option "FingerHigh" "70"
      Option "Resolution" "100"
      Option "SoftButtonAreas" "93% 0 93% 0 0 0 0 0"
    '';
  };

  services.xserver.autoRepeatDelay = 250;

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  #services.xserver.dpi = 128;

  programs.light.enable = true;

  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -s sysfs/backlight/mba6x_backlight -A 10"; }
      { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -s sysfs/backlight/mba6x_backlight -U 10"; }
      { keys = [ 230 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -s sysfs/leds/smc::kbd_backlight -A 10"; }
      { keys = [ 229 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -s sysfs/leds/smc::kbd_backlight -U 10"; }
    ];
  };

  sound.mediaKeys.enable = true;

  services.logind.extraConfig = "HandlePowerKey=suspend";

  powerManagement = { enable = true; cpuFreqGovernor = "ondemand"; };

  environment.systemPackages = [ pkgs.i3lock ];

  systemd.services.lock-screen = {
    before = [ "sleep.target" ];
    wantedBy = [ "sleep.target" ];
    description = "Lock the screen";
    serviceConfig = {
      User = config.settings.username;
      Environment = "DISPLAY=:0";
      ExecStart = "${pkgs.i3lock}/bin/i3lock --nofork";
    };
  };

  home-manager.users.${config.settings.username} = {
    settings = config.settings;
    home.packages = with pkgs; [
      zoom-us
      python-language-server
      google-chrome
    ];
  };
}
