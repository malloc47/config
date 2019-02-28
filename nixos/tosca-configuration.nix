{ config, pkgs, ... }:

{
  imports =
    [
      ./configuration.nix
      ./macbook-air-hardware-configuration.nix
      ./users.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "tosca"; # Define your hostname.
  networking.networkmanager.enable = true;

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    tapButtons = false;
    accelFactor = "0.001";
    buttonsMap = [ 1 3 2 ];
    palmDetect = true;
    additionalOptions = ''
      Option "VertScrollDelta" "-480"
      Option "HorizScrollDelta" "-480"
      Option "FingerLow" "40"
      Option "FingerHigh" "70"
      Option "Resolution" "720"
    '';
  };

  programs.light.enable = true;

  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 225 ]; events = [ "key" ]; command = "/run/wrappers/bin/light -A 10"; }
      { keys = [ 224 ]; events = [ "key" ]; command = "/run/wrappers/bin/light -U 10"; }
    ];
  };

  services.logind.extraConfig = "HandlePowerKey=suspend";

  powerManagement = { enable = true; cpuFreqGovernor = "ondemand"; };

  boot.initrd.kernelModules = [ "fbcon" ];
}