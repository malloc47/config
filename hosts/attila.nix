{ config, pkgs, ... }:

{
  imports = [
    ../modules/settings.nix
    ../nixos/modules/motd.nix
    ../nixos/modules/tailscale-client.nix
  ];

  motd = {
    enable = true;
    hardware = "dell xps 13 9315 · laptop";
    specs = [
      "12 cores"
      "16GB"
      "512GB"
    ];
    tags = [
      "headless"
      "wifi"
      "battery"
      "tailscale"
    ];
  };

  services.tailscale-client = {
    enable = true;
    loginServer = "https://hs.malloc47.com";
  };

  services.openssh.enable = true;
  services.mosh-server.enable = true;

  # Keep systemd --user services (e.g. emacs daemon) running after logout
  users.users.${config.settings.username}.linger = true;

  # Headless: ignore lid close
  services.logind.settings.Login = {
    HandleLidSwitch = "ignore";
    HandleLidSwitchDocked = "ignore";
    HandleLidSwitchExternalPower = "ignore";
  };

  # Battery: cap charge at ~80% to reduce degradation from 24/7 operation
  services.tlp = {
    enable = true;
    settings = {
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };

  age.secrets.wifi-unimatrix47 = {
    file = ../secrets/wifi-unimatrix47.age;
    path = "/var/lib/iwd/Unimatrix47-5ghz.psk";
    mode = "0600";
  };

  # WiFi networking
  networking.wireless.iwd.enable = true;
  networking.useNetworkd = true;
  systemd.network = {
    enable = true;
    networks."20-wlan" = {
      matchConfig.Type = "wlan";
      networkConfig = {
        DHCP = "yes";
      };
    };
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ ];
    allowedUDPPorts = [ ];
  };

  # Suspend bug workaround: prevent spurious wake from I2C device
  services.udev.extraRules = ''
    SUBSYSTEM=="i2c", DRIVERS=="i2c_hid_acpi", ATTR{power/wakeup}="disabled"
  '';

  # Disable display/GPU power for headless operation
  boot.kernelParams = [
    "consoleblank=60"
    # Disable Intel audio entirely — prevents SoundWire enumeration timeout on boot
    "snd_intel_dspcfg.dsp_driver=0"
    "snd_hda_intel.enable=0"
  ];

  # Prevent SoundWire and audio modules from loading
  boot.blacklistedKernelModules = [
    "snd_sof_pci_intel_tgl"
    "snd_sof_intel_hda_common"
    "snd_sof"
    "soundwire_intel"
    "soundwire_cadence"
    "soundwire_bus"
  ];

}
