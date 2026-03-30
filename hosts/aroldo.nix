{ config, pkgs, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      KbdInteractiveAuthentication = false;
    };
    extraConfig = ''
      AllowUsers malloc47
    '';
  };

  services.fail2ban = {
    enable = true;
    maxretry = 5;
    bantime = "1h";
  };

  networking.usePredictableInterfaceNames = false;
  networking.networkmanager.enable = false;
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
  networking.firewall.allowedUDPPorts = [ 3478 ];
  networking.useDHCP = false;

  networking.interfaces.eth0.ipv4.addresses = [
    {
      address = "192.3.76.171";
      prefixLength = 24;
    }
  ];

  networking.defaultGateway = "192.3.76.1";
  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];

  age.secrets = {
    cloudflare-acme.file = ../secrets/cloudflare-acme.age;
  };
}
