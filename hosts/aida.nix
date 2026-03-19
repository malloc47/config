{ config, pkgs, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  services.openssh.enable = true;

  networking.networkmanager.enable = false;
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.useDHCP = false;

  networking.interfaces.eno1.ipv4.addresses = [
    {
      address = "192.168.1.10";
      prefixLength = 24;
    }
  ];

  networking.defaultGateway = "192.168.1.1";
  networking.nameservers = [ "192.168.1.1" ];

  age.secrets.caddy-basicauth = {
    file = ../secrets/caddy-basicauth.age;
    owner = "caddy";
  };

  services.caddy = {
    enable = true;
    virtualHosts."http://localhost" = {
      extraConfig = ''
        basicauth {
          import ${config.age.secrets.caddy-basicauth.path}
        }
        respond "Hello, world!"
      '';
    };
  };
}
