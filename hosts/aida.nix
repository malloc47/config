{ config, pkgs, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  services.openssh.enable = true;

  networking.networkmanager.enable = false;
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 53 80 443 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
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

  services.adguardhome = {
    enable = true;
    settings = {
      http.address = "127.0.0.1:3000";
      dns = {
        bind_hosts = [ "0.0.0.0" ];
        port = 53;
        upstream_dns = [
          "https://cloudflare-dns.com/dns-query"
          "https://dns.google/dns-query"
        ];
        bootstrap_dns = [
          "1.1.1.1"
          "8.8.8.8"
        ];
        rewrites = [
          {
            domain = "*.home.malloc47.com";
            answer = "192.168.1.10";
          }
        ];
      };
    };
  };

  services.caddy = {
    enable = true;
    virtualHosts."http://adguard.home.malloc47.com" = {
      extraConfig = ''
        reverse_proxy http://127.0.0.1:3000
      '';
    };
  };
}
