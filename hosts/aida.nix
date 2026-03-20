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

  age.secrets = {
    authelia-jwt-secret = {
      file = ../secrets/authelia-jwt-secret.age;
      owner = "authelia-main";
    };
    authelia-storage-key = {
      file = ../secrets/authelia-storage-key.age;
      owner = "authelia-main";
    };
    authelia-session-secret = {
      file = ../secrets/authelia-session-secret.age;
      owner = "authelia-main";
    };
    authelia-users = {
      file = ../secrets/authelia-users.age;
      owner = "authelia-main";
    };

    caddy-basicauth = {
      file = ../secrets/caddy-basicauth.age;
      owner = "caddy";
    };
    cloudflare-acme.file = ../secrets/cloudflare-acme.age;
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "malloc47@gmail.com";
    certs."home.malloc47.com" = {
      domain = "*.home.malloc47.com";
      dnsProvider = "cloudflare";
      dnsResolver = "1.1.1.1:53";
      environmentFile = config.age.secrets.cloudflare-acme.path;
    };
  };


  services.authelia.instances.main = {
    enable = true;
    settings = {
      server.address = "tcp://127.0.0.1:9091";
      session.cookies = [
        {
          domain = "home.malloc47.com";
          authelia_url = "https://auth.home.malloc47.com";
        }
      ];
      storage.local.path = "/var/lib/authelia-main/db.sqlite3";
      authentication_backend.file.path = config.age.secrets.authelia-users.path;
      access_control.default_policy = "one_factor";
      notifier.filesystem.filename = "/var/lib/authelia-main/notifications.txt";
    };
    secrets = {
      jwtSecretFile = config.age.secrets.authelia-jwt-secret.path;
      storageEncryptionKeyFile = config.age.secrets.authelia-storage-key.path;
      sessionSecretFile = config.age.secrets.authelia-session-secret.path;
    };
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
          "[/lan/]192.168.1.1"
          "[//]192.168.1.1"
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
        hostsfile_enabled = false;
      };
      # https://www.reddit.com/r/NixOS/comments/1kvpoje/remove_default_hosts_mapping_for_hostname/
      clients.runtime_sources.hosts = false;
    };
  };

  services.ntfy-sh = {
    enable = true;
    settings = {
      base-url = "https://ntfy.home.malloc47.com";
      listen-http = "127.0.0.1:2586";
      behind-proxy = true;
      auth-default-access = "deny-all";
    };
  };

  services.caddy = {
    enable = true;

    virtualHosts."auth.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:9091
      '';
    };

    virtualHosts."ntfy.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:2586
      '';
    };

    virtualHosts."adguard.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        forward_auth http://127.0.0.1:9091 {
          uri /api/authz/forward-auth
          copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
        }
        reverse_proxy http://127.0.0.1:3000
      '';
    };

  };
}
