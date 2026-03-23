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

    cloudflared-credentials.file = ../secrets/cloudflared-credentials.age;

    homepage-env.file = ../secrets/homepage-env.age;
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

      users = [
        {
          name = "admin";
          password = "$2y$10$qOUFASDwxLhkZQTcBd8tjOq/PpyxZfwn7WYFU8JD8URGbteF5GfRC";
        }
      ];
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

  services.homepage-dashboard = {
    enable = true;
    listenPort = 8082;
    allowedHosts = "dash.home.malloc47.com";
    environmentFile = config.age.secrets.homepage-env.path;

    settings = {
      title = "Unimatrix";
      headerStyle = "clean";
      layout = {
        Services = { style = "row"; columns = 3; };
        Network = { style = "row"; columns = 2; };
      };
    };

    widgets = [
      {
        resources = {
          cpu = true;
          memory = true;
          disk = "/";
          cputemp = true;
          uptime = true;
        };
      }
      {
        search = {
          provider = "google";
          focus = true;
          showSearchSuggestions = true;
          target = "_blank";
        };
      }
    ];

    services = [
      {
        Services = [
          {
            "AdGuard Home" = {
              icon = "adguard-home";
              href = "https://adguard.home.malloc47.com";
              siteMonitor = "http://127.0.0.1:3000";
              widget = {
                type = "adguard";
                url = "http://127.0.0.1:3000";
                username = "{{HOMEPAGE_VAR_ADGUARD_USER}}";
                password = "{{HOMEPAGE_VAR_ADGUARD_PASS}}";
              };
            };
          }
          {
            "Authelia" = {
              icon = "authelia";
              href = "https://auth.home.malloc47.com";
              siteMonitor = "http://127.0.0.1:9091";
            };
          }
          {
            "ntfy" = {
              icon = "ntfy";
              href = "https://ntfy.home.malloc47.com";
              siteMonitor = "http://127.0.0.1:2586";
            };
          }
        ];
      }
      {
        Network = [
          {
            "Cloudflare Tunnel" = {
              icon = "cloudflare";
              href = "https://one.dash.cloudflare.com";
              widget = {
                type = "cloudflared";
                accountid = "{{HOMEPAGE_VAR_CF_ACCOUNT_ID}}";
                tunnelid = "eaf269c8-e39f-4a4d-8a06-a3124655e590";
                key = "{{HOMEPAGE_VAR_CF_API_TOKEN}}";
              };
            };
          }
          {
            "OpenWRT Router" = {
              icon = "openwrt";
              href = "http://192.168.1.1";
              ping = "192.168.1.1";
            };
          }
        ];
      }
    ];
  };

  services.cloudflared = {
    enable = true;
    tunnels."eaf269c8-e39f-4a4d-8a06-a3124655e590" = {
      credentialsFile = config.age.secrets.cloudflared-credentials.path;
      ingress = {
        "ntfy-ext.malloc47.com" = "http://localhost:2586";
      };
      default = "http_status:404";
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

    virtualHosts."dash.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        forward_auth http://127.0.0.1:9091 {
          uri /api/authz/forward-auth
          copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
        }
        reverse_proxy http://127.0.0.1:8082
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
