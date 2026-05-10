{ config, pkgs, ... }:

{
  imports = [
    ../modules/settings.nix
    ../nixos/modules/motd.nix
    # Uncomment to enable netboot provisioning for bare-metal installs:
    # ../nixos/modules/netboot.nix
  ];

  services.openssh.enable = true;
  services.mosh-server.enable = true;

  environment.systemPackages = [ pkgs.adguardian ];

  motd = {
    enable = true;
    hardware = "gmktec g10 · mini pc";
    specs = [
      "8 cores"
      "12GB"
      "512GB"
    ];
    tags = [
      "dns"
      "proxy"
      "auth"
      "dashboard"
    ];
  };

  networking.networkmanager.enable = false;
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    53
    80
    443
  ];
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

    homepage-env.file = ../secrets/homepage-env.age;

    authelia-oidc-hmac = {
      file = ../secrets/authelia-oidc-hmac.age;
      owner = "authelia-main";
    };
    authelia-oidc-private-key = {
      file = ../secrets/authelia-oidc-private-key.age;
      owner = "authelia-main";
    };
    authelia-karakeep-secret-digest = {
      file = ../secrets/authelia-karakeep-secret-digest.age;
      owner = "authelia-main";
    };

    karakeep-oauth-env = {
      file = ../secrets/karakeep-oauth-env.age;
      owner = "karakeep";
    };
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

      identity_providers.oidc = {
        # Karakeep's OIDC implementation does not advertise a complete claim
        # set; Authelia's escape hatch forces email into the id_token.
        claims_policies.karakeep.id_token = [ "email" ];
        clients = [
          {
            client_id = "karakeep";
            client_name = "Karakeep";
            client_secret = ''{{ secret "${config.age.secrets.authelia-karakeep-secret-digest.path}" }}'';
            public = false;
            authorization_policy = "one_factor";
            require_pkce = true;
            pkce_challenge_method = "S256";
            redirect_uris = [
              "https://bookmarks.home.malloc47.com/api/auth/callback/custom"
            ];
            scopes = [
              "openid"
              "profile"
              "email"
            ];
            response_types = [ "code" ];
            grant_types = [ "authorization_code" ];
            access_token_signed_response_alg = "none";
            userinfo_signed_response_alg = "none";
            token_endpoint_auth_method = "client_secret_basic";
            claims_policy = "karakeep";
          }
        ];
      };
    };
    secrets = {
      jwtSecretFile = config.age.secrets.authelia-jwt-secret.path;
      storageEncryptionKeyFile = config.age.secrets.authelia-storage-key.path;
      sessionSecretFile = config.age.secrets.authelia-session-secret.path;
      oidcHmacSecretFile = config.age.secrets.authelia-oidc-hmac.path;
      oidcIssuerPrivateKeyFile = config.age.secrets.authelia-oidc-private-key.path;
    };
  };

  services.adguardhome = {
    enable = true;
    mutableSettings = false;
    settings = {
      http.address = "127.0.0.1:3000";
      dns = {
        bind_hosts = [ "0.0.0.0" ];
        port = 53;
        upstream_dns = [
          "https://cloudflare-dns.com/dns-query"
          "https://dns.google/dns-query"
          "[/lan/]192.168.1.1"
          "[/ts.malloc47.com/]100.100.100.100"
          "[//]192.168.1.1"
        ];
        bootstrap_dns = [
          "1.1.1.1"
          "8.8.8.8"
        ];
        hostsfile_enabled = false;
      };
      filtering = {
        rewrites_enabled = true;
        filtering_enabled = true;
        rewrites = [
          {
            enabled = true;
            domain = "*.home.malloc47.com";
            answer = "192.168.1.10";
          }
          # Advertise this on the internal tailscale node only
          {
            enabled = true;
            domain = "status.malloc47.com";
            answer = "100.64.0.3";
          }
          {
            enabled = true;
            domain = "smokeping.malloc47.com";
            answer = "100.64.0.3";
          }
        ];
      };
      filters = [
        {
          enabled = true;
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
          name = "AdGuard DNS filter";
          id = 1;
        }
        {
          enabled = false;
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_2.txt";
          name = "AdAway Default Blocklist";
          id = 2;
        }
      ];
      # https://www.reddit.com/r/NixOS/comments/1kvpoje/remove_default_hosts_mapping_for_hostname/
      clients.runtime_sources.hosts = false;
      clients.persistent = [
        {
          name = "Mercy Laptop";
          ids = [
            "192.168.1.120"
            "d0:57:7e:f0:99:98"
          ];
          tags = [
            "device_pc"
            "os_windows"
            "user_regular"
          ];
          uid = "019d0e43-082d-7048-9e20-4885d91a79bb";
          use_global_settings = true;
          filtering_enabled = false;
        }
        {
          name = "cesare";
          ids = [
            "192.168.1.167"
            "ae:d2:6f:c0:b4:32"
          ];
          tags = [
            "device_pc"
            "os_macos"
            "user_regular"
          ];
          uid = "019d0e3e-3fa2-7247-9283-092c7fa7b0a3";
          use_global_settings = true;
          filtering_enabled = false;
        }
      ];

      users = [
        {
          name = "admin";
          password = "$2y$10$qOUFASDwxLhkZQTcBd8tjOq/PpyxZfwn7WYFU8JD8URGbteF5GfRC";
        }
      ];
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
        Services = {
          style = "row";
          columns = 3;
        };
        Network = {
          style = "row";
          columns = 2;
        };
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
              href = "https://ntfy.malloc47.com";
              siteMonitor = "https://ntfy.malloc47.com";
            };
          }
        ];
      }
      {
        Network = [
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

  services.karakeep = {
    enable = true;
    environmentFile = config.age.secrets.karakeep-oauth-env.path;
    extraEnvironment = {
      PORT = "3001";
      # DISABLE_SIGNUPS = "true";
      DISABLE_NEW_RELEASE_CHECK = "true";

      NEXTAUTH_URL = "https://bookmarks.home.malloc47.com";
      OAUTH_WELLKNOWN_URL = "https://auth.home.malloc47.com/.well-known/openid-configuration";
      OAUTH_CLIENT_ID = "karakeep";
      OAUTH_PROVIDER_NAME = "Authelia";
      # Auto-link OIDC identity to a pre-existing local Karakeep account
      # sharing the same email (otherwise users get a duplicate empty account).
      OAUTH_ALLOW_DANGEROUS_EMAIL_ACCOUNT_LINKING = "true";
      # Leave password auth enabled until OIDC is verified end-to-end; flip to
      # "true" afterward to enforce SSO-only login.
      DISABLE_PASSWORD_AUTH = "true";
    };
  };

  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "server";
    # Initial registration: manually run tailscale up --authkey <key> with these same flags
    extraUpFlags = [
      "--login-server"
      "https://hs.malloc47.com"
      "--advertise-routes=192.168.1.0/24"
      "--advertise-exit-node"
      "--reset"
    ];
  };

  services.caddy = {
    enable = true;

    virtualHosts."auth.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:9091
      '';
    };

    virtualHosts."dash.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        handle /api/healthcheck {
          reverse_proxy http://127.0.0.1:8082
        }
        handle {
          forward_auth http://127.0.0.1:9091 {
            uri /api/authz/forward-auth
            copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
          }
          reverse_proxy http://127.0.0.1:8082
        }
      '';
    };

    virtualHosts."adguard.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        handle /login.html {
          reverse_proxy http://127.0.0.1:3000
        }
        handle {
          forward_auth http://127.0.0.1:9091 {
            uri /api/authz/forward-auth
            copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
          }
          reverse_proxy http://127.0.0.1:3000
        }
      '';
    };

    virtualHosts."bookmarks.home.malloc47.com" = {
      useACMEHost = "home.malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:3001
      '';
    };

  };
}
