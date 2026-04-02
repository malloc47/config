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
    22
    80
    443
  ];
  networking.firewall.allowedUDPPorts = [ 3478 ];
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ 443 ];
  networking.useDHCP = false;

  networking.interfaces.eth0.ipv4.addresses = [
    {
      address = "192.3.76.171";
      prefixLength = 24;
    }
  ];

  networking.defaultGateway = "192.3.76.1";
  networking.nameservers = [
    "100.64.0.1"
    "1.1.1.1"
  ];

  # Override DNS for aida services to use Tailscale IP instead of
  # unreachable LAN IP (192.168.1.10) from AdGuard rewrite
  networking.hosts = {
    "100.64.0.1" = [
      "auth.home.malloc47.com"
      "dash.home.malloc47.com"
      "adguard.home.malloc47.com"
    ];
    "127.0.0.1" = [
      "smokeping.malloc47.com"
    ];
  };

  age.secrets = {
    cloudflare-acme.file = ../secrets/cloudflare-acme.age;
    cloudflared-credentials.file = ../secrets/cloudflared-credentials.age;
    ntfy-admin-password.file = ../secrets/ntfy-admin-password.age;
    ntfy-admin-password-env.file = ../secrets/ntfy-admin-password-env.age;
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "malloc47@gmail.com";
    certs."malloc47.com" = {
      domain = "*.malloc47.com";
      dnsProvider = "cloudflare";
      dnsResolver = "1.1.1.1:53";
      environmentFile = config.age.secrets.cloudflare-acme.path;
    };
  };

  services.headscale = {
    enable = true;
    address = "127.0.0.1";
    port = 8085;
    settings = {
      server_url = "https://hs.malloc47.com";
      prefixes = {
        v4 = "100.64.0.0/10";
        v6 = "fd7a:115c:a1e0::/48";
      };
      derp = {
        server = {
          enabled = true;
          region_id = 999;
          stun_listen_addr = "0.0.0.0:3478";
        };
        urls = [ "https://controlplane.tailscale.com/derpmap/default" ];
      };
      dns = {
        magic_dns = true;
        base_domain = "ts.malloc47.com";
        nameservers.global = [ "100.64.0.1" ];
      };
      policy = {
        mode = "file";
        path = pkgs.writeText "headscale-acl.hujson" (
          builtins.toJSON {
            acls = [
              {
                action = "accept";
                src = [ "*" ];
                dst = [ "*:*" ];
              }
            ];
            autoApprovers = {
              routes = {
                "192.168.1.0/24" = [ "default@" ];
              };
              exitNode = [ "default@" ];
            };
          }
        );
      };
    };
  };

  systemd.services.headscale-bootstrap = {
    description = "Create Headscale user and pre-auth key for Tailscale auto-enrollment";
    after = [ "headscale.service" ];
    requires = [ "headscale.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ config.services.headscale.package ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      RuntimeDirectory = "headscale";
    };
    script = ''
      until headscale users list >/dev/null 2>&1; do sleep 1; done

      if ! headscale users list -o json-line | grep -q '"name":"default"'; then
        headscale users create default
      fi

      USER_ID=$(headscale users list -o json-line | grep '"name":"default"' | sed -n 's/.*"id":\([0-9]*\).*/\1/p')
      KEY=$(headscale preauthkeys create --user "$USER_ID" --reusable --expiration 24h -o json-line | sed -n 's/.*"key":"\([^"]*\)".*/\1/p')
      echo -n "$KEY" > /run/headscale/authkey
      chmod 600 /run/headscale/authkey
    '';
  };

  services.cloudflared = {
    enable = true;
    tunnels."eaf269c8-e39f-4a4d-8a06-a3124655e590" = {
      credentialsFile = config.age.secrets.cloudflared-credentials.path;
      ingress = {
        "ntfy.malloc47.com" = "http://localhost:2586";
      };
      default = "http_status:404";
    };
  };

  services.ntfy-sh = {
    enable = true;
    settings = {
      base-url = "https://ntfy.malloc47.com";
      listen-http = "127.0.0.1:2586";
      behind-proxy = true;
      auth-default-access = "deny-all";
    };
  };

  systemd.services.ntfy-bootstrap = {
    description = "Create ntfy admin user and access rules";
    after = [ "ntfy-sh.service" ];
    requires = [ "ntfy-sh.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    path = [ config.services.ntfy-sh.package ];
    script = ''
      if ! ntfy user list 2>/dev/null | grep -q "user admin"; then
        NTFY_PASSWORD="$(cat ${config.age.secrets.ntfy-admin-password.path})" ntfy user add --role=admin admin
      fi
    '';
  };

  services.gatus = {
    enable = true;
    environmentFile = config.age.secrets.ntfy-admin-password-env.path;
    settings = {
      alerting.ntfy = {
        url = "http://127.0.0.1:2586";
        topic = "alerts";
        priority = 3;
        token = "$NTFY_TOKEN";
        default-alert = {
          enabled = true;
          failure-threshold = 2;
          success-threshold = 3;
          send-on-resolved = true;
        };
      };
      web.port = 3001;
      endpoints = [
        {
          name = "Headscale";
          group = "aroldo";
          url = "https://hs.malloc47.com/health";
          interval = "5m";
          conditions = [ "[STATUS] == 200" ];
          alerts = [ { type = "ntfy"; } ];
        }
        {
          name = "Authelia";
          group = "aida";
          url = "https://auth.home.malloc47.com";
          interval = "5m";
          conditions = [ "[STATUS] == 200" ];
          alerts = [ { type = "ntfy"; } ];
        }
        {
          name = "Homepage";
          group = "aida";
          url = "https://dash.home.malloc47.com/api/healthcheck";
          interval = "5m";
          conditions = [ "[STATUS] == 200" ];
          alerts = [ { type = "ntfy"; } ];
        }
        {
          name = "ntfy";
          group = "aroldo";
          url = "http://127.0.0.1:2586/v1/health";
          interval = "5m";
          conditions = [ "[STATUS] == 200" ];
          alerts = [ { type = "ntfy"; } ];
        }
        {
          name = "AdGuard DNS";
          group = "aida";
          url = "https://adguard.home.malloc47.com/login.html";
          interval = "5m";
          conditions = [ "[STATUS] == 200" ];
          alerts = [ { type = "ntfy"; } ];
        }
        {
          name = "Smokeping";
          group = "aroldo";
          url = "https://smokeping.malloc47.com/smokeping.fcgi";
          interval = "5m";
          conditions = [ "[STATUS] == 200" ];
          alerts = [ { type = "ntfy"; } ];
        }
      ];
    };
  };

  services.smokeping = {
    enable = true;
    webService = false;
    hostName = "smokeping.malloc47.com";
    owner = "malloc47";
    ownerEmail = "malloc47@gmail.com";
    targetConfig = ''
      probe = FPing
      menu = Top
      title = Network Latency Grapher

      + Tailscale
      menu = Tailscale
      title = Tailscale Tunnel Latency

      ++ aida
      title = aida (homelab)
      host = 100.64.0.1

      + Internet
      menu = Internet
      title = Internet Endpoints

      ++ CloudflareDNS
      title = Cloudflare DNS
      host = 1.1.1.1

      ++ GoogleDNS
      title = Google DNS
      host = 8.8.8.8
    '';
  };

  services.fcgiwrap.instances.smokeping = {
    process = {
      user = "smokeping";
      group = "smokeping";
    };
    socket = {
      user = "caddy";
      group = "caddy";
    };
  };

  users.users.caddy.extraGroups = [ "smokeping" ];

  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "server";
    # Initial registration: manually run tailscale up --authkey <key> with these same flags
    extraUpFlags = [
      "--login-server"
      "https://hs.malloc47.com"
      "--advertise-exit-node"
      "--accept-dns=false"
      "--reset"
    ];
  };

  services.caddy = {
    enable = true;
    virtualHosts."hs.malloc47.com" = {
      useACMEHost = "malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:8085
      '';
    };
    virtualHosts."status.malloc47.com" = {
      useACMEHost = "malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:3001
      '';
    };
    virtualHosts."smokeping.malloc47.com" = {
      useACMEHost = "malloc47.com";
      extraConfig = ''
        root * /var/lib/smokeping
        reverse_proxy unix/${config.services.fcgiwrap.instances.smokeping.socket.address} {
          transport fastcgi {
            split .fcgi
          }
        }
        file_server
      '';
    };
  };
}
