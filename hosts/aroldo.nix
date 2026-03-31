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

  security.acme = {
    acceptTerms = true;
    defaults.email = "malloc47@gmail.com";
    certs."hs.malloc47.com" = {
      domain = "hs.malloc47.com";
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
        nameservers.global = [ "192.168.1.10" ];
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

      if ! headscale users list -o json | grep -q '"name":"default"'; then
        headscale users create default
      fi

      USER_ID=$(headscale users list -o json-line | grep '"name":"default"' | sed -n 's/.*"id":\([0-9]*\).*/\1/p')
      KEY=$(headscale preauthkeys create --user "$USER_ID" --reusable --expiration 24h -o json-line | sed -n 's/.*"key":"\([^"]*\)".*/\1/p')
      echo -n "$KEY" > /run/headscale/authkey
      chmod 600 /run/headscale/authkey
    '';
  };

  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "server";
    # Initial registration: manually run tailscale up --authkey <key> with these same flags
    extraUpFlags = [
      "--login-server"
      "https://hs.malloc47.com"
      "--advertise-exit-node"
      "--reset"
    ];
  };

  services.caddy = {
    enable = true;
    virtualHosts."hs.malloc47.com" = {
      useACMEHost = "hs.malloc47.com";
      extraConfig = ''
        reverse_proxy http://127.0.0.1:8085
      '';
    };
  };
}
