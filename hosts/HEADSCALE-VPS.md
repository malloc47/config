# Headscale on VPS — Reference Configuration

Self-hosted Tailscale control plane + DERP relay on a VPS, providing seamless remote access to the home LAN from restrictive networks.

## Why a VPS?

Headscale requires WebSocket passthrough for the Tailscale noise protocol (ts2021). Cloudflare Tunnels strip the Upgrade header, making them incompatible — see https://headscale.net/0.23.0/ref/integration/reverse-proxy/#cloudflare. A VPS with direct TLS termination avoids this.

## Architecture

```
[phone/laptop on any network]
    |  Tailscale client (direct WireGuard or DERP relay over TCP 443)
    v
[VPS: Headscale + DERP + Caddy on TCP 443]
    |  coordinates keys, relays when direct connection fails
    v
[aida: Tailscale client + subnet router]
    |  advertises 192.168.1.0/24
    v
[LAN: 192.168.1.0/24]
```

- On open networks: Tailscale clients connect peer-to-peer (direct WireGuard)
- On restrictive networks (UDP blocked): traffic routes through DERP relay on VPS over TCP 443
- Mobile clients (Android Tailscale app) handle direct/DERP fallback automatically — single toggle, no manual switching
- Zero new inbound ports on OpenWRT — aida connects outbound to VPS

## VPS NixOS Configuration

### Headscale server

```nix
services.headscale = {
  enable = true;
  address = "127.0.0.1";
  port = 8085;
  settings = {
    server_url = "https://hs.malloc47.com";  # adjust to VPS hostname
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
      # Keep Tailscale's public DERP map for STUN (NAT traversal for direct connections)
      urls = [ "https://controlplane.tailscale.com/derpmap/default" ];
    };
    dns = {
      magic_dns = true;
      base_domain = "ts.malloc47.com";
      nameservers.global = [ "192.168.1.10" ];  # aida's AdGuard
    };
    policy = {
      mode = "file";
      path = pkgs.writeText "headscale-acl.hujson" (builtins.toJSON {
        acls = [
          { action = "accept"; src = [ "*" ]; dst = [ "*:*" ]; }
        ];
        autoApprovers = {
          routes = {
            "192.168.1.0/24" = [ "default@" ];
          };
          exitNode = [ "default@" ];
        };
      });
    };
  };
};
```

### Bootstrap oneshot (user + pre-auth key)

Creates the Headscale user and generates a pre-auth key automatically on first boot. The key is written to `/run/headscale/authkey` for the local Tailscale client (if the VPS also runs one), or can be retrieved for agenix-managed distribution to other hosts.

```nix
systemd.services.headscale-bootstrap = {
  description = "Create Headscale user and pre-auth key for Tailscale auto-enrollment";
  after = [ "headscale.service" ];
  requires = [ "headscale.service" ];
  wantedBy = [ "multi-user.target" ];
  path = [ config.services.headscale.package ];
  serviceConfig = {
    Type = "oneshot";
    RemainAfterExit = true;
  };
  script = ''
    # headscale.service is Type=simple, so systemd considers it "started"
    # before the API is ready — poll until it responds
    until headscale users list >/dev/null 2>&1; do sleep 1; done

    # Create user if it doesn't exist
    if ! headscale users list -o json | grep -q '"name":"default"'; then
      headscale users create default
    fi

    # Look up user ID (headscale CLI takes numeric IDs, not names)
    USER_ID=$(headscale users list -o json-line | grep '"name":"default"' | sed -n 's/.*"id":\([0-9]*\).*/\1/p')

    # Generate a pre-auth key and write to a file for Tailscale
    KEY=$(headscale preauthkeys create --user "$USER_ID" --reusable --expiration 24h -o json-line | sed -n 's/.*"key":"\([^"]*\)".*/\1/p')
    echo -n "$KEY" > /run/headscale/authkey
    chmod 600 /run/headscale/authkey
  '';
};
```

### Caddy reverse proxy (TLS + WebSocket)

Caddy natively supports WebSocket proxying, which Headscale's ts2021 noise protocol requires.

```nix
services.caddy = {
  enable = true;
  virtualHosts."hs.malloc47.com" = {
    extraConfig = ''
      reverse_proxy http://127.0.0.1:8085
    '';
  };
};
```

### Firewall

```nix
networking.firewall = {
  enable = true;
  allowedTCPPorts = [ 80 443 ];
  allowedUDPPorts = [ 3478 ];  # STUN for DERP
};
```

## aida Configuration (Tailscale client + subnet router)

Add to `hosts/aida.nix`:

```nix
services.tailscale = {
  enable = true;
  openFirewall = true;
  useRoutingFeatures = "server";
  authKeyFile = config.age.secrets.headscale-preauthkey.path;  # or /run/headscale/authkey if local
  extraUpFlags = [
    "--login-server" "https://hs.malloc47.com"
    "--advertise-routes=192.168.1.0/24"
    "--advertise-exit-node"
    "--reset"
  ];
};
```

Key details:
- `useRoutingFeatures = "server"` enables IP forwarding for subnet routing
- `--reset` makes `tailscale up` idempotent across redeploys
- `--advertise-routes` and `--advertise-exit-node` must go in `extraUpFlags`, not `extraSetFlags` (they conflict otherwise)
- Route and exit node approval is handled by `autoApprovers` in the ACL policy — no manual `headscale routes enable` needed

## Client Enrollment

### Devices with NixOS (aida, attila)

Use `services.tailscale.authKeyFile` with a pre-auth key stored via agenix for fully declarative enrollment. The `headscale-bootstrap` oneshot generates keys; store one as `headscale-preauthkey.age`.

### Android phone

1. Install Tailscale from Play Store
2. Three-dot menu > Use another server > `https://hs.malloc47.com`
3. Login > Headscale shows a registration URL with a node key
4. On VPS: `sudo headscale nodes register --user default --key nodekey:<key>`

### macOS laptop

```bash
tailscale up --login-server https://hs.malloc47.com
# On VPS: sudo headscale nodes register --user default --key nodekey:<key>
```

## Lessons Learned

- **Cloudflare Tunnels are incompatible** with Headscale — they strip WebSocket Upgrade headers required by the ts2021 noise protocol. Error: "No Upgrade header in TS2021 request."
- **ACL policy user references** require `@` suffix: `"default@"` not `"default"`. Error: "Invalid AutoApprover."
- **`tailscale up` flags**: put all config (`--login-server`, `--advertise-routes`, `--advertise-exit-node`) in `extraUpFlags` with `--reset`. Using `extraSetFlags` for routes/exit-node conflicts with `extraUpFlags`.
- **Headscale `Type=simple`**: systemd considers it started before the API is ready. Any dependent oneshot must poll for readiness.
- **Pre-auth keys expire** (default/max 24h). Existing enrolled nodes are unaffected — the key is only used for initial registration. The bootstrap oneshot regenerates on each boot.
