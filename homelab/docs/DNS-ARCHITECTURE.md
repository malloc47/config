# DNS Architecture

This document describes how DNS resolution works across all managed hosts
and client devices, both on-LAN and off-LAN.

## Components

| Component | Host | Address | Role |
|-----------|------|---------|------|
| AdGuard Home | aida | 192.168.1.10 (LAN) / 100.64.0.1 (Tailscale) | Ad-blocking DNS resolver, DNS rewrites for internal domains |
| OpenWRT DNS | router | 192.168.1.1 | DHCP-assigned DNS for LAN clients, upstream hostname resolution for AdGuard |
| Headscale MagicDNS | aroldo | 100.100.100.100 (virtual) | Tailscale-internal DNS, forwards to `nameservers.global` |
| Cloudflare/Google DoH | public | 1.1.1.1 / 8.8.8.8 | Upstream resolvers used by AdGuard |

## DNS Rewrites (AdGuard)

| Domain | Answer | Purpose |
|--------|--------|---------|
| `*.home.malloc47.com` | 192.168.1.10 | Route all home service subdomains to aida (Caddy) |
| `status.malloc47.com` | 100.64.0.3 | Route Uptime Kuma to aroldo via Tailscale |

## DNS Flow by Device and Location

### LAN Clients (cesare, phone, Mercy Laptop) -- On LAN

```
App ──> OS Resolver
         │
         ▼
    OpenWRT DHCP assigns DNS = 192.168.1.10
         │
         ▼
    AdGuard Home (aida, 192.168.1.10)
         │
         ├── *.home.malloc47.com ──> 192.168.1.10 (rewrite)
         ├── status.malloc47.com ──> 100.64.0.3  (rewrite, Tailscale)
         ├── ad/tracker domains ──> blocked
         └── everything else ──> Cloudflare DoH / Google DoH
```

Devices on the LAN get DNS from AdGuard via DHCP. All queries benefit
from ad-blocking. Internal domains resolve via AdGuard rewrites.

### LAN Clients (cesare, phone) -- Off LAN, Tailscale Active

```
App ──> OS Resolver
         │
         ▼
    Tailscale intercepts DNS
         │
         ▼
    MagicDNS (100.100.100.100)
         │
         ▼
    Headscale nameservers.global = 100.64.0.1
         │
         ▼
    AdGuard Home (aida, via Tailscale tunnel)
         │
         ├── *.home.malloc47.com ──> 192.168.1.10 (rewrite)
         │        └── reachable via aida's advertised 192.168.1.0/24 route
         ├── status.malloc47.com ──> 100.64.0.3  (rewrite, Tailscale)
         ├── ad/tracker domains ──> blocked
         └── everything else ──> Cloudflare DoH / Google DoH
```

When off the LAN with Tailscale connected, MagicDNS forwards all queries
to AdGuard via Tailscale tunnel. Ad-blocking continues to work. Internal
domains resolve via the same rewrites, with traffic routed through
Tailscale's subnet routing (192.168.1.0/24 advertised by aida).

### LAN Clients -- Off LAN, Tailscale Inactive

```
App ──> OS Resolver
         │
         ▼
    ISP / public DNS (hotel WiFi, cellular, etc.)
         │
         └── No ad-blocking, no internal domain access
```

### aroldo (VPS)

```
App ──> OS Resolver (/etc/resolv.conf)
         │
         ├── primary: 100.64.0.1 (AdGuard via Tailscale)
         │      │
         │      ├── *.home.malloc47.com ──> 192.168.1.10 (rewrite)
         │      ├── status.malloc47.com ──> 100.64.0.3  (rewrite)
         │      └── everything else ──> Cloudflare DoH / Google DoH
         │
         └── fallback: 1.1.1.1 (if Tailscale tunnel to aida is down)
```

aroldo uses `--accept-dns=false` to prevent MagicDNS from overwriting
`/etc/resolv.conf`. DNS is configured via `networking.nameservers` in
NixOS with AdGuard over Tailscale as primary and Cloudflare as fallback.
This ensures aroldo retains internet access even if the Tailscale tunnel
to aida goes down.

### aida (Homelab Server)

```
App ──> OS Resolver
         │
         ▼
    localhost / systemd-resolved
         │
         ▼
    AdGuard Home (127.0.0.1:53 / 0.0.0.0:53)
         │
         ├── *.home.malloc47.com ──> 192.168.1.10 (self)
         └── everything else ──> Cloudflare DoH / Google DoH
                                  └── [/lan/] ──> 192.168.1.1 (OpenWRT)
```

## Headscale DNS Configuration

Defined in `hosts/aroldo.nix` under `services.headscale.settings.dns`:

```yaml
magic_dns: true
base_domain: ts.malloc47.com
nameservers:
  global:
    - 100.64.0.1    # AdGuard via Tailscale IP
```

MagicDNS provides `<hostname>.ts.malloc47.com` resolution for all
tailnet nodes. The global nameserver ensures off-LAN clients route
all other DNS through AdGuard for ad-blocking.

## Key Design Decisions

1. **`nameservers.global` uses Tailscale IP (100.64.0.1), not LAN IP
   (192.168.1.10)**: The LAN IP is unreachable from off-LAN clients
   and from aroldo. The Tailscale IP is reachable from any tailnet node.

2. **aroldo uses `--accept-dns=false`**: Prevents MagicDNS from
   overwriting `/etc/resolv.conf`. Instead, NixOS `networking.nameservers`
   controls DNS with a Cloudflare fallback for resilience.

3. **AdGuard `mutableSettings = false`**: NixOS config is authoritative.
   All rewrites, filters, and client config must be defined in
   `hosts/aida.nix`. Changes via the web UI are overwritten on deploy.

4. **Gatus monitoring on aroldo**: `*.home.malloc47.com` resolves to the
   LAN IP (192.168.1.10) via AdGuard rewrite, which is not routable from
   aroldo. `networking.hosts` on aroldo overrides these to aida's
   Tailscale IP (100.64.0.1), allowing Gatus to reach aida services
   using real domain names with correct SNI/TLS.

5. **Auth-protected endpoints return 401**: Services behind Authelia
   (Homepage, AdGuard) return 401 to non-browser requests without a
   session. Gatus monitors these with `[STATUS] == 401` to confirm the
   service + auth layer are both responding.

## Future: App Connectors

Headscale PR #3121 (targeting v0.31.0) adds Tailscale app connector
support. Once available, domain-based routing (e.g., `status.malloc47.com`)
could move from AdGuard DNS rewrites to Headscale ACL policy, removing
the dependency on the DNS layer for service routing.
