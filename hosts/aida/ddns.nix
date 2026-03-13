# Dynamic DNS — keeps vpn.yourdomain.com pointed at the router's WAN IP.
#
# Prerequisites:
#   - Cloudflare is the authoritative nameserver for yourdomain.com
#     (change NS records at name.com to Cloudflare's assigned nameservers —
#     this is free and requires no domain transfer)
#   - Create a scoped Cloudflare API token:
#       My Profile -> API Tokens -> Create Token
#       Permissions: Zone:DNS:Edit, Zone:Zone:Read
#       Zone Resources: Include -> Specific zone -> yourdomain.com
#   - Store the token in secrets/secrets.yaml under cloudflare/ddns-token

{ config, ... }:

{
  services.ddclient = {
    enable = true;

    # Check every 5 minutes
    interval = "5min";

    # Cloudflare API v4
    protocol = "cloudflare";

    # When using a scoped API token the username must be the literal string "token"
    username = "token";

    # Token is read from /run/secrets/cloudflare/ddns-token at runtime
    passwordFile = config.sops.secrets."cloudflare/ddns-token".path;

    domains = [
      # Only the VPN endpoint needs a public A record.
      # *.yourdomain.com stays as a private RFC1918 record (only reachable via VPN).
      "vpn.yourdomain.com"
    ];

    zone = "yourdomain.com";
    ssl = true;

    # Detect WAN IP from Cloudflare's trace endpoint (works behind NAT,
    # avoids relying on third-party IP-echo services)
    use = "web, web=https://cloudflare.com/cdn-cgi/trace, web-skip='ip='";
  };
}
