# Caddy reverse proxy with automatic TLS via Cloudflare DNS-01 challenge.
#
# DNS-01 is used instead of HTTP-01 because no inbound port 80/443 is open
# from the WAN — Caddy proves domain ownership by writing a temporary TXT
# record via the Cloudflare API. All internal service names resolve to
# 192.168.1.10 (split DNS) and are only reachable on LAN or via WireGuard.
#
# Caddy is built with the caddy-dns/cloudflare plugin. First build:
#   nix build .#nixosConfigurations.aida.config.services.caddy.package
# If the vendorHash is wrong, the error message shows the correct hash.
#
# To use name.com API instead of Cloudflare (if you keep name.com as NS):
#   Replace the plugin with "github.com/caddy-dns/namedotcom@latest" and
#   swap `dns cloudflare` with:
#     dns namedotcom {
#       api_token {env.NAMECOM_API_TOKEN}
#       user     {env.NAMECOM_USER}
#     }

{ config, pkgs, lib, ... }:

let
  # Pin the plugin version for reproducibility. To find the correct
  # vendorHash, set it to pkgs.lib.fakeHash, run the build, and replace
  # with the hash from the error output.
  caddyWithCloudflare = pkgs.caddy.withPlugins {
    plugins = [ "github.com/caddy-dns/cloudflare@v0.2.1" ];
    vendorHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; # TODO: replace
  };

  # Shared TLS block for all virtual hosts
  tlsCloudflare = ''
    tls {
      dns cloudflare {env.CLOUDFLARE_API_TOKEN}
    }
  '';

in

{
  services.caddy = {
    enable = true;
    package = caddyWithCloudflare;

    globalConfig = ''
      email you@yourdomain.com
    '';

    virtualHosts = {
      "immich.yourdomain.com".extraConfig = ''
        ${tlsCloudflare}
        reverse_proxy localhost:2283
      '';

      "jellyfin.yourdomain.com".extraConfig = ''
        ${tlsCloudflare}
        reverse_proxy localhost:8096
      '';

      "actual.yourdomain.com".extraConfig = ''
        ${tlsCloudflare}
        reverse_proxy localhost:5006
      '';

      "syncthing.yourdomain.com".extraConfig = ''
        ${tlsCloudflare}
        # Syncthing's API requires the Host header to match; Caddy sets it.
        reverse_proxy localhost:8384 {
          header_up Host {upstream_hostport}
        }
      '';

      "hass.yourdomain.com".extraConfig = ''
        ${tlsCloudflare}
        reverse_proxy localhost:8123
      '';

      "n8n.yourdomain.com".extraConfig = ''
        ${tlsCloudflare}
        reverse_proxy localhost:5678
      '';
    };
  };

  # Inject the Cloudflare token as an environment variable at runtime.
  # The token never touches the Nix store (it's decrypted to /run/secrets by sops-nix).
  systemd.services.caddy.serviceConfig.EnvironmentFile =
    config.sops.secrets."cloudflare/caddy-env".path;
}
