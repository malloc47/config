# Secret management via sops-nix.
#
# First-time setup on aida:
#
#   1. Generate an age keypair:
#        mkdir -p /var/lib/sops-nix
#        age-keygen -o /var/lib/sops-nix/key.txt
#        cat /var/lib/sops-nix/key.txt | grep 'public key'
#
#   2. Create .sops.yaml at the repo root with that public key:
#        keys:
#          - &aida age1<YOUR_PUBLIC_KEY>
#        creation_rules:
#          - path_regex: secrets/.*\.yaml$
#            key_groups:
#              - age: [*aida]
#
#   3. Create and encrypt secrets:
#        mkdir -p secrets
#        sops secrets/secrets.yaml
#        # Add the keys listed in sops.secrets below, then save and exit.
#
#   4. nixos-rebuild switch — sops-nix decrypts to /run/secrets/ at boot.

{ config, ... }:

{
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    age.keyFile = "/var/lib/sops-nix/key.txt";

    secrets = {
      # Scoped Cloudflare API token for ddclient (Zone:DNS:Edit on your zone)
      "cloudflare/ddns-token" = {
        owner = "ddclient";
      };

      # Environment file consumed by the Caddy systemd unit.
      # Contents: CLOUDFLARE_API_TOKEN=<token with Zone:DNS:Edit>
      "cloudflare/caddy-env" = {
        owner = "caddy";
      };

      # Immich Postgres password
      "immich/db-password" = {
        owner = "root"; # containers run as root in Podman rootful mode
      };
    };
  };
}
