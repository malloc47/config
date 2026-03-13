# Podman container runtime for aida.
#
# All service containers use the OCI containers abstraction so they run as
# systemd units (container-<name>.service) and integrate with journald.
#
# Data layout on /data (second NVMe — see disk/data-drive.nix):
#   /data/immich/      — Immich upload storage, ML model cache, Postgres DB
#   /data/jellyfin/    — Jellyfin config and metadata
#   /data/media/       — Jellyfin media library (movies, TV, music)
#   /data/actual/      — Actual Budget database
#   /data/syncthing/   — Syncthing data and config
#   /data/n8n/         — n8n workflows and credentials
#
# Before the second NVMe arrives, /data is just a directory on the root FS.
# Once the drive is installed, add disk/data-drive.nix to the flake and
# run `nixos-rebuild switch` — the mount replaces the directory transparently.

{ ... }:

{
  virtualisation.podman = {
    enable = true;
    # Provides a `docker` shim for any tooling that expects the Docker CLI
    dockerCompat = true;
    # Prune stopped containers and dangling images weekly
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
  };

  virtualisation.oci-containers.backend = "podman";

  # Ensure the data root exists even before the second NVMe is mounted.
  # The systemd units for containers all depend on /data being present.
  systemd.tmpfiles.rules = [
    "d /data                0755 root root -"
    "d /data/immich         0755 root root -"
    "d /data/immich/upload  0755 root root -"
    "d /data/immich/cache   0755 root root -"
    "d /data/immich/db      0755 root root -"
    "d /data/jellyfin       0755 root root -"
    "d /data/media          0755 root root -"
    "d /data/actual         0755 root root -"
    "d /data/n8n            0755 root root -"
  ];
}
