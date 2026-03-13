# Syncthing — self-hosted Dropbox replacement (peer-to-peer sync mesh).
#
# Runs as a native NixOS service (no container). Syncthing's declarative
# config via services.syncthing.settings covers folders and devices, but
# device IDs are exchanged on first pairing via the web UI or config file.
#
# Workflow for adding a new device:
#   1. Find the device ID: Settings -> General -> Device ID (in the Syncthing UI)
#      Or on CLI: syncthing --device-id
#   2. Add the device ID to settings.devices below and rebuild.
#   3. On the remote device, add aida's ID and accept the share on both sides.
#
# The GUI is proxied by Caddy at https://syncthing.yourdomain.com.
# It is bound to 127.0.0.1 only and protected by Caddy's TLS + your VPN.

{ config, ... }:

{
  services.syncthing = {
    enable = true;
    # Run as root to allow reading/writing /data; adjust if you prefer a
    # dedicated syncthing user (requires chowning /data/syncthing).
    user = "root";
    group = "root";
    dataDir = "/data/syncthing";
    configDir = "/data/syncthing/.config/syncthing";
    openDefaultPorts = true;

    settings = {
      gui = {
        # Caddy proxies this; never expose directly
        address = "127.0.0.1:8384";
        # Required when behind a reverse proxy (Host header check relaxed)
        insecureSkipHostcheck = true;
      };

      options = {
        # Disable global discovery and relay — all sync happens on LAN or VPN
        globalAnnounceEnabled = false;
        relaysEnabled = false;
        urAccepted = -1; # disable usage reporting
      };

      # Define remote devices here after exchanging device IDs.
      # Example structure (fill in real IDs):
      #
      # devices = {
      #   "laptop" = { id = "XXXXXX-XXXXXX-..."; };
      #   "phone"  = { id = "YYYYYY-YYYYYY-..."; };
      # };

      # Define shared folders. Paths under /data/syncthing/
      # are created automatically by services.syncthing.
      #
      # Example:
      # folders = {
      #   "documents" = {
      #     path = "/data/syncthing/documents";
      #     devices = [ "laptop" "phone" ];
      #   };
      # };
    };
  };
}
