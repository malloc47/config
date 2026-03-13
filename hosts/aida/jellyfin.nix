# Jellyfin — self-hosted media server.
#
# Uses the native NixOS module (not a container) for tighter systemd
# integration. VAAPI hardware transcoding via the Ryzen 5 3500U's Vega 8
# iGPU is configured here; Jellyfin's UI must also be configured to use it:
#
#   Dashboard -> Playback -> Transcoding:
#     Hardware acceleration: VA-API
#     VA-API Device: /dev/dri/renderD128
#
# Media library lives at /data/media/ — add it as a library in the Jellyfin UI.
# Recommended structure:
#   /data/media/movies/
#   /data/media/tv/
#   /data/media/music/
#
# Access: https://jellyfin.yourdomain.com

{ pkgs, ... }:

{
  services.jellyfin = {
    enable = true;
    # Don't open firewall ports directly — Caddy handles external access
    openFirewall = false;
    dataDir = "/data/jellyfin";
    # The media library itself lives outside dataDir to keep config separate
    # from large media files on the data drive
    cacheDir = "/data/jellyfin/cache";
  };

  # VAAPI support for Vega 8 (GCN architecture, amdgpu driver)
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      # VAAPI driver for AMD GCN (covers Vega 8)
      amdvlk
      # libva-utils can be used to verify: vainfo
      libva-utils
    ];
  };

  # jellyfin user needs access to the DRI render node for VAAPI
  users.users.jellyfin.extraGroups = [ "render" "video" ];
}
