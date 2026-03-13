{ config, pkgs, ... }:

{
  imports = [
    # settings.nix is already imported at the flake level; no need to repeat it
    ./aida/networking.nix
    ./aida/secrets.nix
    ./aida/ddns.nix
    ./aida/caddy.nix
    ./aida/containers.nix
    ./aida/immich.nix
    ./aida/syncthing.nix
    ./aida/actual.nix
    ./aida/jellyfin.nix
  ];

  services.openssh.enable = true;
}
