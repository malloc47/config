{ config, pkgs, ... }:
{
  home = {
    stateVersion = "25.05";
    packages = with pkgs; [
      clojure
      nixos-anywhere
      inconsolata-unstable #proves that overlays work
    ];
  };

  programs.home-manager.enable = true;
}

