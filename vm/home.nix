{ config, pkgs, ... }:
{
  home = {
    stateVersion = "25.05";
    packages = with pkgs; [
      clojure
      nixos-anywhere
    ];
  };

  programs.home-manager.enable = true;
}

