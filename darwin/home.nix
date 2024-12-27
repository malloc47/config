# home.nix

{ config, pkgs, ... }:

{

  home = {
    stateVersion = "23.05";
    packages = with pkgs; [
      clojure
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}

