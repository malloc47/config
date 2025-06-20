{ config, pkgs, lib, options, ... }:
{
  config = {
    nixpkgs.overlays = [(import ../pkgs/default.nix)];

    environment.etc."overlays-compat" = {
      text = ''
        final: prev:
        with prev.lib;
        let
          # Load the system config and get the `nixpkgs.overlays` option
          overlays = (import <nixpkgs/nixos> { }).config.nixpkgs.overlays;
        in
          # Apply all overlays to the input of the current "main" overlay
          foldl' (flip extends) (_: prev) overlays final
      '';
      target = "nixos/overlays-compat/overlays.nix";
    };

    nix.channel.enable = false;
    

    # nix.nixPath =
    #   options.nix.nixPath.default ++
    #   [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];
  };
}
