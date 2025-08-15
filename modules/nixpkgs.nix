{
  config,
  pkgs,
  lib,
  options,
  inputs,
  ...
}:
{
  config = {
    nixpkgs.overlays = [ (import ../pkgs/default.nix) ];

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

    nix.nixPath = [
      "nixpkgs=${inputs.nixpkgs}"
      #"nixos-config=/etc/nixos/flake.nix"
      "nixpkgs-overlays=/etc/nixos/overlays-compat/"
    ];

    nix.registry.nixpkgs.flake = inputs.nixpkgs;
    nix.registry.self.flake = inputs.self;

    nix.registry.sys = {
      from = {
        type = "indirect";
        id = "sys";
      };
      flake = inputs.nixpkgs;
    };
  };
}
