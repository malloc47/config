#!/usr/bin/env bash
nix run github:nix-community/nixos-anywhere -- --flake .#salome --build-on remote --target-host nixos@$1
