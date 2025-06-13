#!/usr/bin/env bash
nix run github:nix-community/nixos-anywhere -- --flake .#salome --target-host nixos@$1
