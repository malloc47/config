#!/usr/bin/env bash
nix run github:nix-community/nixos-anywhere/1.11.0 -- --disko-mode mount --flake .#salome --target-host root@$1
