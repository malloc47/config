#!/usr/bin/env bash
# Choose "no" to install vanilla nix
curl -fsSL https://install.determinate.systems/nix | sh -s -- install
sudo nix run nix-darwin/nix-darwin-25.05#darwin-rebuild -- switch --flake .#cesare
sudo mkdir /etc/nix-darwin/
sudo ln -s $(git rev-parse --show-toplevel)/flake.nix /etc/nix-darwin/flake.nix
