#!/usr/bin/env bash
IP=$1
nix run github:nix-community/nixos-anywhere -- --flake .#salome --build-on remote --target-host nixos@$IP
# Easiest way I know to extract the default user out of the built system
USER=$(ssh root@$IP id -nu 1000)
rsync -av --mkpath --stats $(git rev-parse --show-toplevel)/ ${USER}@${IP}:src/config/
ssh root@$1 "ln -s /home/$USER/src/config/vm/flake.nix /etc/nixos/flake.nix" 
