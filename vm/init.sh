#!/usr/bin/env bash
# Assumes that the target VM can login over ssh, following these instructions:
# https://github.com/nix-community/nixos-anywhere/blob/main/docs/howtos/no-os.md
IP=$1
nix run github:nix-community/nixos-anywhere -- --flake .#salome --build-on remote --target-host nixos@$IP
# Remove old keys from known_hosts
ssh-keygen -R $IP
sleep 10
# Easiest way I know to extract the default user out of the built system
USER=$(ssh root@$IP id -nu 1000)
echo "Set password for $USER": 
ssh -t root@$IP "passwd $USER"
rsync -av --mkpath --stats $(git rev-parse --show-toplevel)/ ${USER}@${IP}:src/config/
ssh root@$IP "ln -s /home/$USER/src/config/vm/flake.nix /etc/nixos/flake.nix" 
ssh $USER@$IP "sudo nixos-rebuild switch" 
echo "★★★★ VM build complete ★★★★★★★"
