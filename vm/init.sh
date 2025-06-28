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
VMX_FILE=$(vmrun list | tail -n +2 | head -1)
vmrun stop $VMX_FILE
# Make VM full screen
vmcli ConfigParams SetEntry gui.lastPoweredViewMode "fullscreen" $VMX_FILE
vmcli ConfigParams SetEntry gui.viewModeAtPowerOn  "fullscreen" $VMX_FILE
# Remove iso
vmcli Disk SetPresent sata0:1 0 $VMX_FILE
vmcli Sata SetPresent sata0 0 $VMX_FILE
vmrun start $VMX_FILE
echo "★★★★ VM build complete ★★★★★★★"
