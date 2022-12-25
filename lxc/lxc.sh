#!/usr/bin/env bash
set -e
lxc stop nixos || true
lxc delete nixos || true
lxc image delete nixos-base || true
lxc profile delete x11 || true
LXC_ARTIFACT=`readlink -f lxc`
LXC_METADATA_ARTIFACT=`readlink -f lxc-metadata`
rm -f lxc lxc-metadata
echo "Deleting $LXC_ARTIFACT"
nix store delete $LXC_ARTIFACT || true
echo "Deleting $LXC_METADATA_ARTIFACT"
nix store delete $LXC_METADATA_ARTIFACT || true
rm -rf ~/lxc-share
nixos-generate -f lxc -c lxc-bootstrap.nix -o lxc
nixos-generate -f lxc-metadata -c lxc-bootstrap.nix -o lxc-metadata
lxc image import --alias nixos-base lxc-metadata/tarball/nixos-system-x86_64-linux.tar.xz lxc/tarball/nixos-system-x86_64-linux.tar.xz
lxc profile create x11
cat x11.yaml | lxc profile edit x11
lxc init nixos-base nixos -c security.nesting=true -c security.privileged=true --profile default --profile x11
mkdir -p $HOME/lxc-share
lxc config device add nixos homedir disk source=/home/$USER/lxc-share path=/home/$USER
git clone --recurse-submodules git@github.com:malloc47/config.git ~/lxc-share/src/config
lxc start nixos
lxc exec nixos -- ln -f -s /home/malloc47/src/config/hosts/harpocrates.nix /etc/nixos/configuration.nix
# Because the configuration is not active yet, we have to manually set
# the overlays folder this first time; relies on the base image having
# defined the compatibility overlay in this directory.
lxc exec nixos -- su - -c 'NIX_PATH="$NIX_PATH:nixpkgs-overlays=/etc/nixos/overlays-compat" nixos-rebuild switch'
COOKIE=$(xauth list | awk '{print $3}')
lxc exec nixos -- su - malloc47 -c "xauth add \$HOST/unix:0 MIT-MAGIC-COOKIE-1 $COOKIE"
# Hacky way to copy the user password hash into the container
lxc exec nixos -- bash -c "echo \"malloc47:$(printf \"%q\" $(sudo cat /etc/shadow | grep malloc47 | awk -F: '{print $2}'))\" | chpasswd -e"
echo "lxc exec nixos -- /run/current-system/sw/bin/bash"
echo "lxc exec nixos -- machinectl shell --uid=malloc47"
