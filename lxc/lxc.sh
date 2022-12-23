#!/usr/bin/env bash
lxc stop nixos
lxc delete nixos
lxc image delete nixos-base
LXC_ARTIFACT=`readlink -f lxc`
LXC_METADATA_ARTIFACT=`readlink -f lxc-metadata`
rm lxc lxc-metadata
echo "Deleting $LXC_ARTIFACT"
nix store delete $LXC_ARTIFACT
echo "Deleting $LXC_METADATA_ARTIFACT"
nix store delete $LXC_METADATA_ARTIFACT
nixos-generate -f lxc -c lxc-bootstrap.nix -o lxc
nixos-generate -f lxc-metadata -c lxc-bootstrap.nix -o lxc-metadata
lxc image import --alias nixos-base lxc-metadata/tarball/nixos-system-x86_64-linux.tar.xz lxc/tarball/nixos-system-x86_64-linux.tar.xz
lxc launch nixos-base nixos -c security.nesting=true -c security.privileged=true
echo "lxc exec nixos -- /run/current-system/sw/bin/bash"
echo "lxc exec nixos -- su - malloc47"
