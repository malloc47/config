#!/usr/bin/env bash
set -e

function cleanup () {
    lxc stop nixos || true
    lxc delete nixos || true
    lxc image delete nixos-base || true
    LXC_ARTIFACT=`readlink -f lxc`
    LXC_METADATA_ARTIFACT=`readlink -f lxc-metadata`
    rm -f lxc lxc-metadata
    echo "Deleting $LXC_ARTIFACT"
    nix store delete $LXC_ARTIFACT || true
    echo "Deleting $LXC_METADATA_ARTIFACT"
    nix store delete $LXC_METADATA_ARTIFACT || true
    rm -rf ~/lxc-share
}

function image() {
    nixos-generate -f lxc -c lxc-bootstrap.nix -o lxc
    nixos-generate -f lxc-metadata -c lxc-bootstrap.nix -o lxc-metadata
    lxc image import --alias nixos-base lxc-metadata/tarball/nixos-system-x86_64-linux.tar.xz lxc/tarball/nixos-system-x86_64-linux.tar.xz
}

function x11_profile() {
    # Adapted from
    # https://github.com/ustuehler/lxc-desktop/blob/master/usr/share/lxc/hooks/desktop-autodev
    lxc profile delete x11 || true
    cp x11.template x11.yaml
    for f in /dev/vga_arbiter \
	     /dev/fb0 \
	     /dev/dri \
	     /dev/dri/card* \
	     /dev/input \
	     /dev/input/* \
	     /dev/input/*/* \
	     /dev/psaux \
	     /dev/tty[0-9] \
	     /dev/tty[0-9][0-9] \
	     /dev/console \
	     /dev/net \
	     /dev/net/tun \
	     /dev/nvidia0 \
	     /dev/nvidiactl \
	     /dev/video0 \
	     /dev/loop-control \
	     /dev/loop[0-9]* ; do
	ALIAS=`basename $f`
	case `stat -L -c '%F' $f` in
	    "block special file")
		TYPE="unix-block"
                ;;
	    "character special file")
		TYPE="unix-char"
                ;;
	    "directory")
		TYPE="disk"
		continue
                ;;
	    *)
                continue
                ;;
	esac
	FILE_UID=$(stat -c "%u" $f)
	FILE_GID=$(stat -c "%g" $f)
	FILE_MODE=$(stat -c "%a" $f)
	cat <<EOT >> x11.yaml
  ${ALIAS}:
    path: $f
    source: $f
    type: $TYPE
    uid: "${FILE_UID}"
    gid: "${FILE_GID}"
    mode: 777
EOT
    done
    lxc profile create x11 || true
    cat x11.yaml | lxc profile edit x11
}

function container () {
    lxc init nixos-base nixos -c security.nesting=true -c security.privileged=true --profile default --profile x11
    mkdir -p $HOME/lxc-share
    lxc config device add nixos homedir disk source=/home/$USER/lxc-share path=/home/$USER
    # Needed for xorg to launch cleanly
    # lxc config device add mycontainer tty0 unix-char source=/dev/tty0 path=/dev/tty0
    # lxc config device add nixos nixstore disk source=/nix/store path=/nix/store
    git clone --recurse-submodules git@github.com:malloc47/config.git ~/lxc-share/src/config
    lxc start nixos
    lxc exec nixos -- ln -f -s /home/malloc47/src/config/hosts/harpocrates.nix /etc/nixos/configuration.nix
    # Because the configuration is not active yet, we have to manually set
    # the overlays folder this first time; relies on the base image having
    # defined the compatibility overlay in this directory.
    #
    # Interestingly this fairly consistently repros
    # https://github.com/NixOS/nixpkgs/issues/160289
    lxc exec nixos -- su - -c 'NIX_PATH="$NIX_PATH:nixpkgs-overlays=/etc/nixos/overlays-compat" nixos-rebuild switch' || true
    # Do it again because of the above issue
    lxc exec nixos -- su - -c 'NIX_PATH="$NIX_PATH:nixpkgs-overlays=/etc/nixos/overlays-compat" nixos-rebuild switch'
    COOKIE=$(xauth list | awk '{print $3}')
    lxc exec nixos -- su - malloc47 -c "xauth add \$HOST/unix:0 MIT-MAGIC-COOKIE-1 $COOKIE"
    # Hacky way to copy the user password hash into the container
    lxc exec nixos -- bash -c "echo \"malloc47:$(printf \"%q\" $(sudo cat /etc/shadow | grep malloc47 | awk -F: '{print $2}'))\" | chpasswd -e"
}

function background_image () {
    cp ~/.background-image $HOME/lxc-share/
}

cleanup
image
x11_profile
container
background_image

echo "Various ways to get into the container:"
echo "    lxc exec nixos -- /run/current-system/sw/bin/bash"
echo "    lxc exec nixos -- su - malloc47"
echo "    lxc exec nixos -- machinectl shell --uid=malloc47"
echo "    lxc exec nixos -- machinectl shell --uid=malloc47 .host /run/current-system/sw/bin/startx"
