#!/usr/bin/env bash
set -e

# Only use this to completely delete/reinstall LXD
# See https://discuss.linuxcontainers.org/t/how-to-remove-lxd-from-my-system/2336/16
function cleanup_lxd () {
    for container in $(lxc list -f json | jq -r .[].name); do
	echo "Deleting container $container"
        lxc delete $container
    done
    for image in $(lxc image list -f json | jq -r .[].fingerprint); do
	echo "Deleting image $image"
        lxc image delete $image
    done
    # Can do this better but am too lazy
    echo "Deleting network lxbr0"
    lxc network detach-profile lxdbr0 default || true
    lxc network delete lxdbr0 || true
    echo "Deleting profile default"
    echo '{"config": {}}' | lxc profile edit default
    for volume in $(lxc storage volume list default -f json | jq .[].name); do
        echo "Deleting volume $volume"
        lxc storage volume delete default $volume
    done
    lxc storage delete default
}

function cleanup_image_build () {
    LXC_ARTIFACT=`readlink -f lxc`
    LXC_METADATA_ARTIFACT=`readlink -f lxc-metadata`
    rm -f lxc lxc-metadata
    echo "Deleting $LXC_ARTIFACT"
    nix store delete $LXC_ARTIFACT || true
    echo "Deleting $LXC_METADATA_ARTIFACT"
    nix store delete $LXC_METADATA_ARTIFACT || true
}

function cleanup_container () {
    lxc stop nixos || true
    lxc delete nixos || true
    rm -rf ~/lxc-share
}

function cleanup () {
    cleanup_container
    cleanup_image_build
    lxc image delete nixos-base || true
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
    CURRENT_UID=$(id -u $USER)
    cat <<EOT > x11.yaml
config:
  environment.DISPLAY: :0
  environment.PULSE_SERVER: unix:/home/${USER}/.pulse-native
description: X11 LXD profile
name: x11
used_by: []
devices:
  PASocket1:
    bind: container
    connect: unix:/run/user/${CURRENT_UID}/pulse/native
    listen: unix:/home/${USER}/.pulse-native
    security.gid: "${CURRENT_UID}"
    security.uid: "${CURRENT_UID}"
    uid: "${CURRENT_UID}"
    gid: "${CURRENT_UID}"
    mode: "0777"
    type: proxy
  X0:
    bind: container
    connect: unix:@/tmp/.X11-unix/X0
    listen: unix:@/tmp/.X11-unix/X0
    security.gid: "${CURRENT_UID}"
    security.uid: "${CURRENT_UID}"
    type: proxy
EOT

    if [ -f /tmp/.X0-lock ] ; then
    cat <<EOT >> x11.yaml
  X0-lock:
    path: /tmp/.X0-lock
    source: /tmp/.X0-lock
    type: disk
EOT
    fi

    cat <<EOT >> x11.yaml
  mygpu:
    type: gpu
    gid: "${CURRENT_UID}"
EOT
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
    lxc init nixos-base nixos -c security.nesting=true -c security.privileged=true --profile default # --profile x11hh
    mkdir -p $HOME/lxc-share
    lxc config device add nixos homedir disk source=/home/$USER/lxc-share path=/home/$USER
    # before doing this, the /nix/store must be populated with the contents of the image or it will fail to boot
    if [ -d "/nix/store" ] ; then
        lxc config device add nixos nixstore disk source=/nix/store path=/nix/store
    fi
    # lxc config device add nixos resolv disk source=/etc/resolv.conf path=/etc/resolv.conf
    lxc config device add nixos certs disk source=/etc/drw-security-certs/ path=/etc/drw-security-certs/ readonly=true
    lxc config device add nixos certs-pem disk source=/etc/drw-certs/ path=/etc/drw-certs/ readonly=true
    # Needed for xorg to launch cleanly
    # lxc config device add mycontainer tty0 unix-char source=/dev/tty0 path=/dev/tty0
    git clone --recurse-submodules git@github.com:malloc47/config.git ~/lxc-share/src/config
    cp ~/src/config/hosts/drw.nix ~/lxc-share/src/config/hosts/
    echo "Starting container"
    lxc start nixos
    echo "Waiting for container"
    sleep 5
    lxc exec nixos -- ln -f -s /home/$USER/src/config/flake.nix /etc/nixos/flake.nix
    # https://superuser.com/a/1598351
    lxc exec nixos -- loginctl enable-linger $USER
    # Because the configuration is not active yet, we have to manually set
    # the overlays folder this first time; relies on the base image having
    # defined the compatibility overlay in this directory.
    #
    # Interestingly this fairly consistently repros
    # https://github.com/NixOS/nixpkgs/issues/160289
    lxc exec nixos -- su - -c 'nixos-rebuild switch' || true
    # Do it again because of the above issue
    lxc exec nixos -- su - -c 'nixos-rebuild switch'
    COOKIE=$(xauth list | awk '{print $3}' | head -1)
    lxc exec nixos -- su - $USER -c "xauth add \$HOST/unix:0 MIT-MAGIC-COOKIE-1 $COOKIE"
    # Hacky way to copy the user password hash into the container
    lxc exec nixos -- bash -c "echo \"$USER:$(printf \"%q\" $(sudo cat /etc/shadow | grep $USER | awk -F: '{print $2}'))\" | chpasswd -e"
}

function background_image () {
    cp ~/.background-image $HOME/lxc-share/
}

function build () {
    cleanup
    image
    x11_profile
    container
    background_image
    cleanup_image_build
    help
}

function build_from_container () {
    cleanup_container
    # x11_profile
    container
    background_image
    help
}

function help () {
    echo "Various ways to get into the container:"
    echo "    lxc exec nixos -- /run/current-system/sw/bin/bash"
    echo "    lxc exec nixos -- su - $USER"
    echo "    lxc exec nixos -- machinectl shell --uid=$USER"
    echo "    lxc exec nixos -- machinectl shell --uid=$USER .host /run/current-system/sw/bin/startx"
}

function parse_opts () {

    if [ $# -eq 0 ]; then
	help
	return
    fi

    for arg in "$@"; do
	case "$arg" in
	    all)
		build
		;;
	    *)
		if [[ $(type -t $1) == function ]] ; then
	            $1
		else
		    echo "Error: $1 is not a recognized command" 1>&2
		    exit 1
	        fi
		;;
	esac
	shift
    done
}

parse_opts "$@"
