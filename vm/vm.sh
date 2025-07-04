#!/usr/bin/env bash
set -e

ROOT=$(git rev-parse --show-toplevel)

ISO_URL="https://releases.nixos.org/nixos/25.05/nixos-25.05.804936.a676066377a2/nixos-minimal-25.05.804936.a676066377a2-aarch64-linux.iso"
ISO_NAME=nixos-minimal-aarch64-linux.iso

NAME=NixOS
VM_ROOT=~/vm

DISK_SIZE="100GB"

function download_iso() {
    if [ ! -f $VM_ROOT/$ISO_NAME ]; then
	echo wget $ISO_URL -O $VM_ROOT/$ISO_NAME
    fi
    ISO=$VM_ROOT/$ISO_NAME
}

function _vmx_file() {
    if [ -z "${VMX_FILE}" ]; then VMX_FILE=$VM_ROOT/$NAME.vmx ; fi
    return $VMX_FILE
}

function create_vmx_file() {
    OUT=_vmx_file()

    if [ -f $OUT ]; then
	echo "VMX file $OUT already exists!"
	return
    fi
    echo "Creating $OUT"


    vmcli VM Create -d $(dirname $OUT) -g arm-other6xlinux-64 -n $NAME
    vmcli ConfigParams SetEntry displayName "$NAME" $OUT
    vmcli Chipset SetMemSize 8192 $OUT
    vmcli Chipset SetVCpuCount 4 $OUT
    vmcli Chipset SetCoresPerSocket 1 $OUT

    vmcli ConfigParams SetEntry firmware "efi" $OUT
    vmcli ConfigParams SetEntry tools.syncTime "TRUE" $OUT
    vmcli ConfigParams SetEntry ehci.present "TRUE" $OUT
    vmcli ConfigParams SetEntry usb.present "TRUE" $OUT
    vmcli ConfigParams SetEntry usb_xhci.present "TRUE" $OUT
    # Hardcoded to match predefined profile
    vmcli ConfigParams SetEntry keyboardAndMouseProfile "5262affe-9b57-1399-580b-68ddea78bfb9" $OUT

    rm $VM_ROOT/$NAME.vmdk # This is autocreated for some reason
    vmcli Disk Create -f $VM_ROOT/$NAME.vmdk -a ide -s $DISK_SIZE -t 1 $OUT
    vmcli Nvme SetPresent nvme0 1 $OUT
    vmcli Disk SetBackingInfo nvme0:0 disk $NAME.vmdk 1 $OUT
    vmcli Disk SetPresent nvme0:0 1 $OUT

    vmcli Sata SetPresent sata0 1 $OUT
    vmcli Disk SetBackingInfo sata0:1 cdrom_image $ISO 1 $OUT
    vmcli Disk SetPresent sata0:1 1 $OUT

    vmcli ConfigParams SetEntry bios.bootOrder "HDD" $OUT
    vmcli ConfigParams SetEntry bios.hddOrder "nvme0:0" $OUT

    vmcli Ethernet SetVirtualDevice ethernet0 e1000e $OUT
    vmcli Ethernet SetConnectionType ethernet0 nat $OUT
    vmcli Ethernet SetAddressType ethernet0 generated "" $OUT
    vmcli Ethernet SetLinkStatePropagation ethernet0 true $OUT
    vmcli Ethernet SetPresent ethernet0 1 $OUT

    vmcli ConfigParams SetEntry gui.fitGuestUsingNativeDisplayResolution "TRUE" $OUT

    vmcli ConfigParams SetEntry sound.autoDetect "TRUE" $OUT
    vmcli ConfigParams SetEntry sound.present "TRUE" $OUT
    vmcli ConfigParams SetEntry sound.virtualDev "hdaudio" $OUT

    VMX_FILE=$OUT
}

function start_vm() {
    if [ -z "${VMX_FILE}" ]; then VMX_FILE=$VM_ROOT/$NAME.vmx ; fi

    vmrun start $VMX_FILE
}


function help () {
    echo "HELP: VM commands"
}

function parse_opts () {

    if [ $# -eq 0 ]; then
	help
	return
    fi

    COMMANDS=()

    while [[ $# -gt 0 ]]; do
	case $1 in
	    -d|--disk-size)
		DISK_SIZE="$2"
		shift; shift
		;;
	    -i|--ip)
		IP="$2"
		shift; shift
		;;
	    -p|--path)
		VM_ROOT="$2"
		ISO=$VM_ROOT/$ISO_NAME
		shift; shift
		;;
	    -n|--name)
		NAME="$2"
		shift; shift
		;;
	    -v|--vmx)
		VMX_FILE="$2"
		shift; shift
		;;
	    -*|--*)
		echo "Unknown option $1"
		exit 1
		;;
	    *)
		if [[ $(type -t $1) == function ]] ; then
		    COMMANDS+=("$1")
		else
		    echo "Error: $1 is not a recognized command" 1>&2
		    exit 1
	        fi
		shift
		;;
	esac
    done

    echo "Running commands: ${COMMANDS[@]}"

    for command in "${COMMANDS[@]}"; do
	$command
    done
}

parse_opts "$@"
