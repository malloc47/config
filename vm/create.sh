#!/usr/bin/env bash

ISO=~/Downloads/nixos-minimal-25.05.803882.fd4871834379-aarch64-linux.iso 
NAME=NixOS2
OUTPUT=~/vm/$NAME.vmx

vmcli VM Create -d $(dirname $OUTPUT) -g arm-other6xlinux-64 -n $NAME 
vmcli ConfigParams SetEntry displayName "$NAME" $OUTPUT
vmcli Chipset SetMemSize 8192 $OUTPUT
vmcli Chipset SetVCpuCount 4 $OUTPUT
vmcli Chipset SetCoresPerSocket 1 $OUTPUT

vmcli ConfigParams SetEntry firmware "efi" $OUTPUT
vmcli ConfigParams SetEntry tools.syncTime "TRUE" $OUTPUT
vmcli ConfigParams SetEntry ehci.present "TRUE" $OUTPUT
vmcli ConfigParams SetEntry usb.present "TRUE" $OUTPUT
vmcli ConfigParams SetEntry usb_xhci.present "TRUE" $OUTPUT
vmcli ConfigParams SetEntry keyboardAndMouseProfile "5262affe-9b57-1399-580b-68ddea78bfb9" $OUTPUT

rm ~/vm/$NAME.vmdk # This is autocreated for some reason
vmcli Disk Create -f ~/vm/NixOS2.vmdk -a ide -s 100GB -t 1 $OUTPUT
vmcli Nvme SetPresent nvme0 1 $OUTPUT
vmcli Disk SetBackingInfo nvme0:0 disk NixOS2.vmdk 1 $OUTPUT
vmcli Disk SetPresent nvme0:0 1 $OUTPUT

vmcli Sata SetPresent sata0 1 $OUTPUT
vmcli Disk SetBackingInfo sata0:1 cdrom_image $ISO 1 $OUTPUT
vmcli Disk SetPresent sata0:1 1 $OUTPUT

vmcli ConfigParams SetEntry bios.bootOrder "HDD" $OUTPUT
vmcli ConfigParams SetEntry bios.hddOrder "nvme0:0" $OUTPUT

#vmcli Ethernet query $OUTPUT
vmcli Ethernet SetVirtualDevice ethernet0 e1000e $OUTPUT
vmcli Ethernet SetConnectionType ethernet0 nat $OUTPUT
vmcli Ethernet SetAddressType ethernet0 generated "" $OUTPUT
vmcli Ethernet SetLinkStatePropagation ethernet0 true $OUTPUT
vmcli Ethernet SetPresent ethernet0 1 $OUTPUT

vmcli ConfigParams SetEntry gui.fitGuestUsingNativeDisplayResolution "TRUE" $OUTPUT
