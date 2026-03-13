# Disko configuration for the second M.2 NVMe data drive.
#
# This file is NOT yet imported by flake.nix. When the drive arrives:
#   1. Install the drive in the G10's second M.2 slot.
#   2. Identify the device: `lsblk` (will be /dev/nvme1n1 if nvme0n1 is the OS drive)
#   3. Add `disk/data-drive.nix` to the aida modules list in flake.nix.
#   4. Run: sudo nix run github:nix-community/disko -- --mode destroy,format,mount /etc/nixos/...
#      Or just mkfs.ext4 -L data /dev/nvme1n1 and let NixOS mount it via the fstab entry below.
#
# The systemd.tmpfiles.rules in containers.nix create the subdirectories
# automatically, so no manual mkdir needed after the drive is mounted.

{ ... }:

{
  disko.devices.disk.data = {
    device = "/dev/disk/by-id/nvme-REPLACE_WITH_DRIVE_ID";
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        data = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/data";
            mountOptions = [ "defaults" "noatime" ];
            extraArgs = [ "-L" "data" ];
          };
        };
      };
    };
  };
}
