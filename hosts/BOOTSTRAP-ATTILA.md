# Bootstrapping attila

Manual steps required to bring up the attila host (Dell XPS 13 9315) as a headless NixOS server.

## Prerequisites

- Dell XPS 13 9315 with power adapter
- USB-C flash drive with NixOS installer, or another machine for nixos-anywhere
- WiFi network credentials
- A personal SSH ed25519 keypair

## 1. BIOS configuration

Power on and press F2 to enter BIOS:

1. **Power Management > Lid Switch**: set to disabled (prevent sleep on lid close)
2. **Power Management > AC Recovery**: set to "Power On" (auto-start after power loss)
3. **Power Management > Battery Charge Configuration**: set to "Primarily AC Use" (caps charge at ~80%)
4. **Security > Secure Boot**: disable (required for NixOS)
5. **Boot Sequence**: ensure PXE/network boot is enabled (for netboot install), or USB boot (for USB install)

Save and exit.

## 2. Install NixOS

### Option A: Netboot from aida + nixos-anywhere (recommended, no USB needed)

Enable pixiecore on aida temporarily:

```bash
# In hosts/aida.nix, uncomment the ../modules/netboot.nix import, then deploy:
nixos-rebuild switch --flake .#aida
```

Boot the XPS:
1. Power on, press **F12** for one-time boot menu
2. Select the **network/PXE boot** option
3. The XPS will get an IP from OpenWRT and boot options from pixiecore on aida
4. A minimal NixOS installer boots in RAM with your SSH key pre-authorized

Connect the installer to WiFi (needed for nixos-anywhere to download packages):

```bash
# Find the XPS IP from OpenWRT DHCP leases or AdGuard Home, then SSH in:
ssh nixos@<xps-ip>

# Connect to WiFi from the installer (nmtui is easier than iwctl)
nmtui
```

Then from your Mac, run nixos-anywhere:

```bash
# Stage new files first
git add hardware/dell-xps-9315.nix disk/dell-xps-9315.nix hosts/attila.nix

nix run github:nix-community/nixos-anywhere -- \
  --flake .#attila \
  root@<xps-ip>
```

After installation completes, disable pixiecore on aida:

```bash
# In hosts/aida.nix, comment out the ../modules/netboot.nix import, then deploy:
nixos-rebuild switch --flake .#aida
```

### Option B: USB boot + nixos-anywhere

First, boot a live Linux USB (Ubuntu or NixOS installer) and connect to WiFi:

```bash
# On the XPS, from live environment (nmtui is easier than iwctl):
nmtui
ip addr  # note the IP address

# Enable root SSH (default user on NixOS installer is nixos)
sudo passwd root
```

Then from your Mac:

```bash
git add hardware/dell-xps-9315.nix disk/dell-xps-9315.nix hosts/attila.nix

nix run github:nix-community/nixos-anywhere -- \
  --flake .#attila \
  root@<xps-ip>
```

### Option C: Manual install from NixOS USB

1. Boot NixOS installer USB (default user is `nixos`)
2. Connect to WiFi: `nmtui`
3. Partition with disko: `nix run github:nix-community/disko -- --mode disko /path/to/disk/dell-xps-9315.nix`
4. Install: `nixos-install --flake .#attila`

## 3. Connect to WiFi (post-install)

After first boot, connect to WiFi using `iwd`:

```bash
iwctl
[iwd]# station wlan0 scan
[iwd]# station wlan0 get-networks
[iwd]# station wlan0 connect <SSID>
```

iwd stores network credentials in `/var/lib/iwd/` and reconnects automatically on subsequent boots.

## 4. Verify SSH access

From your Mac:

```bash
# Find attila's IP (check router DHCP leases or use mDNS)
ssh malloc47@<attila-ip>
```

Consider adding a static DHCP lease on your OpenWRT router for attila's WiFi MAC address.

## 5. Grab the host key for agenix

```bash
cat /etc/ssh/ssh_host_ed25519_key.pub
```

Add the key to `secrets/secrets.nix`:

```nix
attila = "ssh-ed25519 AAAA... root@attila";
```

Add `attila` to the `publicKeys` list for any secrets this host needs, then re-encrypt:

```bash
cd secrets
agenix -r -i /path/to/your/personal/id_ed25519
```

## 6. Physical setup

1. Plug in the 45W USB-C power adapter
2. Close the lid (logind is configured to ignore lid events)
3. Place in a ventilated area — avoid enclosed spaces due to thermal constraints

If using Ethernet: connect a USB-C to Ethernet adapter to the remaining Thunderbolt port.

## 7. Optional: DHCP reservation

Add a static lease on OpenWRT for attila's WiFi MAC address to ensure a stable IP:

```
# In hosts/openwrt/dhcp, add:
config host
	option name 'attila'
	option mac '<wifi-mac-address>'
	option ip '192.168.1.XX'
```

## Verification checklist

```bash
# SSH access
ssh malloc47@<attila-ip>

# Confirm headless (lid should be closed, no display activity)
cat /sys/class/drm/card*/status

# Battery charge threshold
cat /sys/class/power_supply/BAT0/charge_control_end_threshold
# Should show 80

# WiFi connectivity
ip addr show wlan0
ping -c 3 192.168.1.1
```
