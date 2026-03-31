# Bootstrapping aroldo

Manual steps required to bring up the aroldo host (RackNerd VPS) running Headscale + DERP relay.

## Prerequisites

- RackNerd VPS with root SSH access (Ubuntu or any Linux)
- A personal SSH ed25519 keypair for encrypting/decrypting agenix secrets
- Cloudflare account with `malloc47.com` zone

## VPS Details (current instance)

These values are specific to the current VPS instance and will change if the VPS is rebuilt or migrated:

| Parameter | Value |
|-----------|-------|
| Public IP | 192.3.76.171 |
| Netmask | /24 (255.255.255.0) |
| Gateway | 192.3.76.1 |
| Interface | eth0 |
| Disk | /dev/vda (45GB) |
| DNS | 8.8.8.8, 8.8.4.4 |

If any of these change on rebuild, update `hosts/aroldo.nix` accordingly (static IP, gateway).

## 1. Install NixOS via nixos-anywhere

From an x86_64-linux machine (e.g. aida — the VPS only has 2.5GB RAM which is insufficient for building the closure remotely):

```bash
# Stage new files first
git add hosts/aroldo.nix disk/racknerd-vps.nix hardware/racknerd-vps.nix

nix run github:nix-community/nixos-anywhere -- \
  --flake .#aroldo \
  --generate-hardware-config nixos-generate-config hardware/racknerd-vps.nix \
  --build-on local
  root@192.3.76.171
```

The `--generate-hardware-config` flag runs `nixos-generate-config` on the VPS and writes the result to `hardware/racknerd-vps.nix`. This file provides the virtio kernel modules (`virtio_pci`, `virtio_blk`) needed to boot on KVM — without it, the initrd cannot see `/dev/vda` and boot will hang at `waiting for device`.

The generated hardware file must be committed and included in the aroldo flake entry (`hardware/racknerd-vps.nix` in the modules list). If rebuilding a new VPS, re-run with `--generate-hardware-config` to regenerate it.

nixos-anywhere will:
1. SSH into the Ubuntu VPS
2. kexec into a NixOS installer in RAM
3. Run `nixos-generate-config` and save hardware config locally
4. Partition the disk via disko
5. Install the NixOS configuration
6. Reboot into NixOS

After reboot, SSH access uses your ed25519 key (configured via `modules/user.nix`):

```bash
ssh aroldo
```

## 2. Register hostname on OpenWRT

Add a DNS host entry so `ssh aroldo` resolves without using the raw IP.

In the OpenWRT LuCI web UI:
1. Go to **Network → DNS**
2. Find the **Host entries** section
3. Add an entry: hostname `aroldo`, IP `192.3.76.171`
4. **Save & Apply**

If the VPS IP changes on rebuild, update this entry.

## 3. Enroll host key in agenix

The NixOS install generates a new SSH host key. Grab it:

```bash
ssh aroldo 'cat /etc/ssh/ssh_host_ed25519_key.pub'
```

Update `secrets/secrets.nix`:

```nix
aroldo = "ssh-ed25519 AAAA... root@aroldo";
```

Add `aroldo` to the `publicKeys` list for secrets this host needs (at minimum `cloudflare-acme.age`), then re-encrypt:

```bash
cd secrets
agenix -r -i /path/to/your/personal/id_ed25519
```

Deploy again so aroldo can decrypt its secrets:

```bash
nixos-rebuild switch --flake .#aroldo --target-host malloc47@aroldo
```

## 4. Cloudflare DNS

Add an A record for the Headscale control plane:

| Type | Name | Content | Proxy |
|------|------|---------|-------|
| A | hs.malloc47.com | 192.3.76.171 | **DNS only** (gray cloud) |

**The proxy MUST be disabled** (DNS-only / gray cloud). Cloudflare's HTTP proxy strips WebSocket Upgrade headers, which breaks the Tailscale ts2021 noise protocol. See https://headscale.net/0.23.0/ref/integration/reverse-proxy/#cloudflare

## 5. Trigger ACME certificate issuance

The ACME timer may not fire immediately. Start it manually:

```bash
ssh aroldo 'sudo systemctl start acme-hs.malloc47.com.service'
ssh aroldo 'journalctl -fu acme-hs.malloc47.com'
```

Wait for it to complete — Caddy needs the cert before it can serve HTTPS.

## 6. Register aroldo as a Tailscale exit node

aroldo runs a Tailscale client that advertises itself as an exit node, allowing any device on the tailnet to route all internet traffic through the VPS.

```bash
ssh aroldo 'sudo tailscale up \
  --login-server https://hs.malloc47.com \
  --advertise-exit-node \
  --reset \
  --authkey $(sudo cat /run/headscale/authkey)'
```

Verify:

```bash
ssh aroldo 'tailscale status'
```

To use from a client:
- **CLI**: `tailscale set --exit-node=aroldo`
- **Android**: Tailscale app > three-dot menu > Use exit node > aroldo
- **macOS**: Menu bar > Exit node > aroldo

## 7. Register aida as a Tailscale subnet router

First deploy aida (if not already) so that `tailscaled` is running:

```bash
nixos-rebuild switch --flake .#aida
```

Get a pre-auth key from aroldo (the `headscale-bootstrap` oneshot writes one to `/run/headscale/authkey`):

```bash
ssh aroldo 'sudo cat /run/headscale/authkey'
```

On aida, manually register with the pre-auth key:

```bash
sudo tailscale up \
  --login-server https://hs.malloc47.com \
  --advertise-routes=192.168.1.0/24 \
  --advertise-exit-node \
  --reset \
  --authkey <paste key here>
```

The pre-auth key expires after 24h but is only needed for this one-time registration. Once registered, aida reconnects automatically using its persistent node key in `/var/lib/tailscale/`.

## 8. Verify Headplane

Headplane (web UI for Headscale) is available at `https://hs.malloc47.com/admin`. It requires an API key for authentication.

The `headscale-bootstrap` oneshot generates an API key at `/run/headscale/api-key`. On first boot this should happen automatically. If Headplane shows an auth error, verify the key exists:

```bash
ssh aroldo 'sudo cat /run/headscale/api-key'
```

If empty or missing, generate one manually:

```bash
ssh aroldo 'sudo headscale apikeys create --expiration 90d'
```

Copy the key and paste it into the Headplane login page.

Note: The API key expires after 90 days. The `headscale-bootstrap` oneshot regenerates one on each boot, but the key in `/run/headscale/api-key` is ephemeral (lost on reboot). For persistent access, create a long-lived key via the CLI and log in with it.

## 9. Enroll mobile/laptop clients

### Android phone

1. Install Tailscale from Play Store
2. Three-dot menu > Use another server > `https://hs.malloc47.com`
3. Login > Headscale shows a registration URL with a node key
4. On aroldo, register and rename for identification:
   ```bash
   ssh aroldo 'sudo headscale nodes register --user 1 --key nodekey:<key>'
   ssh aroldo 'sudo headscale nodes list'  # note the node ID
   ssh aroldo 'sudo headscale node rename <name> -u <node-id>'
   ```

### macOS laptop

```bash
tailscale up --login-server https://hs.malloc47.com
```

On aroldo, register and rename:
```bash
ssh aroldo 'sudo headscale nodes register --user 1 --key nodekey:<key>'
ssh aroldo 'sudo headscale nodes list'  # note the node ID
ssh aroldo 'sudo headscale node rename <name> -u <node-id>'
```

## Verification checklist

```bash
# Headscale health
curl https://hs.malloc47.com/health

# DERP connectivity (from aida)
tailscale netcheck

# Tailscale enrollment (from aida)
tailscale status

# LAN access (from phone on Tailscale)
ping 192.168.1.10

# fail2ban
ssh aroldo 'sudo fail2ban-client status sshd'
```

## Rebuilding after a RackNerd incident

If the VPS is destroyed and reprovisioned:

1. **Update network details** in `hosts/aroldo.nix` if the IP, gateway, or interface changed
2. **Re-run nixos-anywhere** (step 1 above) against the new VPS
3. **Register hostname** (step 2) — update IP in OpenWRT if changed
4. **Re-enroll the new host key** (step 3) — the old key is gone with the old VPS
5. **Update Cloudflare DNS** if the IP changed
6. **Re-register Tailscale nodes** — existing nodes' keys are still valid in Headscale's database (which is gone), so all clients need to re-register:
   - aida: restart `tailscaled-autoconnect` or `tailscale up --reset`
   - phone/laptop: re-login in Tailscale app
7. The `headscale-bootstrap` oneshot will automatically recreate the user and pre-auth key on first boot
