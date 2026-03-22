# Bootstrapping aida

Manual steps required to bring up the aida host from a fresh NixOS install.
The NixOS configuration in `hosts/aida.nix` is assumed to be correct and committed.

## Prerequisites

- Fresh NixOS install on the gmktec-g10 with SSH access
- A personal SSH ed25519 keypair for encrypting/decrypting secrets
- Cloudflare account with `malloc47.com` zone

## 1. Update secrets/secrets.nix with the new host key

From the new machine:

```bash
cat /etc/ssh/ssh_host_ed25519_key.pub
```

Update `secrets/secrets.nix` — replace the `aida` public key with the new one:

```nix
aida = "ssh-ed25519 AAAA... root@aida";
```

If your personal key also changed, update `malloc47` as well.

## 2. Re-encrypt all secrets for the new keys

From your local machine (where your personal SSH key is available):

```bash
cd secrets

# Re-encrypt every .age file for the updated keys
for f in *.age; do
  agenix -r -i /path/to/your/personal/id_ed25519 "$f"
done
```

This decrypts each secret with your personal key and re-encrypts for both the new host key and your personal key.

## 3. Create/update secrets

Each secret must contain the following. If starting fresh, generate new values.

| Secret file | Contents | How to generate |
|---|---|---|
| `caddy-basicauth.age` | `username $2a$14$bcrypt_hash` (Caddy basicauth format) | `nix run nixpkgs#caddy -- hash-password` |
| `cloudflare-acme.age` | `CLOUDFLARE_DNS_API_TOKEN=<token>` | Cloudflare dashboard → API tokens → Zone:Zone:Read + Zone:DNS:Edit on malloc47.com |
| `authelia-jwt-secret.age` | Random 64+ char string | `openssl rand -hex 64` |
| `authelia-storage-key.age` | Random 64+ char string | `openssl rand -hex 64` |
| `authelia-session-secret.age` | Random 64+ char string | `openssl rand -hex 64` |
| `authelia-users.age` | Authelia users YAML (see below) | Manual |
| `cloudflared-credentials.age` | Tunnel credentials JSON (see step 6) | `cloudflared tunnel create` |

To create or edit a secret:

```bash
agenix -e secrets/<name>.age
```

### Authelia users file format

Generate the password hash first:

```bash
nix run nixpkgs#authelia -- crypto hash generate argon2 --password 'yourpassword'
```

Then the secret file should contain:

```yaml
users:
  malloc47:
    displayname: "Jarrell Waggoner"
    password: "$argon2id$v=19$m=65536,t=3,p=4$..."
    email: "malloc47@gmail.com"
    groups:
      - admins
```

## 4. First deploy

```bash
git add secrets/*.age
nixos-rebuild switch --flake .#aida
```

## 5. Trigger initial ACME certificate issuance

The ACME timer may not fire immediately. Start it manually:

```bash
sudo systemctl start acme-home.malloc47.com.service
journalctl -fu acme-home.malloc47.com
```

Wait for it to complete successfully before proceeding — Caddy needs the cert.

## 6. Create the Cloudflare Tunnel

```bash
nix shell nixpkgs#cloudflared

# Authenticate (opens browser to Cloudflare)
cloudflared tunnel login

# Create the tunnel — note the UUID it prints
cloudflared tunnel create aida-tunnel

# Route DNS
cloudflared tunnel route dns aida-tunnel ntfy-ext.malloc47.com
```

Encrypt the credentials JSON:

```bash
cat ~/.cloudflared/<UUID>.json
agenix -e secrets/cloudflared-credentials.age
# Paste the JSON contents
```

Update `hosts/aida.nix` — replace the tunnel UUID in `services.cloudflared.tunnels`:

```nix
tunnels."<UUID>" = { ... };
```

Deploy again:

```bash
git add secrets/cloudflared-credentials.age
nixos-rebuild switch --flake .#aida
```

Verify:

```bash
cloudflared tunnel info aida-tunnel
```

## 7. Setup ntfy users

ntfy's user database is not declarative — seed it via CLI after deploy:

```bash
# Create admin user (prompts for password)
sudo ntfy user add malloc47

# Create an API token for scripts
sudo ntfy token add malloc47

# Grant access to all topics
sudo ntfy access malloc47 '*' rw
```

The user database persists at `/var/lib/ntfy-sh/user.db` across rebuilds.

## 8. AdGuard Home initial setup

Visit `https://adguard.home.malloc47.com` and complete the setup wizard:

1. Confirm DNS listen port (53) and web UI port (3000)
2. Set admin credentials
3. Configure upstream DNS if prompted (declarative config should pre-fill these)

## 9. Android ntfy app

1. Add server: `https://ntfy-ext.malloc47.com`
2. Login with the ntfy credentials from step 7
3. Subscribe to your topic (e.g. `unimatrix-alerts`)

On LAN, you can alternatively use `https://ntfy.home.malloc47.com` directly.

## Verification checklist

```bash
# AdGuard Home DNS
dig @192.168.1.10 example.com

# Caddy + ACME cert
curl -I https://adguard.home.malloc47.com

# Authelia
curl -I https://auth.home.malloc47.com

# ntfy (replace token)
curl -H "Authorization: Bearer tk_..." \
  -d "Bootstrap test" \
  https://ntfy.home.malloc47.com/unimatrix-alerts

# Cloudflare Tunnel (from outside LAN)
curl -H "Authorization: Bearer tk_..." \
  -d "Tunnel test" \
  https://ntfy-ext.malloc47.com/unimatrix-alerts
```
