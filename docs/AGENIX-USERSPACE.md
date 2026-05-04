# Userspace agenix (home-manager secrets)

Reference for adding home-manager-scope secrets across personal machines.

## Setup state

- Identity: `~/src/personal/age/malloc47/keys.txt` (passphraseless age key, version-controlled in the private `personal` flake input).
- Public key: `age19a3dcu3xrl77my69a38uw62ret4edv2x87rkxedgndlpyacfsdeqkd0h2h`, registered in `secrets/secrets.nix` as `malloc47-user`.
- Wiring: `config/home-agenix.nix` imports `agenix.homeManagerModules.default` and sets `age.identityPaths` from the `personal` input. Imported by every host's home-manager (cesare, salome, attila, aida, aroldo).

Same identity works on all machines because it lives in the `personal` repo, available wherever the flake input is. No per-host bootstrap needed.

## Adding a new userspace secret

1. Edit `secrets/secrets.nix` — add an entry listing `malloc47-user` as a recipient. Add `malloc47` (your SSH key) too if you want to be able to decrypt/edit it from any machine without requiring the personal repo to be present.

   ```nix
   "my-secret.age".publicKeys = [ malloc47 malloc47-user ];
   ```

2. Create the encrypted file:

   ```bash
   cd ~/src/config/secrets
   agenix -e my-secret.age
   ```

3. Reference it from a home-manager module:

   ```nix
   age.secrets.my-secret.file = ../secrets/my-secret.age;
   # readable at config.age.secrets.my-secret.path
   ```

4. Rebuild. The decrypted secret lands at a runtime path owned by your user.

## What's appropriate for userspace agenix

Static, long-lived credentials that you control and that downstream tools only **read**:

- API tokens you mint yourself (GitHub PATs, OpenAI API keys, etc.)
- `.netrc` entries
- SMTP / IMAP passwords
- Per-user environment files for systemd units

## What's NOT appropriate

agenix decrypts to a **read-only** path. Anything the consuming app needs to write back to is a bad fit:

- OAuth session files that auto-refresh tokens (e.g., `~/.codex/auth.json`, `~/.config/gh/hosts.yml`) — let the app manage these directly, log in per host.
- Mutable application state of any kind.
- Anything that rotates faster than you'd want to re-encrypt.

For mutable per-machine credential state, prefer logging in per host, a password manager, or a live-sync mechanism (Syncthing on a private folder, etc.).

## Headless hosts

`home-agenix` is wired into aida and aroldo too, but they have no `age.secrets` declared at the home-manager level. The module is a no-op there until you declare one. Don't add userspace secrets on those servers unless there's a concrete user-level service that needs them — system-level `age.secrets` (already in use for Authelia, ntfy, Cloudflare, etc.) remain the right tool for system services.

## Key rotation

If the userspace age key ever needs to rotate:

1. Generate a new key at `~/src/personal/age/malloc47/keys.txt` (back up the old one first — anything still encrypted with only the old key is unrecoverable without it).
2. Update the `malloc47-user` value in `secrets/secrets.nix` with the new public key (`age-keygen -y < keys.txt`).
3. Re-encrypt all `.age` files that list `malloc47-user`: `cd secrets && agenix -r`.
4. Commit the personal repo and the config repo together; rebuild each host.

Same procedure if you ever want a per-host userspace identity (split `malloc47-user` into `cesare-user`, `attila-user`, etc.) — generate per host, add each as a recipient.
