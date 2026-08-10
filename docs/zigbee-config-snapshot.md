# Zigbee2MQTT config snapshot & fold-in

**Status:** implemented (aida). This is the *config-file backup* mechanism. It is
**not** the mesh reconciler in `docs/zigbee-declarative-reconciler.md` — that one
converges live groups/membership/**bindings** over the bridge API. This one just
captures the two UI-editable YAML files so a NixOS deploy doubles as a backup.

## What it covers (and what it doesn't)

The NixOS `services.zigbee2mqtt` module regenerates only `configuration.yaml`
(from `settings` in `nixos/modules/home-automation.nix`). Two files are left
Z2M-owned and mutable — the clickops surface:

- `devices.yaml` — friendly-name renames and per-device options
- `groups.yaml` — group definitions/options

**Out of scope:** device pairings / network keys live in `database.db` and
`coordinator_backup.json` (binary). Restoring the YAML alone gives you back
names/groups but not the paired mesh — plan a separate binary backup if you want
bare-metal recovery. Group *membership* and bindings are the reconciler's job.

## How it works (seed-then-warn, never clobber)

Mirrors `home/modules/drift-check.nix`, at the system level:

1. **Baseline** lives in the repo at `hosts/aida/zigbee2mqtt/{devices,groups}.yaml`.
2. **Seed** — `systemd.tmpfiles` `C` (copy-if-absent) restores the baseline only
   when the file is missing (fresh box / disaster recovery). It never overwrites
   a file Z2M is already managing.
3. **Drift-warn** — a `system.activationScripts` entry diffs the live file against
   the baseline on every `nixos-deploy` and prints a warning + diff if they
   differ. It is non-fatal and never touches the running file, so UI edits always
   win at runtime.

So a deploy is a safe restore point: it reminds you when aida has uncaptured
clickops state, but never destroys it.

## Fold-in workflow

Capture out-of-band UI edits back into the repo baseline:

```sh
z2m-foldin aida       # defined next to nixos-deploy in home/modules/shell-personal.nix
```

It fetches `aida:/var/lib/zigbee2mqtt/{devices,groups}.yaml` into the repo and
prints the diff. Review it and commit — **the commit is the backup**. Then a
subsequent `nixos-deploy aida` sees no drift.

Typical loop: clickops in the Z2M UI → `z2m-foldin aida` → review diff → commit.
