# NixOS Major Version Upgrade Runbook

A guide to upgrading both the `config` and `work-config` flakes from one stable
NixOS release to the next (e.g. 26.05 → 26.11).  The steps below are
version-agnostic; see the linked commits for the exact option renames and
workarounds that were needed at each release.

**NixOS release cadence:** `.05` releases in May, `.11` in November.
Check whether the new branch exists before starting:

```sh
curl -sI https://channels.nixos.org/nixos-26.11   # 302 → exists, 404 → not yet
```

---

## 1. Confirm the new release branch exists

```sh
curl -sI https://channels.nixos.org/nixos-X.11   # or X.05
```

A `302` redirect means the channel is live.  A `404` means wait — the branch
exists in nixpkgs but the channel isn't published yet.

Also verify the companion inputs have their release branches cut:
- `github:nix-community/home-manager/release-X.11`
- `github:LnL7/nix-darwin/nix-darwin-X.11`
- `github:danth/stylix/release-X.11`

---

## 2. Upgrade `config`

### 2a. Bump the version strings in `flake.nix`

Four inputs need to move to the new release.  Current strings look like:

```nix
nixpkgs.url        = "github:NixOS/nixpkgs/nixos-26.05";
home-manager.url   = "github:nix-community/home-manager/release-26.05";
nix-darwin.url     = "github:LnL7/nix-darwin/nix-darwin-26.05";
stylix.url         = "github:danth/stylix/release-26.05";
```

Change each `26.05` → `26.11` (or whatever the new version is).

> **Note on the nixpkgs URL:** track the *rolling* `nixos-X.11` branch rather
> than the bare `X.11` tag so the config keeps receiving security backports.
> The bare tag is frozen at release day.

### 2b. Relock

```sh
cd ~/src/config
nix flake update
```

### 2c. Fix evaluation errors

Attempt to build a representative host config and chase any renamed/removed
options that surface.  Typical error classes at each upgrade cycle:

- **Home-manager module renames** — especially `programs.ssh`, `programs.git`,
  and GUI-related modules get reworked most often.
- **`nodePackages.*` removals** — packages promoted to top-level drop their
  `nodePackages.` prefix.
- **`xorg.*` wrapper removals** — packages sometimes move out of the xorg
  attrset.
- **Desktop-manager/window-manager option paths** — Aerospace, i3, rofi options
  occasionally shift.
- **`allowUnfree` / security flags** — packages sometimes change license
  classification between releases.

Build the NixOS hosts to surface errors:

```sh
nix build .#nixosConfigurations.aida.config.system.build.toplevel --no-link
nix build .#nixosConfigurations.aroldo.config.system.build.toplevel --no-link
nix build .#nixosConfigurations.attila.config.system.build.toplevel --no-link
```

### 2d. Bump `stateVersion` in `flake.nix`

Search for all `stateVersion = "26.05"` (the *old* version) and update to
`"26.11"`.  Do **not** change the `system-manager` stateVersion — it uses a
numeric scheme (`stateVersion = 6`) that is unrelated to the NixOS release.

```sh
grep -n 'stateVersion' flake.nix
```

### 2e. Commit and push `config`

Commit **all changed files** (flake.nix, flake.lock, any host/module fixes) in
one commit.  Write a detailed commit message that lists every renamed option —
this is the primary audit trail for future upgrades.

```sh
git add flake.nix flake.lock <...any fixed modules...>
git commit -m "Upgrade to NixOS X.11

Bump nixpkgs to nixos-X.11 and home-manager / nix-darwin / stylix to
their X.11 releases; bump all stateVersions to X.11.

Fix options renamed or removed in X.11:
- <option> -> <new-option>  (reason)
- ...

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
git push origin master
```

---

## 3. Upgrade `work-config`

`work-config` upgrades in lockstep — the `config` flake input advances at the
same time as the native inputs.

### 3a. Bump the version strings in `flake.nix`

Same four inputs as in `config`:

```nix
nixpkgs.url      = "github:NixOS/nixpkgs/nixos-26.05";
home-manager.url = "github:nix-community/home-manager/release-26.05";
nix-darwin.url   = "github:LnL7/nix-darwin/nix-darwin-26.05";
```

(`system-manager` has no release-tied URL; leave it on the rolling ref.)

### 3b. Relock, pointing `config` at the new `master` HEAD

```sh
cd ~/src/work-config
nix flake update              # updates everything including the config input
```

Verify `config` resolved to the commit pushed in step 2:

```sh
python3 -c "import json;print(json.load(open('flake.lock'))['nodes']['config']['locked']['rev'])"
```

### 3c. Fix evaluation errors

Work-config tends to see fewer renames than `config` because it imports most
modules through the `config` flake input, but check for:

- **`xorg.*` path changes** that affect the work-specific host files.
- **`programs.ssh` `settings.*`** — the agent matchBlock rework appeared here
  too at 26.05.
- **`config.allowUnfree`** for any packages newly flagged unfree in the agent
  `homeConfiguration`.

Build the agent home config (the one that runs on this host):

```sh
nix build --no-link ".#homeConfigurations.agent.activationPackage"
```

Also evaluate the work NixOS hosts:

```sh
nix build --no-link ".#nixosConfigurations.drw.config.system.build.toplevel"
```

### 3d. Bump `stateVersion`

Same rule as `config`: change `"26.05"` → `"26.11"` everywhere, skip the
numeric `system-manager` version.

### 3e. Commit and push `work-config`

```sh
git add flake.nix flake.lock <...any fixed hosts...>
git commit -m "Upgrade to NixOS X.11 in lockstep with config

Bump nixpkgs to nixos-X.11 and home-manager/nix-darwin to their X.11
releases; bump config input to <sha7> and stateVersions to X.11.

Fix options renamed or removed in X.11:
- <option> -> <new-option>
- ...

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
git push origin main
```

---

## 4. Deploy

### 4a. Agent host (this machine)

```sh
cd ~/src/work-config
home-manager switch --flake .#agent
```

Evaluate whether to also activate the system-manager config:

```sh
sudo $(nix build --no-link --print-out-paths .#systemConfigs.agent)/bin/activate
```

### 4b. Other hosts

Deploy via the normal per-host mechanism (nixos-rebuild, darwin-rebuild, etc.).
Each host rebuilds against the new generation on its next scheduled update or
manual deploy.

---

## 5. Post-upgrade cleanup

After all hosts have been deployed and are stable:

- Fix any advisory warnings that appeared in the eval output (e.g. the
  `boot.zfs.forceImportRoot` advisory added at 26.05 that becomes the default
  at 26.11 — see `65b5937`).
- Remove any temporary compatibility shims added in the pre-upgrade period
  (see `eb45f98` for an example of a module option that was held back until the
  26.05 upgrade could land).
- Run `home-manager news` on active hosts to review the release notes.

---

## Known advisories heading into 26.11

These were noted during the 26.05 upgrade as things that will change at 26.11:

- `boot.zfs.forceImportRoot = false` becomes the default — no manual action
  needed, the explicit `false` in `vm-iso` can be removed (see `65b5937`).

---

## Version history

Every NixOS upgrade recorded in the `config` repo git history.
`git show <sha>` on any flake-era commit lists the exact option renames and
workarounds needed at that release.

Versions not listed were skipped entirely (19.09, 23.05, 24.11).

### Flake era (2025–present)

| Version | Date applied | Primary commit | Notes |
|---------|-------------|----------------|-------|
| **26.05** | 2026-08-09 | `63d28aa` | First Claude-assisted upgrade. `65b5937` silenced a ZFS advisory same day. `work-config` counterpart: `9302885`. |
| **25.11** | 2026-02-03 | `065ff2a` | Upgrade was folded into a darwin-focused commit ("Update darwin to 25.11"); `74a28df` (same day) was the follow-up cleanup. No dedicated `work-config` counterpart at the time. |
| **25.05** | 2025-06-13 | `8e7b218` | Darwin side first. Non-flake NixOS channel upgraded in `7f829b5` (2025-07-11). The VM sub-flake was folded into the root hierarchy at 25.05 in `37a445e` (2025-07-06), marking the start of the current flake structure. |

### Pre-flake / channel era (2019–2024)

These upgrades used `sudo nix-channel --add` + `sudo nixos-rebuild --upgrade boot`
rather than flake inputs, so no `flake.nix` changes are involved.

| Version | Date applied | Commit | Notes |
|---------|-------------|--------|-------|
| **24.05** | 2024-06-09 | `48fab32` | |
| **23.11** | 2024-05-18 | `4f17e56` | Applied ~18 months late; 24.05 followed three weeks later. |
| **22.11** | 2022-12-01 | `95c79bb` | |
| **22.05** | 2022-05-31 | `2bb7cbe` | |
| **21.11** | 2021-12-01 | `89041dc` | |
| **21.05** | 2021-09-25 | `7fb9d8c` | |
| **20.09** | 2020-10-31 | `f6cdf66` | |
| **20.03** | 2020-09-17 | `9261b4c` | |
| **19.03** | 2019-04-11 | `123565c` | Oldest NixOS upgrade in the repo. |
