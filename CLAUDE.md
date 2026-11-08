# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a public Nix/NixOS/nix-darwin configuration repository managing personal machines via a single flake. It exports reusable modules consumed by a separate private `work-config` repository for work machines.

The home network (aida, aroldo, Home Assistant and friends) lives in the separate private `lab-config` repo (`~/src/lab-config`), which consumes this flake like work-config does. Homelab work happens there, not here.

## Multi-Repo Architecture

This repo (`config`) is one of five repositories:

```
          ai-dev (public)
           │          │
           v          v
personal → config ← work-config
(private)  (public)   (private)
             ^
             │
         lab-config
          (private)

work-config and lab-config import:
            config.nixosModules.*
            config.darwinModules.*
            config.homeManagerModules.*
            config.hardwareModules.*
            config.diskModules.*
            config.overlays.default
```

- **config** (this repo): Public personal NixOS/nix-darwin configs + exported modules
- **personal**: Private repo consumed as a `flake = false` input; contains SSH keys and secrets
- **work-config**: Private repo for work machines; imports config's modules via flake input
- **lab-config**: Private repo for the home network (aida, aroldo); imports config's modules via flake input, with nixpkgs/home-manager/etc. following config's pins
- **ai-dev**: Standalone AI development toolkit, consumed by both config and work-config

All cross-repo dependencies use flake inputs — no git submodules between repos.

## Commands

```bash
# Build a NixOS host
nix build .#nixosConfigurations.<host>.config.system.build.toplevel

# Build a macOS (nix-darwin) host
nix build .#darwinConfigurations.<host>.system

# Build the installer ISO
nix build .#nixosConfigurations.vm-iso.config.system.build.isoImage

# Format Nix files
nix fmt

# Update flake inputs
nix flake update

# Evaluate all hosts (via taskfile)
task eval
```

## Architecture

### Flake Outputs

**NixOS hosts** (`nixosConfigurations`):
- `salome` — aarch64-linux, VMware Fusion guest on M-series Mac
- `attila` — x86_64-linux, headless Dell XPS 13 9315 laptop
- `vm-iso` — aarch64-linux, installer ISO image

aida and aroldo are defined in lab-config.

**nix-darwin hosts** (`darwinConfigurations`):
- `cesare` — aarch64-darwin, personal MacBook Pro (M-series)

**Exported modules** (consumed by work-config and lab-config):
- `nixosModules` — settings, user, ssh, nixpkgs, networking, virtualization, sound, gui, motd, mosh, vmware-guest, configuration-flake, stylix, theme
- `homeManagerModules` — osconfig-bridge, home, home-dev, home-gui, home-vm, ssh, stylix, theme, home-ai, home-agenix, home-gh, home-sqlit, home-drift-check, homelab-tools
- `darwinModules` — configuration, homebrew, home, stylix, theme
- `hardwareModules` — lxc, vmware-fusion-arm, dell-xps-9315
- `diskModules` — vmware-fusion, dell-xps-9315
- `overlays.default` — general packages only; homelab packages are in lab-config's `pkgs/`

### Module Composition

Each host configuration in `flake.nix` assembles a stack of modules:
1. Shared modules from `nixos/modules/` and `modules/settings.nix`
2. Host-specific config from `hosts/<name>.nix`
3. Hardware config from `hardware/<type>.nix`
4. Disk layout from `disk/<type>.nix` (for flake-managed hosts using disko)
5. Central NixOS config from `nixos/configuration-flake.nix`
6. Home-manager wired in as a NixOS/darwin module pointing to `home/default.nix` (Linux) or `darwin/home.nix` (macOS)

### Key Abstraction: `modules/settings.nix`

A custom options module that normalizes per-host differences, avoiding host-name conditionals throughout the config. Key options:
- `settings.sshKeys` — path to directory containing SSH key profiles (set to `"${inputs.personal}/ssh"` in this repo, `./ssh` in work-config)
- `settings.vm` — boolean, suppresses keybindings already handled by the VM host
- `settings.fontSize` / `settings.dpi` / `settings.fontName` — display scaling for Retina vs non-Retina
- `settings.xkbFile` — selects a keyboard layout from `xkb/`
- `settings.profile` — groups machines by identity (e.g., `malloc47`) to load the right SSH keys from `config.settings.sshKeys + "/<profile>/"`

### Userspace (Home-Manager)

- Linux entrypoint: `home/default.nix`
- macOS entrypoint: `darwin/home.nix`
- All standalone dotfiles live in `home/` alongside `default.nix`
- Dotfiles use three modes: raw files via `home.file.<n>.source`, inline strings via `home.file.<n>.text`, or fully Nix-generated (no separate file)
- Userspace config is shared: work-config imports `config.homeManagerModules.*` so both personal and work machines get the same dotfiles

### macOS-Specific (`darwin/`)

- Homebrew managed declaratively via `nix-homebrew` (`darwin/homebrew.nix`)
- macOS system defaults, keyboard (Karabiner), and window manager config in `darwin/configuration.nix`
- Custom packages in `pkgs/` (albert, autoraise, etc.)

### Homelab (lab-config)

aida and aroldo, their modules, packages, secrets and runbooks live in `~/src/lab-config` (git@github.com:malloc47/lab-config, private), which takes this repo as its `config` input. What stays here:

- `home/modules/homelab-tools.nix` — opt-in operator commands `esphome-deploy`, `z2m-foldin`, `ha-foldin`, enabled on the personal hosts only; they act on `programs.homelab-tools.repo` (default `~/src/lab-config`).
- `nixos-deploy <host>` (shell-personal) deploys from `~/src/lab-config` when it has `hosts/<host>.nix`, otherwise from this repo.
- Changes to exported modules can break aida/aroldo: evaluate lab-config against this checkout with `task eval-local` there (`--override-input config path:../config`), push config, then `nix flake update config` in lab-config.

### CI (`--override-input personal`)

CI cannot access the private `personal` repo. The GitHub Actions workflow creates a stub directory with a dummy SSH key and passes it via `nix eval --override-input personal path:$stub`.

## Mistakes to Avoid

- Files that are new to the repository need to be added to the git staging area before trying to deploy

- Local build tests (nix build .#nixosConfigurations.<host>.config.system.build.toplevel) of builds intended for another host will only work if they share the same target architecture

- When modifying exported modules, test that work-config and lab-config still evaluate (the `nix eval` commands in work-config's taskfile; `task eval-local` in lab-config)

- Don't put homelab-only code (Home Assistant, Zigbee, ESPHome, aida/aroldo services, their packages or tooling) in this repo: it ships to work machines. It belongs in lab-config, or in an opt-in module the personal hosts enable
