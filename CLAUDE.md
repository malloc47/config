# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a public Nix/NixOS/nix-darwin configuration repository managing personal machines via a single flake. It exports reusable modules consumed by a separate private `work-config` repository for work machines.

## Multi-Repo Architecture

This repo (`config`) is one of four repositories:

```
          ai-dev (public)
           │          │
           v          v
personal → config ← work-config
(private)  (public)   (private)

work-config imports:
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
- **ai-dev**: Standalone AI development toolkit, consumed by both config and work-config

All cross-repo dependencies use flake inputs — no git submodules between repos.

## Commands

```bash
# Build a NixOS host
nix build .#nixosConfigurations.<host>.config.system.build.toplevel

# Build a macOS (nix-darwin) host
nix build .#darwinConfigurations.<host>.system.build.toplevel

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
- `aida` — x86_64-linux, baremetal on gmktec-g10 mini PC
- `aroldo` — x86_64-linux, RackNerd VPS (Headscale + DERP relay)
- `attila` — x86_64-linux, headless Dell XPS 13 9315 laptop
- `vm-iso` — aarch64-linux, installer ISO image

**nix-darwin hosts** (`darwinConfigurations`):
- `cesare` — aarch64-darwin, personal MacBook Pro (M-series)

**Exported modules** (consumed by work-config):
- `nixosModules` — settings, user, ssh, nixpkgs, networking, virtualization, sound, gui, motd, vmware-guest, configuration-flake
- `homeManagerModules` — home, home-dev, home-gui, home-vm
- `darwinModules` — configuration, homebrew, home
- `hardwareModules` — lxc, vmware-fusion-arm
- `diskModules` — vmware-fusion
- `overlays.default`

### Module Composition

Each host configuration in `flake.nix` assembles a stack of modules:
1. Shared modules from `modules/` (settings, user, nixpkgs, networking, ssh, sound, gui, virtualization)
2. Host-specific config from `hosts/<name>.nix`
3. Hardware config from `hardware/<type>.nix`
4. Disk layout from `disk/<type>.nix` (for flake-managed hosts using disko)
5. Central NixOS config from `nixos/configuration-flake.nix`
6. Home-manager wired in as a NixOS/darwin module pointing to `config/home.nix` (Linux) or `darwin/home.nix` (macOS)

### Key Abstraction: `modules/settings.nix`

A custom options module that normalizes per-host differences, avoiding host-name conditionals throughout the config. Key options:
- `settings.sshKeys` — path to directory containing SSH key profiles (set to `"${inputs.personal}/ssh"` in this repo, `./ssh` in work-config)
- `settings.vm` — boolean, suppresses keybindings already handled by the VM host
- `settings.fontSize` / `settings.dpi` / `settings.fontName` — display scaling for Retina vs non-Retina
- `settings.xkbFile` — selects a keyboard layout from `xkb/`
- `settings.profile` — groups machines by identity (e.g., `malloc47`) to load the right SSH keys from `config.settings.sshKeys + "/<profile>/"`

### Userspace (Home-Manager)

- Linux entrypoint: `config/home.nix`
- macOS entrypoint: `darwin/home.nix`
- All standalone dotfiles live in `config/` alongside `home.nix`
- Dotfiles use three modes: raw files via `home.file.<n>.source`, inline strings via `home.file.<n>.text`, or fully Nix-generated (no separate file)
- Userspace config is shared: work-config imports `config.homeManagerModules.*` so both personal and work machines get the same dotfiles

### macOS-Specific (`darwin/`)

- Homebrew managed declaratively via `nix-homebrew` (`darwin/homebrew.nix`)
- macOS system defaults, keyboard (Karabiner), and window manager config in `darwin/configuration.nix`
- Custom packages in `pkgs/` (albert, autoraise, etc.)

### OpenWRT Router (`hosts/openwrt/`)

UCI-format configuration files managed separately — not built by the flake. Applied manually to the router.

### CI (`--override-input personal`)

CI cannot access the private `personal` repo. The GitHub Actions workflow creates a stub directory with a dummy SSH key and passes it via `nix eval --override-input personal path:$stub`.

## Mistakes to Avoid

- Files that are new to the repository need to be added to the git staging area before trying to deploy

- Local build tests (nix build .#nixosConfigurations.<host>.config.system.build.toplevel) of builds intended for another host will only work if they share the same target architecture

- When modifying exported modules, test that work-config still evaluates (the `nix eval` commands in work-config's taskfile)
