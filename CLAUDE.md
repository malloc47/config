# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Nix/NixOS/nix-darwin configuration repository managing multiple machines via a single flake. The repo has partially adopted flakes — legacy `configuration.nix`-style files coexist with the flake-based entrypoints.

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
```

## Architecture

### Flake Outputs

**NixOS hosts** (`nixosConfigurations`):
- `salome` — aarch64-linux, VMware Fusion guest on M-series Mac
- `drw` — x86_64-linux, LXC container on Ubuntu 22.04 (work machine)
- `drw-vmware` — aarch64-linux, VMware Fusion guest for work machine
- `aida` — x86_64-linux, baremetal on gmktec-g10 mini PC
- `vm-iso` — aarch64-linux, installer ISO image

**nix-darwin hosts** (`darwinConfigurations`):
- `cesare` — aarch64-darwin, personal MacBook Pro (M-series)
- `nylmd-jwaggon1` — aarch64-darwin, work MacBook Pro (M-series)

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
- `settings.vm` — boolean, suppresses keybindings already handled by the VM host
- `settings.fontSize` / `settings.dpi` / `settings.fontName` — display scaling for Retina vs non-Retina
- `settings.terminal` — terminal emulator choice (alacritty vs others)
- `settings.xkbFile` — selects a keyboard layout from `xkb/`
- `settings.profile` — groups machines by identity (e.g., `malloc47`, `drw`) to load the right SSH keys from `personal/ssh/<profile>/`

### Userspace (Home-Manager)

- Linux entrypoint: `config/home.nix`
- macOS entrypoint: `darwin/home.nix`
- All standalone dotfiles live in `config/` alongside `home.nix`
- Dotfiles use three modes: raw files via `home.file.<n>.source`, inline strings via `home.file.<n>.text`, or fully Nix-generated (no separate file)

### LXC Container Strategy (`drw`)

On work Linux hardware, NixOS runs inside an LXC container (not a VM) to reduce overhead. The container shares the host X server rather than running its own:
- The container filesystem is bind-mounted on the host at `~/lxc-share`
- The host i3 config symlinks to the container's generated i3 config via that mount
- Keybindings in the `drw` host config wrap application launches with `lxc exec nixos -- machinectl shell ...` so containerized apps appear in the host window manager
- Setup/reset script: `lxc/lxc.sh`

### macOS-Specific (`darwin/`)

- Homebrew managed declaratively via `nix-homebrew` (`darwin/homebrew.nix`)
- macOS system defaults, keyboard (Karabiner), and window manager config in `darwin/configuration.nix`
- Custom packages in `pkgs/` (albert, autoraise, aws-okta, etc.)

### OpenWRT Router (`hosts/openwrt/`)

UCI-format configuration files managed separately — not built by the flake. Applied manually to the router.

## Mistakes to Avoid

- Files that are new to the repository need to be added to the git staging area before trying to deploy

- Local build tests (nix build .#nixosConfigurations.<host>.config.system.build.toplevel) of builds intended for another host will only work if they share the same target architecture
