# Android Termux and Nix

Reference date: 2026-06-17.

This document captures the current options for managing Android terminal
environments from this config repo. The repo is already flake-centered and
uses NixOS, nix-darwin, and Home Manager on other machines. Android is
different enough that it should be treated as its own target class, not just
another `aarch64-linux` host.

## Summary

The most Nix-native Android path is `nix-on-droid`, but it should be treated
as experimental and pinned separately from the main host matrix. Official
Termux remains the best-supported Android terminal environment, but it is not
natively controlled by Nix. Installing Nix directly into ordinary Termux and
using Home Manager is not a clean daily-driver path today; the viable versions
all involve `proot` or a Termux-derived environment.

Recommended approach:

1. Keep official Termux as the stable Android terminal.
2. Add a conservative Android Home Manager profile that contains only CLI
   userspace config: shell, git, ssh, editor files, and common CLI packages.
3. Test `nix-on-droid` as the Nix-native target for that profile.
4. If official Termux must remain the host, use a small bootstrap script plus
   dotfile management, and optionally run Nix/Home Manager inside a proot
   distro for isolated Linux-like workloads.

## Option: nix-on-droid

`nix-on-droid` is the most viable Nix-native option. It provides a
Termux-derived Android terminal app, a Nix installation, a module system, and
Home Manager integration. It supports flake-based rebuilds such as:

```bash
nix-on-droid switch --flake path#device
```

Pros:

- Most declarative/Nix-native Android path available.
- Includes Home Manager integration.
- Can reuse a subset of this repo's existing Home Manager modules.
- Available from F-Droid.
- Good fit for CLI tools, shell config, git, ssh, mosh, and editor setup.

Cons:

- Not full NixOS.
- Upstream describes the app as prototype/alpha quality.
- Uses a fork of Termux, not the official Termux distribution.
- Relies on Android/proot-specific machinery.
- Public release branches observed during research went only through
  `release-24.05`, while this repo tracks `nixpkgs` and Home Manager `25.11`.
- Not all Termux ecosystem assumptions and plugins apply.

Practical repo guidance:

- Add a separate `nix-on-droid` flake input pinned to the latest supported
  upstream release branch rather than forcing it to follow the repo's primary
  `nixpkgs`.
- Create an Android-specific Home Manager module. Start with low-risk modules
  only: shell, git, ssh config, editor config, `jq`, `ripgrep`, `tree`, `zip`,
  `unzip`, and similar CLI packages.
- Avoid service modules, desktop modules, systemd assumptions, font/session
  plumbing, and anything GUI-oriented.

Sources:

- <https://github.com/nix-community/nix-on-droid>
- <https://nix-community.github.io/nix-on-droid/nix-on-droid-options.html>
- <https://f-droid.org/en/packages/com.termux.nix/>

## Option: official Termux plus bootstrap

Official Termux remains the most practical and Android-integrated terminal
environment. It uses Android-native packages built against Android's runtime
environment rather than ordinary glibc Linux userspace.

This makes it less Nix-native, but more reliable for day-to-day Android use.

Recommended declarative-ish control:

- Store a curated Termux package list in this repo.
- Provide a bootstrap script that runs `pkg upgrade` and installs the package
  list.
- Manage dotfiles with repo-native symlinks, `chezmoi`, or `yadm`.
- Keep Android-specific shell/editor branches separate from desktop Linux and
  Darwin assumptions.

Good fits:

- Shell config.
- Git config.
- SSH client config.
- Editor config.
- Package list replay.
- Small Android-specific helper scripts.

Poor fits:

- Fully reproducible package closures.
- Nixpkgs package selection.
- NixOS-like services.
- Home Manager modules that expect normal Linux session infrastructure.

Sources:

- <https://github.com/termux/termux-packages/wiki/package-management>
- <https://github.com/termux/termux-packages/wiki/Termux-execution-environment>
- <https://github.com/termux/termux-app/releases>
- <https://www.chezmoi.io/>
- <https://yadm.io/docs/bootstrap>

## Option: Nix inside official Termux

A clean native installation of Nix inside ordinary Termux is not currently a
good target for this repo.

The core problem is that Termux is not a normal Linux distribution. Termux
packages are Android binaries, use Android paths, and are linked for Android's
runtime environment. Nixpkgs packages generally assume a more conventional
Linux environment and `/nix/store` behavior. Older native-ish attempts have
depended on `proot` and patching.

The historical `nix-in-termux` project is archived and points users toward
`nix-on-droid`.

Bottom line: Home Manager can manage files if Nix can run, but getting Nix to
run natively and reliably in official Termux is the hard part. Treat native
Nix-on-Termux as research, not an operational foundation.

Source:

- <https://github.com/t184256/nix-in-termux>

## Option: Termux plus proot-distro plus Nix/Home Manager

This is viable for experimentation and isolated dev environments:

1. Install official Termux.
2. Install `proot-distro`.
3. Create a Linux-like userland.
4. Install Nix inside that userland.
5. Run standalone Home Manager there.

Pros:

- Can provide a more conventional Linux userspace than native Termux.
- Allows Home Manager to work in a familiar environment.
- Keeps official Termux as the outer Android-integrated terminal.

Cons:

- More layers: Android -> Termux -> proot -> distro -> Nix -> Home Manager.
- Weaker Android integration from inside the proot environment.
- Not NixOS.
- PRoot limitations still apply.
- More bootstrap/state to debug.

Use this when a workload needs Linux compatibility more than Android
integration. Do not use it as the main phone terminal strategy unless the
extra layer is acceptable.

Source:

- <https://github.com/termux/proot-distro>

## Option: NixOS AVF

`nixos-avf` is an interesting future path for running something closer to real
NixOS on Android using Android Virtualization Framework.

Current viability is limited:

- It targets newer Android AVF support.
- Upstream documentation is Pixel/new-Android oriented.
- Some workflows require a debuggable Android build or root.
- It is better treated as a future watch item than a daily terminal setup.

Source:

- <https://github.com/nix-community/nixos-avf>

## Home Manager expectations on Android

Home Manager is useful for the subset of Android terminal state that is
ordinary user config:

- Shell startup files and prompt.
- Git config.
- SSH config.
- Editor config.
- XDG config files.
- CLI package lists when running under a Nix-capable environment.

Avoid or isolate:

- `systemd` user services.
- Desktop environment integrations.
- Font/session modules.
- GUI app modules.
- Modules that assume normal Linux FHS paths or daemon behavior.
- Secrets or agents that expect stable socket/session semantics unless tested.

For this repo, the right shape is probably an Android module that imports a
small subset of `home/default.nix` behavior without importing GUI, VM, or
service-heavy modules.

## Decision

Use two tracks:

1. Stable track: official Termux plus repo-managed bootstrap/dotfiles.
2. Nix-native track: nix-on-droid pinned independently, using a conservative
   Android Home Manager profile.

Do not try to make native Nix inside official Termux the primary strategy right
now. It is technically interesting, but the maintenance risk is higher than
`nix-on-droid` and the practical payoff is lower than a simple Termux
bootstrap for the stable path.
