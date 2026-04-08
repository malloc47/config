# TODO

Capture ideas and pain points to work on when I have bandwidth:

## Darwin

- Get https://albertlauncher.github.io/ to properly link so I can deprecate the homebrew version
- Build aarch64-darwin support for [ActivityWatch](https://github.com/NixOS/nixpkgs/blob/nixos-25.05/pkgs/applications/office/activitywatch/wrapper.nix)
- Compile emacs from pkgs-unstable to work around NixOS/nixpkgs#395169
- https://github.com/FelixKratz/SketchyBar
- Idea: Run `i3wm` in full-screen XQuartz and run-natively-compiled X applications inside it
  - Chrome could be a challenge--I doubt it's commonly-compiled with X support against aarch64-darwin
  - This would be a continued disinvestment in Wayland
  - A lot of my darwin setup has prioritized cross-platform software, regardless
  - https://cmacr.ae/blog/seamless-x11-on-osx/
- Idea: Make xquartz-wm expose modern Accessibility API hooks so X programs can be managed by MacOS
  window managers (like Aerospace)
  - https://apple.stackexchange.com/questions/354676/how-can-i-disable-xquartz-entirely-and-use-xorg-with-my-own-desktop-environment
- `copyq` integration with Alfred https://github.com/albertlauncher/albert-plugin-python-copyq
  - Might work to get images working
- nix-ify my ukelele keyboard setup
  - Bug: fix the "switching back to US" that happens when opening certain Settings windows or
    TextEdit
- Keyboard: Ctrl+Shift+Return as paste
- Darwin: Remove LogiOptions+ in favor of Karabiner
  - Then layer https://github.com/jtroo/kanata/ on top of the Karabiner virtual keyboard
- Ghostty / Kitty for better [Claude Code integration](https://code.claude.com/docs/en/terminal-config)

## VPS (aroldo)

- [Beszel](https://github.com/henrygd/beszel) — lightweight server monitoring (CPU/RAM/disk) with hub on aroldo and agents on both machines
- [CrowdSec](https://www.crowdsec.net/) — collaborative intrusion prevention (modern fail2ban with crowd-sourced threat intel), has Caddy bouncer plugin
- [Headplane](https://github.com/tale/headplane) — web UI for Headscale node/ACL/DNS management without SSH
- [App Connectors](https://github.com/juanfont/headscale/pull/3121) — once landed (targeting v0.31.0), replace AdGuard DNS rewrites for vanity domains (e.g. status.malloc47.com) with Headscale ACL-based domain routing through tagged connector nodes
- [Vaultwarden](https://github.com/dani-garcia/vaultwarden) — lightweight Bitwarden server, benefits from always-on public availability separate from home server
- Offsite backup target — use aroldo as Restic/Borg destination over Tailscale for geographic redundancy (limited by 45GB disk)
- [Miniflux](https://miniflux.app/) / FreshRSS — lightweight RSS reader, always accessible
- DNS-over-HTTPS proxy — public encrypted DNS entry point forwarding to AdGuard over tailnet, works without Tailscale active on device

## Repo Hygiene

- Normalize all repos on `main` branch (config and personal currently use `master`)

## Security

- Audit SSH private keys ending up in `/nix/store` — flake inputs (personal, work-config's `./ssh`) are copied to the store world-readable; consider using agenix for private keys instead

## Network

Bifurcate wifi into separate LANs
