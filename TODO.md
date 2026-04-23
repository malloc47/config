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

## Emacs

Ideas from [Stealing from the Best Emacs Configs](https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/):

- Disable bidirectional text scanning to improve redisplay performance in large buffers
- Skip fontification during active typing to reduce micro-stutters
- Increase process output buffer to 4MB for better LSP server performance
- Hide cursors in non-focused windows to reduce rendering overhead
- Preserve existing clipboard content before killing text in Emacs
- Remove duplicate entries from the kill ring to save slots
- Persist kill ring across sessions using savehist mode
- Strip text properties from kill ring entries to prevent file bloat
- Auto-execute shell scripts by adding executable permissions on save
- Switch re-builder to string syntax for more intuitive regexp development
- Disable ffap hostname pinging to prevent network-related freezes
- Enable minibuffer-regexp-mode for visual regexp structure feedback
- Use proportional window resizing when splitting frames
- Make C-x 1 toggle between single and multi-window layouts with winner-mode
- Restore previous window configurations instead of permanently deleting them
- Enable faster mark ring navigation with consecutive C-SPC presses
- Recenter view after save-place restores cursor position
- Auto-select help windows to streamline documentation viewing
- Use built-in lazy isearch counting for match statistics

## Terminal

- Ghostty shell integration
- Ghostty quick terminal setup / keybinding

## VPS (aroldo)

- [Beszel](https://github.com/henrygd/beszel) — lightweight server monitoring (CPU/RAM/disk) with hub on aroldo and agents on both machines
- [CrowdSec](https://www.crowdsec.net/) — collaborative intrusion prevention (modern fail2ban with crowd-sourced threat intel), has Caddy bouncer plugin
- [Headplane](https://github.com/tale/headplane) — web UI for Headscale node/ACL/DNS management without SSH
- [App Connectors](https://github.com/juanfont/headscale/pull/3121) — once landed (targeting v0.31.0), replace AdGuard DNS rewrites for vanity domains (e.g. status.malloc47.com) with Headscale ACL-based domain routing through tagged connector nodes
- [Vaultwarden](https://github.com/dani-garcia/vaultwarden) — lightweight Bitwarden server, benefits from always-on public availability separate from home server
- Offsite backup target — use aroldo as Restic/Borg destination over Tailscale for geographic redundancy (limited by 45GB disk)
- [Miniflux](https://miniflux.app/) / FreshRSS — lightweight RSS reader, always accessible
- DNS-over-HTTPS proxy — public encrypted DNS entry point forwarding to AdGuard over tailnet, works without Tailscale active on device

## Zellij

- [zbuffers](https://github.com/Strech/zbuffers) — buffer management plugin for zellij

## Theming

Stylix drift — the light path generally flows through `config.lib.stylix.colors`, but
several dark paths and older configs still hardcode solarized palettes, so switching
polarity won't propagate cleanly.

- Zellij: move the ~130-line inline `solarized-light-soft` KDL theme out of
  `config/home-ai.nix:51-172` — no stylix involvement at all. Either derive the RGB
  triples from `config.lib.stylix.colors` or document the decoupling.
- agent-deck: `theme = "light"` is hardcoded in `config/home-ai.nix:178`. Derive from
  `config.stylix.polarity` (or the same `~/.config/theme-mode` toggle tmux/emacs use)
  so it follows system dark mode.
- Emacs: `config/emacs.nix:38-55` hardcodes the solarized-dark base16 palette while
  the light path uses `config.lib.stylix.colors.withHashtag`. Asymmetric — either
  hardcode both or stylix-derive both.
- Tmux: `config/shell.nix:42-43` hardcodes `fg=#93a1a1,bg=#002b36` / `bg=#073642` for
  the dark session style; light path uses stylix. Same asymmetry as emacs.
- i3 / i3status / i3bar: `stylix.targets.i3.enable = false` (set in `config/flake.nix`)
  with a full hand-rolled solarized palette in `config/wm.nix:18-171`. Evaluate
  whether stylix's current i3 module is usable; if so, drop the manual colors.
- `config/theme.nix:13` passes `#fdf6e3` literally to ImageMagick for the blank
  wallpaper — replace with `config.lib.stylix.colors.base00` (or similar) for
  consistency.
- Ghostty's dual palette (`config/terminal.nix:36-83`, with
  `stylix.targets.ghostty.colors.enable = false`) is intentional — native OS-following
  `light:X,dark:X` theme switching. Keep as a documented exception, not drift.

## Repo Hygiene

- Normalize all repos on `main` branch (config and personal currently use `master`)
- Create an automatic (ideally evented or, at worst, time-based) way to automatically pull down config/ and work-config/ repo changes that I can run via launchd/systemd to pull down the latest github state so I don't forget to do a `git pull`.

## Security

- Audit SSH private keys ending up in `/nix/store` — flake inputs (personal, work-config's `./ssh`) are copied to the store world-readable; consider using agenix for private keys instead

## Network

Bifurcate wifi into separate LANs
