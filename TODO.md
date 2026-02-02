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
