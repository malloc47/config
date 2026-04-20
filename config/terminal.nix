{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) stdenv;
  # Ghostty modifier names after key-remap.  On macOS the remap swaps
  # Command↔Option so "alt" = physical Command and "super" = physical
  # Option.  On Linux no remap is active; names match physical keys.
  cmd = if stdenv.isDarwin then "alt" else "super";
  opt = if stdenv.isDarwin then "super" else "alt";
in
{
  imports = [
    ../modules/settings.nix
    ./ghostty-terminfo.nix
  ];

  programs.ghostty = {
    enable = true;
    # nixpkgs ghostty requires wayland and doesn't build on darwin;
    # set package = null so the module manages config without installing.
    # macOS gets the binary via the Homebrew cask (see darwin/homebrew.nix).
    package = lib.mkIf stdenv.isDarwin null;
    settings = {
      # font, colors, and opacity are managed by Stylix
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor";
      window-decoration = if (stdenv.isDarwin) then "auto" else "none";
      window-theme = "system";
      quit-after-last-window-closed = true;
      macos-option-as-alt = true;
      macos-hidden = "always";
      split-divider-color = "#${config.lib.stylix.colors.base03}";
      # On macOS, swap Command (super) and Option (alt) so Ghostty
      # matches GUI Emacs, where mac-command-modifier = 'meta
      # (Command = M-) and mac-option-modifier = nil (Option = s-).
      # On Linux, Alt is already Meta — no remap needed.
      key-remap = lib.mkIf stdenv.isDarwin [
        "super=alt"
        "alt=super"
      ];
      # Keybinds reference `cmd` / `opt` so the physical keys stay the
      # same across platforms despite the macOS-only key-remap.
      keybind = [
        # Pass Command+F / Command+B through to Emacs
        "${cmd}+f=text:f"
        "${cmd}+b=text:b"
        # Split navigation matching Emacs windmove M-h/j/k/l.
        # performable: only consumes the key when a split exists in that
        # direction; otherwise the keypress passes through to the
        # application (so Emacs windmove still works via M-h/j/k/l).
        "performable:${cmd}+h=goto_split:left"
        "performable:${cmd}+j=goto_split:down"
        "performable:${cmd}+k=goto_split:up"
        "performable:${cmd}+l=goto_split:right"
        # Split management under Option+x (macOS) / Alt+x (Linux) leader,
        # mirroring Emacs C-x:
        #   0 = close current split    (C-x 0  delete-window)
        #   1 = zoom/unzoom split      (C-x 1  delete-other-windows)
        #   2 = split below            (C-x 2  split-window-below)
        #   3 = split right            (C-x 3  split-window-right)
        "${opt}+x>0=close_surface"
        "${opt}+x>1=toggle_split_zoom"
        "${opt}+x>2=new_split:down"
        "${opt}+x>3=new_split:right"
      ];
      # Stylix scales font-size by 4/3 on macOS to normalise across DPI differences
      # between Ghostty (72dpi base) and the OS (96dpi). In practice this reads as
      # too large on Retina displays where Ghostty already handles HiDPI itself.
      # Ghostty uses last-value-wins for duplicate keys, so this overrides Stylix's
      # scaled value without touching the global settings.fontSize.
    }
    // lib.optionalAttrs stdenv.isDarwin {
      font-size = builtins.floor config.settings.fontSize;
    };
  };

  programs.dircolors.enable = true;
}
