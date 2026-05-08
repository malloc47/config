{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.ghostty-personal;
  inherit (pkgs) stdenv;
  # Ghostty modifier names after key-remap.  On macOS the remap swaps
  # Command↔Option so "alt" = physical Command and "super" = physical
  # Option.  On Linux no remap is active; names match physical keys.
  cmd = if stdenv.isDarwin then "alt" else "super";
  opt = if stdenv.isDarwin then "super" else "alt";
in
{
  imports = [
    ../../modules/settings.nix
  ];

  options.programs.ghostty-personal = {
    enable = lib.mkEnableOption "personal Ghostty config (themes, keybinds, dircolors)";
  };

  config = lib.mkIf cfg.enable {
    # Disable Stylix's auto-generated Ghostty color theme so we can define
    # our own light/dark pair that Ghostty switches automatically based on
    # OS appearance.  Stylix still manages fonts and opacity.
    stylix.targets.ghostty.colors.enable = false;

    programs.ghostty = {
      enable = true;
      # nixpkgs ghostty requires wayland and doesn't build on darwin;
      # set package = null so the module manages config without installing.
      # macOS gets the binary via the Homebrew cask (see darwin/homebrew.nix).
      package = lib.mkIf stdenv.isDarwin null;
      # Dual solarized themes: Ghostty auto-switches based on OS dark mode
      # (macOS appearance / Linux XDG portal).  Palette mapping mirrors
      # Stylix's own modules/ghostty/hm.nix.
      themes = {
        solarized-light = {
          background = "fdf6e3";
          foreground = "586e75";
          cursor-color = "586e75";
          selection-background = "93a1a1";
          selection-foreground = "586e75";
          palette = [
            "0=#fdf6e3"
            "1=#dc322f"
            "2=#859900"
            "3=#b58900"
            "4=#268bd2"
            "5=#6c71c4"
            "6=#2aa198"
            "7=#586e75"
            "8=#839496"
            "9=#dc322f"
            "10=#859900"
            "11=#b58900"
            "12=#268bd2"
            "13=#6c71c4"
            "14=#2aa198"
            "15=#002b36"
          ];
        };
        solarized-dark = {
          background = "002b36";
          foreground = "93a1a1";
          cursor-color = "93a1a1";
          selection-background = "586e75";
          selection-foreground = "93a1a1";
          palette = [
            "0=#002b36"
            "1=#dc322f"
            "2=#859900"
            "3=#b58900"
            "4=#268bd2"
            "5=#6c71c4"
            "6=#2aa198"
            "7=#93a1a1"
            "8=#657b83"
            "9=#dc322f"
            "10=#859900"
            "11=#b58900"
            "12=#268bd2"
            "13=#6c71c4"
            "14=#2aa198"
            "15=#fdf6e3"
          ];
        };
      };
      settings = {
        # Font and opacity managed by Stylix; colors managed by themes above
        theme = "light:solarized-light,dark:solarized-dark";
        cursor-style = "block";
        cursor-style-blink = false;
        shell-integration-features = "no-cursor";
        window-decoration = if (stdenv.isDarwin) then "auto" else "none";
        window-theme = "system";
        quit-after-last-window-closed = true;
        scrollback-limit = 100000;
        macos-option-as-alt = true;
        # Allow OSC 52 clipboard writes from inside the terminal (e.g. tmux
        # forwarding mouse selections out to the system clipboard).  Default
        # is `ask`, which silently rejects programmatic writes when no user
        # is present to confirm.
        clipboard-write = "allow";
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
          # Copy/paste: performable copy only fires when there's a selection,
          # otherwise passes through (e.g., M-c in Emacs).
          "performable:${cmd}+c=copy_to_clipboard"
          "${cmd}+v=paste_from_clipboard"
          # Insert last argument (M-. in zsh/readline).  Ghostty's kitty
          # keyboard protocol encodes alt+period as a CSI sequence on Linux,
          # bypassing zsh's ^[. binding.  Explicit esc: forces the plain ESC+.
          # byte sequence that zsh expects.
          "alt+period=esc:."
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
          # Open scrollback in Emacs.  Ghostty uses open/xdg-open (not
          # $EDITOR) so macOS needs duti to set Emacs.app as the default
          # text handler (see darwin/home.nix).  ghostty-org/ghostty#2504
          "${cmd}+shift+s=write_scrollback_file:open"

          # Font size: bind explicitly under ${cmd} so they match physical Cmd
          # post-key-remap.  The Ghostty defaults are written against `super`,
          # which on this setup matches physical Option (not Cmd) after the
          # super=alt / alt=super remap.
          "${cmd}+equal=increase_font_size:1"
          "${cmd}+plus=increase_font_size:1"
          "${cmd}+minus=decrease_font_size:1"
          "${cmd}+zero=reset_font_size"

          # Disable Ghostty's built-in shortcuts that conflict with terminal-app
          # keybinds (Emacs, tmux, TUIs).  Selected via interactive audit; see
          # https://github.com/ghostty-org/ghostty/blob/main/src/config/Config.zig
          # `Keybinds.init` for the full list of defaults.  Font-size bindings
          # (cmd+=, cmd+-, cmd+0) are intentionally left active.
          #
          # Important: defaults are written against literal `super` / `ctrl` —
          # NOT the `${cmd}` macro.  On macOS Ghostty's keybind matcher sees
          # physical Cmd as `super` regardless of our key-remap, so unbinds
          # must use `super+...` to actually disable physical-Cmd shortcuts.
          # The macro `${cmd}` (= `alt` on Darwin) is only correct for our
          # *custom* bindings, where the remap routes physical Cmd → alt.
          #
          # Cross-platform defaults (ctrl_or_super → both ctrl and super match):
          "ctrl+tab=unbind"
          "ctrl+shift+tab=unbind"
          "ctrl+enter=unbind"
          "ctrl+shift+enter=unbind"
          "ctrl+shift+p=unbind"
          "ctrl+,=unbind"
          "ctrl+shift+,=unbind"
        ]
        ++ lib.optionals stdenv.isDarwin [
          # macOS-only defaults: all use literal `super` (= physical Cmd).
          # Window / surface / tab close + create + quit:
          "super+w=unbind"
          "super+shift+w=unbind"
          "super+alt+w=unbind"
          "super+shift+alt+w=unbind"
          "super+n=unbind"
          "super+t=unbind"
          "super+q=unbind"
          # Tab nav (goto_tab N + last_tab + tab cycling + split jumps):
          "super+1=unbind"
          "super+2=unbind"
          "super+3=unbind"
          "super+4=unbind"
          "super+5=unbind"
          "super+6=unbind"
          "super+7=unbind"
          "super+8=unbind"
          "super+9=unbind"
          "super+[=unbind"
          "super+]=unbind"
          "super+shift+[=unbind"
          "super+shift+]=unbind"
          # Edit / search:
          "super+f=unbind"
          "super+a=unbind"
          "super+k=unbind"
          # Splits / fullscreen / palette / undo / config:
          "super+d=unbind"
          "super+shift+d=unbind"
          "super+enter=unbind"
          "super+shift+enter=unbind"
          "super+shift+p=unbind"
          "super+z=unbind"
          "super+shift+z=unbind"
          "super+,=unbind"
          "super+shift+,=unbind"
        ]
        ++ lib.optionals stdenv.isLinux [
          # Linux defaults (mostly ctrl+shift; alt+digit for tabs):
          "ctrl+shift+w=unbind"
          "ctrl+shift+n=unbind"
          "ctrl+shift+t=unbind"
          "ctrl+shift+q=unbind"
          "ctrl+page_up=unbind"
          "ctrl+page_down=unbind"
          "ctrl+shift+left=unbind"
          "ctrl+shift+right=unbind"
          "ctrl+shift+page_up=unbind"
          "ctrl+shift+page_down=unbind"
          "alt+1=unbind"
          "alt+2=unbind"
          "alt+3=unbind"
          "alt+4=unbind"
          "alt+5=unbind"
          "alt+6=unbind"
          "alt+7=unbind"
          "alt+8=unbind"
          "alt+9=unbind"
          "ctrl+super+[=unbind"
          "ctrl+super+]=unbind"
          "ctrl+shift+f=unbind"
          "ctrl+shift+a=unbind"
          "ctrl+shift+o=unbind"
          "ctrl+shift+e=unbind"
          # Bare Esc is bound to `end_search` on Linux (performable, but the
          # pass-through path can interact badly with the start_search unbind
          # above).  Unbind explicitly so Esc always passes to the terminal.
          "escape=unbind"
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
  };
}
