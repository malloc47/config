{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) stdenv;
in
{
  imports = [ ../modules/settings.nix ];

  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        padding = {
          x = 2;
          y = 2;
        };
        decorations = if (stdenv.isDarwin) then "Buttonless" else "Full";
        dynamic_title = true;
        option_as_alt = "Both";
      };
      scrolling = {
        history = 10000;
        multiplier = 3;
      };
      # Font and colors managed by Stylix (config/theme.nix)
      mouse = {
        hide_when_typing = false;
        bindings = [
          {
            mouse = "Middle";
            action = "PasteSelection";
          }
        ];
      };
      selection = {
        semantic_escape_chars = ",│`|:\"' ()[]{}<>";
        save_to_clipboard = false;
      };
      cursor = {
        style = "Block";
        unfocused_hollow = true;
        thickness = 1.0;
      };
      keyboard.bindings = [
        {
          key = "F";
          mods = "Super";
          action = "ReceiveChar";
        }
        {
          key = "B";
          mods = "Super";
          action = "ReceiveChar";
        }
      ];
      general.live_config_reload = true;
    };
  };

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
      # Pass Super+F and Super+B through to Emacs (mirrors alacritty config)
      keybind = [
        "super+f=text:f"
        "super+b=text:b"
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

  # Build a modified xterm-ghostty terminfo with colors#16777216 (truecolor).
  # Ghostty's upstream terminfo ships colors#256 and a Tc flag; Emacs's C-level
  # init_tty reads the terminfo colors count directly, so emacsclient -t only
  # gets 256 colors unless the terminfo itself declares 16777216.  This
  # activation script patches the entry and installs it into ~/.terminfo/ so it
  # takes precedence over the system copy.
  home.activation.ghostty-terminfo =
    let
      # On macOS (Homebrew install) the terminfo lives inside the .app bundle.
      # On NixOS the ghostty package puts it in $out/share/terminfo.
      ghosttyTerminfo =
        if stdenv.isDarwin then
          "/Applications/Ghostty.app/Contents/Resources/terminfo"
        else
          "${pkgs.ghostty}/share/terminfo";
      infocmp = if stdenv.isDarwin then "/usr/bin/infocmp" else "${pkgs.ncurses}/bin/infocmp";
      tic = if stdenv.isDarwin then "/usr/bin/tic" else "${pkgs.ncurses}/bin/tic";
      sed = if stdenv.isDarwin then "/usr/bin/sed" else "${pkgs.gnused}/bin/sed";
    in
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      GHOSTTY_TERMINFO="${ghosttyTerminfo}"
      if [ -d "$GHOSTTY_TERMINFO" ]; then
        TERMINFO_DIRS="$GHOSTTY_TERMINFO" ${infocmp} -x xterm-ghostty 2>/dev/null \
          | ${sed} 's/colors#256/colors#16777216/' \
          | ${tic} -x -o "$HOME/.terminfo" - 2>/dev/null || true
      fi
    '';

  programs.dircolors.enable = true;
}
