{ config, lib, pkgs, ... }:
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
    # xterm-ghostty terminfo is only available via the nixpkgs package; on macOS
    # where we install via Homebrew, use xterm-256color which Emacs already knows.
    } // lib.optionalAttrs stdenv.isDarwin {
      term = "xterm-256color";
    };
  };

  programs.dircolors.enable = true;
}
