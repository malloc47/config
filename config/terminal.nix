{ config, pkgs, ... }:
let
  inherit (pkgs) stdenv;
in
{
  imports = [ ../modules/settings.nix ];

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    shortcut = "u";
  };

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
      font = {
        normal = {
          family = config.settings.fontName;
          style = "Regular";
        };
        bold = {
          family = config.settings.fontName;
          style = "Bold";
        };
        italic = {
          family = config.settings.fontName;
          style = "Italic";
        };
        size = config.settings.fontSize;
        offset = {
          x = 0;
          y = 0;
        };
        glyph_offset = {
          x = 0;
          y = 0;
        };
      };
      # Colors managed by Stylix (config/theme.nix)
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
      general.live_config_reload = true;
    };
  };

  programs.ghostty = {
    enable = false;
    settings = {
      theme = "Builtin Solarized Light";
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor";
      window-decoration = "none";
      window-theme = "system";
      font-family = config.settings.fontName;
      font-size = builtins.floor config.settings.fontSize;
    };
  };

  programs.dircolors.enable = true;
}
