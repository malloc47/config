{ config, pkgs, ... }:
{
  imports = [ ../modules/settings.nix ];


  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    shortcut = "u";
  };

  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.inconsolata-unstable;
      name = "${config.settings.fontName}";
    };
    settings = {

      font_size = toString config.settings.fontSize;

      background = "#fdf6e3";
      foreground = "#657b83";
      cursor = "#586e75";

      selection_background = "#475b62";
      selection_foreground = "#eae3cb";

      color0 = "#073642";
      color8 = "#002b36";

      color1 = "#dc322f";
      color9 = "#cb4b16";

      color2 = "#859900";
      color10 = "#586e75";

      color3 = "#b58900";
      color11 = "#657b83";

      color4 = "#268bd2";
      color12 = "#839496";

      color5 = "#d33682";
      color13 = "#6c71c4";

      color6 = "#2aa198";
      color14 = "#93a1a1";

      color7 = "#eee8d5";
      color15 = "#fdf6e3";
    };
  };

  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        padding = {
          x = 2;
          y = 2;
        };
        decorations = "full";
        dynamic_title = true;
        opacity = 1.0;
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
      colors = {
        primary = {
          background = "0xfdf6e3";
          foreground = "0x586e75";
        };
        normal = {
          black = "0xeee8d5";
          red = "0xdc322f";
          green = "0x859900";
          yellow = "0xb58900";
          blue = "0x268bd2";
          magenta = "0xd33682";
          cyan = "0x2aa198";
          white = "0x073642";
        };
        bright = {
          black = "0xfdf6e3";
          red = "0xcb4b16";
          green = "0x93a1a1";
          yellow = "0x839496";
          blue = "0x657b83";
          magenta = "0x6c71c4";
          cyan = "0x586e75";
          white = "0x002b36";
        };
        draw_bold_text_with_bright_colors = false;
      };
      mouse = {
        hide_when_typing = false;
        bindings = [
          {mouse = "Middle"; action = "PasteSelection";}
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
}
