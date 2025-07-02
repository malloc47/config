{ config, pkgs, ... }:
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

  programs.ghostty = {
    enable = true;
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
}
