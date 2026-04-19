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
    # nixpkgs ghostty only builds on Linux; macOS uses the Homebrew cask
    enable = !stdenv.isDarwin;
    settings = {
      theme = "Builtin Solarized Light";
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor";
      window-decoration = if (stdenv.isDarwin) then "auto" else "none";
      window-theme = "system";
      font-family = config.settings.fontName;
      font-size = builtins.floor config.settings.fontSize;
      # Pass Super+F and Super+B through to Emacs (mirrors alacritty config)
      keybind = [
        "super+f=text:f"
        "super+b=text:b"
      ];
    };
  };

  # On macOS, programs.ghostty is disabled (nixpkgs build requires wayland),
  # so write the config file directly. Use term=xterm-256color because the
  # xterm-ghostty terminfo entry isn't available outside the nixpkgs package.
  home.file.".config/ghostty/config" = lib.mkIf stdenv.isDarwin {
    text = ''
      term = xterm-256color
      theme = Builtin Solarized Light
      cursor-style = block
      cursor-style-blink = false
      shell-integration-features = no-cursor
      window-decoration = auto
      window-theme = system
      font-family = ${config.settings.fontName}
      font-size = ${toString (builtins.floor config.settings.fontSize)}
      keybind = super+f=text:f
      keybind = super+b=text:b
    '';
  };

  programs.dircolors.enable = true;
}
