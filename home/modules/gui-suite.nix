{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.gui-suite;
in
{
  imports = [
    ./ghostty-personal.nix
    ./i3-personal.nix
  ];

  options.programs.gui-suite = {
    enable = lib.mkEnableOption "personal Linux GUI suite (i3, Ghostty, browsers, content apps)";
  };

  config = lib.mkIf cfg.enable {
    programs.ghostty-personal.enable = true;
    programs.i3-personal.enable = true;

    home.packages = with pkgs; [
      anki
      evince
      feh
      firefox
      gimp
      scrot
    ];

    programs.chromium.enable = true;

    home.file."wifi" = {
      target = "bin/wifi";
      executable = true;
      text = ''
        #!/usr/bin/env bash
        ghostty -e nmtui
      '';
    };

    home.file."brightness" = {
      target = "bin/brightness";
      executable = true;
      text = ''
        #!/usr/bin/env bash
        sudo bash -c "echo $1 > /sys/class/backlight/mba6x_backlight/brightness"
      '';
    };
  };
}
