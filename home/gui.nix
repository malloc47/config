{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./modules/ghostty-personal.nix
    ./wm.nix
  ];

  programs.ghostty-personal.enable = true;

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
}
