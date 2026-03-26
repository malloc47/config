{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./terminal.nix
    ./wm.nix
  ];

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
      ${config.settings.terminal} -e nmtui
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
