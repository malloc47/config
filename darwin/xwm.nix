{
  config,
  pkgs,
  ...
}:
with pkgs.lib;
{
  imports = [
    ../modules/settings.nix
  ];

  home.packages = with pkgs; [
    i3
    feh
    firefox
  ];

  home.file."xinitrc" = {
    target = ".xinitrc";
    text = ''
      #!/usr/bin/env bash
      exec ${pkgs.i3}/bin/i3
    '';
  };

}
