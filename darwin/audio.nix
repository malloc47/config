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
    switchaudio-osx
  ];
}
