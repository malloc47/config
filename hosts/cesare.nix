{ pkgs, config, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  settings = {
    fontSize = 14.0;
  };
}
