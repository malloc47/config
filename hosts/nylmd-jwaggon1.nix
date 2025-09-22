{ pkgs, config, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  settings = {
    username = "jwaggoner";
    email = "jwaggoner@drwholdings.com";
    profile = "drw";
    fontName = "Roboto Mono";
    fontSize = 16.0;
  };
}
