{ pkgs, config, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  settings = {
    username = "jwaggoner";
    email = "jwaggoner@drwholdings.com";
    profile = "drw";
    fontName = "Menlo";
    fontSize = 14.0;
  };
}
