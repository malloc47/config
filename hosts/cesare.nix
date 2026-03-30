{ pkgs, config, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  settings = {
    fontSize = 14.0;
  };

  age.secrets.nix-access-tokens.file = ../secrets/nix-access-tokens.age;

  nix.extraOptions = ''
    !include ${config.age.secrets.nix-access-tokens.path}
  '';
}
