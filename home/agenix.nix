{ inputs }:
{ config, ... }:
{
  imports = [
    ../modules/settings.nix
    inputs.agenix.homeManagerModules.default
  ];

  age.identityPaths = [
    "${inputs.personal}/age/${config.settings.profile}/keys.txt"
  ];
}
