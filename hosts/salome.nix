{
  modulesPath,
  lib,
  pkgs,
  config,
  ...
}@args:
{
  imports = [
    (modulesPath + "/virtualisation/vmware-guest.nix")
    ../modules/settings.nix
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontName = "Roboto Mono";
    fontSize = 9.0;
    dpi = 224;
  };
}
