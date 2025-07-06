{ modulesPath, lib, pkgs, config, ...  } @ args: {
  imports = [
    (modulesPath + "/virtualisation/vmware-guest.nix")
    ../modules/settings.nix
    ../nixos/configuration-flake.nix
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontSize = 10.0;
    dpi = 224;
  };
}
