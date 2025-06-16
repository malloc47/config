{
  modulesPath, lib, pkgs, ...  } @ args:
{
  imports = [
    (modulesPath + "/virtualisation/vmware-guest.nix")
    ./disk-config.nix
    ../modules/settings.nix
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontSize = 9.0;
    xkbFile = "vm";
    terminal = "kitty";
    extraGroups = ["audio" "docker" "networkmanager" "wheel" "lxd"];
  };

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  services.openssh.enable = true;

  virtualisation.vmware.guest.enable = true;

  nix.settings.experimental-features = "nix-command flakes";

  environment.systemPackages = map lib.lowPrio [
    pkgs.curl
    pkgs.gitMinimal
  ];

  users.users.root.openssh.authorizedKeys.keys =
  [ (builtins.readFile ../personal/ssh/malloc47/id_rsa.pub) ];

  system.stateVersion = "25.05";
}
