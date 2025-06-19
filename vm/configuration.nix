{ modulesPath, lib, pkgs, config, ...  } @ args: {
  imports = [
    (modulesPath + "/virtualisation/vmware-guest.nix")
    ./disk-config.nix
    ../modules/settings.nix
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontSize = 9.0;
    extraGroups = ["audio" "docker" "wheel" "networkmanager"];
  };

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  networking.networkmanager.enable = true;
  networking.firewall.enable = false;
  networking.nameservers = ["8.8.8.8" "8.8.4.4"];
  #settings.extraGroups = ["networkmanager"];

  services.openssh.enable = true;

  virtualisation.vmware.guest.enable = true;

  nix.settings.experimental-features = "nix-command flakes";

  environment.systemPackages = with pkgs; map lib.lowPrio [
    curl
    gitMinimal
    inconsolata-unstable # proves that overlays work
  ];

  users.users.root.openssh.authorizedKeys.keys =
    [ (builtins.readFile ../personal/ssh/${config.settings.profile}/id_rsa.pub) ];

  system.stateVersion = "25.05";
}
