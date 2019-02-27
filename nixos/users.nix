{ config, pkgs, ... }:

{

   imports = [
     "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
   ];
  
  users.users.malloc47 = {
    isNormalUser = true;
    createHome = true;
    home = "/home/malloc47";
    description = "Jarrell Waggoner";
    extraGroups = ["wheel" "networkmanager" "audio"];
    uid = 1000;
  };

  users.users.eve.isNormalUser = true;

  home-manager.users.malloc47 = import ../config/home.nix ;
}
