{ config, pkgs, ... }:
{
  imports = [
    ./settings.nix
  ];

  config = {
    users.users.${config.settings.username} = {
      isNormalUser = true;
      createHome = true;
      home = "/home/${config.settings.username}";
      description = "Jarrell Waggoner";
      extraGroups = ["audio" "docker" "networkmanager" "wheel" "lxd"];
      uid = 1000;
    };
  };
}
