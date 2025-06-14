{ config, pkgs, ... }:
{
  imports = [
    ./settings.nix
  ];

  config = {
    users.users.${config.settings.username} = {
      isNormalUser = true;
      createHome = true;
      home = "/" + (if stdenv.isDarwin then "User" else "home") + "/" + config.settings.username;
      description = "Jarrell Waggoner";
      extraGroups = config.settings.extraGroups; # ["audio" "docker" "networkmanager" "wheel" "lxd"];
      uid = 1000;
    };
  };
}
