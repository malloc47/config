{ config, pkgs, lib, ... }:
let
  inherit (pkgs) stdenv;
in
{
  imports = [
    ./settings.nix
  ];

  config = {
    users.users.${config.settings.username} = ({
      createHome = true;
      home = "/" + (if (stdenv.isDarwin) then "Users" else "home") + "/" + config.settings.username;
      description = "Jarrell Waggoner";
      uid = 1000;
    }
    // lib.optionalAttrs (!stdenv.isDarwin)
    {
      isNormalUser = true;
      extraGroups = config.settings.extraGroups; # ["audio" "docker" "networkmanager" "wheel" "lxd"];
    });
  };
}
