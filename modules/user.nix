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
      openssh.authorizedKeys.keys = [
        (builtins.readFile (../personal/ssh + "/${config.settings.profile}/id_rsa.pub"))
      ];
      shell = pkgs.zsh;
    }
    // lib.optionalAttrs (!stdenv.isDarwin)
    {
      isNormalUser = true;
      extraGroups = config.settings.extraGroups;
    });

    settings.xkbFile = lib.mkIf (config.settings.vm) (lib.mkDefault "vm");
    settings.terminal = lib.mkIf (config.settings.vm) (lib.mkDefault "kitty");

    settings.extraGroups = ["wheel"];
    nix.settings.trusted-users = [ "@wheel" ];

    security.sudo = {}
    // lib.optionalAttrs (!stdenv.isDarwin)
    { wheelNeedsPassword = false; };
  };
}
