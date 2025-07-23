{ config, pkgs, lib, ... }:
{
  imports = [ ../modules/settings.nix ];
  config = {

    services = {} // lib.optionalAttrs config.settings.vm {
      openssh = lib.mkIf (config.settings.vm)  {
        enable = true;
        settings.X11Forwarding = true;
      };
    };

    home-manager.users.${config.settings.username} = {
      programs.ssh = {
        enable = true;
        controlPath = "~/.ssh/master-%C";
      };
      home.file."id_rsa" = {
        source = ./. + "/../personal/ssh/${config.settings.profile}/id_rsa";
        target = ".ssh/id_rsa";
      };
      home.file."id_rsa.pub" = {
        source = ./. + "/../personal/ssh/${config.settings.profile}/id_rsa.pub";
        target = ".ssh/id_rsa.pub";
      };
    };
  };
}
