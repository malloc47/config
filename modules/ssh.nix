{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ../modules/settings.nix ];
  config = {

    services =
      { }
      // lib.optionalAttrs config.settings.vm {
        openssh = lib.mkIf (config.settings.vm) {
          enable = true;
          settings.X11Forwarding = true;
        };
      };

    home-manager.users.${config.settings.username} = {
      programs.ssh = {
        enable = true;
        # https://github.com/nix-community/home-manager/blob/bec08ef6e3b9d92f391a2940f6dbeffa50b17fa8/modules/programs/ssh.nix#L563-L574
        enableDefaultConfig = false;
        matchBlocks."*" = {
          forwardAgent = false;
          addKeysToAgent = "no";
          compression = false;
          serverAliveInterval = 0;
          serverAliveCountMax = 3;
          hashKnownHosts = false;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "no";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "no";
        };
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
