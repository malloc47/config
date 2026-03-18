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
      home.file."id_ed25519" = {
        source = ./. + "/../personal/ssh/${config.settings.profile}/id_ed25519";
        target = ".ssh/id_ed25519";
      };
      home.file."id_ed25519.pub" = {
        source = ./. + "/../personal/ssh/${config.settings.profile}/id_ed25519.pub";
        target = ".ssh/id_ed25519.pub";
      };
    };
  };
}
