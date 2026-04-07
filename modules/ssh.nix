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
          controlMaster = "auto";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "1h";
        };
      };
      home.file."ssh-key" = {
        source = config.settings.sshKeys + "/${config.settings.profile}/${config.settings.sshKeyName}";
        target = ".ssh/${config.settings.sshKeyName}";
      };
      home.file."ssh-key-pub" = {
        source = config.settings.sshKeys + "/${config.settings.profile}/${config.settings.sshKeyName}.pub";
        target = ".ssh/${config.settings.sshKeyName}.pub";
      };
    };
  };
}
