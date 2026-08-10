# Opinionated SSH client config and personal key placement. Usable by:
#   - NixOS/darwin hosts via the bridge in nixos/modules/ssh.nix
#   - standalone home-manager (e.g. work-config's `agent` homeConfiguration)
{
  config,
  lib,
  ...
}:
let
  cfg = config.programs.ssh-personal;
in
{
  imports = [ ../../modules/settings.nix ];

  options.programs.ssh-personal = {
    enable = lib.mkEnableOption "opinionated SSH client config and personal key placement";
  };

  config = lib.mkIf cfg.enable {
    programs.ssh = {
      enable = true;
      # https://github.com/nix-community/home-manager/blob/bec08ef6e3b9d92f391a2940f6dbeffa50b17fa8/modules/programs/ssh.nix#L563-L574
      enableDefaultConfig = false;
      # 26.05: matchBlocks + extraOptions are deprecated; settings.* is a
      # freeform block whose attrs are raw OpenSSH directives (upstream names).
      settings."*" = {
        ForwardAgent = false;
        AddKeysToAgent = "no";
        Compression = false;
        # Keepalive: 30s * 6 = 180s blip tolerance. SSH-layer keepalive is
        # encrypted and detected in-band, unlike kernel TCPKeepAlive.
        ServerAliveInterval = 30;
        ServerAliveCountMax = 6;
        HashKnownHosts = false;
        UserKnownHostsFile = "~/.ssh/known_hosts";
        ControlMaster = "auto";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        # Short enough that a silently-dead master doesn't poison new
        # invocations for long.
        ControlPersist = "10m";
        TCPKeepAlive = "no";
        ConnectTimeout = "10";
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
}
