# Shared SSH client config and key-file placement. Usable by:
#   - NixOS/darwin hosts via the bridge in modules/ssh.nix
#   - standalone home-manager (e.g. work-config's `agent` homeConfiguration)
{
  config,
  ...
}:
{
  imports = [ ../modules/settings.nix ];

  programs.ssh = {
    enable = true;
    # https://github.com/nix-community/home-manager/blob/bec08ef6e3b9d92f391a2940f6dbeffa50b17fa8/modules/programs/ssh.nix#L563-L574
    enableDefaultConfig = false;
    matchBlocks."*" = {
      forwardAgent = false;
      addKeysToAgent = "no";
      compression = false;
      # Keepalive: 30s * 6 = 180s blip tolerance. SSH-layer keepalive is
      # encrypted and detected in-band, unlike kernel TCPKeepAlive.
      serverAliveInterval = 30;
      serverAliveCountMax = 6;
      hashKnownHosts = false;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "auto";
      controlPath = "~/.ssh/master-%r@%n:%p";
      # Short enough that a silently-dead master doesn't poison new
      # invocations for long.
      controlPersist = "10m";
      extraOptions = {
        TCPKeepAlive = "no";
        ConnectTimeout = "10";
      };
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
}
