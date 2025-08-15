{ config, lib, ... }:
{
  imports = [ ./settings.nix ];

  config = {
    virtualisation.docker.enable = !config.settings.vm;
    virtualisation.docker.enableOnBoot = false;

    services.logind.extraConfig = ''
      RuntimeDirectorySize=8G
    '';

    virtualisation.lxd.enable = !config.settings.vm;
    virtualisation.lxc.lxcfs.enable = !config.settings.vm;

    settings.extraGroups = [ "docker" ];
  };
}
