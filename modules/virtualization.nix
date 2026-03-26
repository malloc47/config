{ config, lib, ... }:
{
  imports = [ ./settings.nix ];

  config = {
    virtualisation.docker.enable = !config.settings.vm;
    virtualisation.docker.enableOnBoot = false;

    services.logind.settings.Login.RuntimeDirectorySize = "8G";

    virtualisation.incus.enable = !config.settings.vm;
    virtualisation.lxc.lxcfs.enable = !config.settings.vm;

    settings.extraGroups = [ "docker" ];
  };
}
