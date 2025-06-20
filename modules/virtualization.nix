{ lib, ... }:
{
  imports = [ ./settings.nix ];

  config = {
    virtualisation.docker.enable = true;
    virtualisation.docker.enableOnBoot = false;

    services.logind.extraConfig = ''
      RuntimeDirectorySize=8G
    '';

    virtualisation.lxd.enable = true;
    virtualisation.lxc.lxcfs.enable = true;

    settings.extraGroups = ["docker"];
  };
}
