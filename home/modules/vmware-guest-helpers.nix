{ config, lib, ... }:
let
  cfg = config.programs.vmware-guest-helpers;
in
{
  options.programs.vmware-guest-helpers = {
    enable = lib.mkEnableOption "VMware host-share setup script (~/bin/setup-shared)";
  };

  config = lib.mkIf cfg.enable {
    home.file."setup-shared" = {
      target = "bin/setup-shared";
      executable = true;
      text = ''
        #!/usr/bin/env bash
        mkdir $HOME/shared 2>/dev/null
        vmhgfs-fuse .host:/shared $HOME/shared -o subtype=vmhgfs-fuse
      '';
    };
  };
}
