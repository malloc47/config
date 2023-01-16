{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ../nixos/configuration.nix
    ../modules/settings.nix
    ../hardware/lxc.nix
  ];

  settings = {
    vm = true;
    username = "jwaggoner";
    fontSize = 8.0;
    xkbFile = "vm";
  };

  # For VPN reasons, this container shares the host network namespace
  # so its resolv.conf needs to match the host
  networking.hostName = "drw";
  networking.domain = "us.drwholdings.com";
  networking.search = ["us.drwholdings.com" "drwholdings.com" "lan"];
  networking.firewall.enable = false;
  networking.nameservers = ["10.64.16.15" "10.64.16.16"];
  services.openssh.enable = false;

  users.users.${config.settings.username}.uid = lib.mkForce 34098;

  services.xserver.dpi = 162;

  home-manager.users.${config.settings.username} = {
    home.pointerCursor.size = 64;
    xresources.properties."Xft.dpi" = 162;
    # startx does not use .xsession but all the contents are valid for
    # an .xinitrc
    xsession.scriptPath = ".xinitrc";

    # Use the host's xsession
    home.sessionVariables = {
      DISPLAY = ":0";
    };

    programs.i3status.modules = {
      "ethernet _first_" = {
        position = 3;
        settings = {
          format_up = "E: %ip";
          format_down = "E";
        };
      };
    };
  };
}
