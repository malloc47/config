{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./settings.nix ];

  config = {

    services.xserver = {
      dpi = config.settings.dpi;
      enable = true;
      desktopManager.xterm.enable = false;
      windowManager.i3.enable = true;
      autorun = true;
      autoRepeatDelay = 250;
    };

    services.displayManager.defaultSession = "none+i3";

    home-manager.users.${config.settings.username} = {
      xresources.properties."Xft.dpi" = config.settings.dpi;
      home.pointerCursor = {
        size = 64;
        package = pkgs.vanilla-dmz;
        name = "Vanilla-DMZ";
      };
      xsession.enable = true;
    };
  };
}
