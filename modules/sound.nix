{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ./settings.nix ];

  config = {
    # hardware.pulseaudio.enable = true;
    # hardware.pulseaudio.support32Bit = true;

    settings.extraGroups = [ "audio" ];
  };
}
