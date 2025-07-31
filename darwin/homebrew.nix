{ config, pkgs, inputs, ... }:
{
  imports = [ ../modules/settings.nix ];

  nix-homebrew = {
    enable = true;
    enableRosetta = true;
    user = config.settings.username;
    taps = {
      "homebrew/homebrew-core" = inputs.homebrew-core;
      "homebrew/homebrew-cask" = inputs.homebrew-cask;
      "socsieng/homebrew-tap" = inputs.sendkeys-tap;
    };
    mutableTaps = false;
  };

  homebrew = {
    enable = true;
    casks = [ "gauntlet" ];
    brews = [
      "sendkeys"
    ];
  };
}
