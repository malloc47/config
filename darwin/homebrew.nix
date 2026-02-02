{
  config,
  pkgs,
  inputs,
  ...
}:
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
      "albertlauncher/homebrew-albert" = inputs.albert-tap;
    };
    mutableTaps = false;
  };

  homebrew = {
    enable = true;
    casks = [
      "albert"
      "xquartz"
    ];
    brews = [
      "sendkeys"
    ];
    # caskArgs.no_quarantine = true;
    # onActivation.cleanup = "uninstall";
  };
}
