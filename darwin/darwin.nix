{ pkgs, config,  ... }:
{
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  system.defaults.NSGlobalDomain = {
    KeyRepeat = 2;
    InitialKeyRepeat = 25;
    ApplePressAndHoldEnabled = false;
  };
}
