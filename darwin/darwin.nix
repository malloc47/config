{ pkgs, config,  ... }:
{
  imports = [
    ../modules/settings.nix
  ];

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  system.defaults.NSGlobalDomain = {
    KeyRepeat = 2;
    InitialKeyRepeat = 25;
    ApplePressAndHoldEnabled = false;
  };

  system.activationScripts.extraActivation.text = let
    user = config.settings.username;
  in ''
    # These initializations may be needed, but waiting until something blows up to confirm
    # sudo -u ${user} -- plutil -insert 'Window Settings' -json '{}' /Users/${user}/Library/Preferences/com.apple.Terminal.plist > /dev/null 2>&1 || true
    # sudo -u ${user} -- plutil -insert 'Window Settings'.Basic -json '{}' /Users/${user}/Library/Preferences/com.apple.Terminal.plist > /dev/null 2>&1 || true
    sudo -u ${user} -- plutil -replace 'Window Settings'.Basic.useOptionAsMetaKey -bool YES /Users/${user}/Library/Preferences/com.apple.Terminal.plist
    sudo -u ${user} -- defaults write com.apple.Terminal "Default Window Settings" -string "Basic"
  '';

  security.pam.services.sudo_local.touchIdAuth = true;
}
