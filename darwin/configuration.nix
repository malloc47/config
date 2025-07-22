{ pkgs, config, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  environment.systemPackages = with pkgs; [
    curl
    docker-compose
    exfat
    feh
    gitMinimal
    man-pages
    man-pages-posix
    vim
    watch
    wget
  ];

  nix.settings.experimental-features = "nix-command flakes";

  system = {
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };

    defaults = {
      NSGlobalDomain = {
        KeyRepeat = 2;
        InitialKeyRepeat = 25;
        ApplePressAndHoldEnabled = false;
        NSWindowShouldDragOnGesture = true;
        NSAutomaticWindowAnimationsEnabled = false;
      };

      CustomUserPreferences = {
        # Run ./keyboard-to-nix.sh to snapshot current keyboard settings
        "com.apple.symbolichotkeys" = (import ../darwin/keyboard.nix);
        "org.nixos.xquartz.X11" = {
          enable_render_extension = 1;
          no_auth = true;
          nolisten_tcp = false;
        };
        "org.xquartz.X11" = {
          enable_render_extension = 1;
          no_auth = true;
          nolisten_tcp = false;
        };
      };

      dock = {
        mru-spaces = false;
        expose-group-apps = true;
      };
    };

    activationScripts.extraActivation.text = let
      user = config.settings.username;
    in ''
      # These initializations may be needed, but waiting until something blows up to confirm
      # sudo -u ${user} -- plutil -insert 'Window Settings' -json '{}' /Users/${user}/Library/Preferences/com.apple.Terminal.plist > /dev/null 2>&1 || true
      # sudo -u ${user} -- plutil -insert 'Window Settings'.Basic -json '{}' /Users/${user}/Library/Preferences/com.apple.Terminal.plist > /dev/null 2>&1 || true
      sudo -u ${user} -- plutil -replace 'Window Settings'.Basic.useOptionAsMetaKey -bool YES /Users/${user}/Library/Preferences/com.apple.Terminal.plist
      sudo -u ${user} -- defaults write com.apple.Terminal "Default Window Settings" -string "Basic"
    '';
  };

  security.pam.services.sudo_local.touchIdAuth = true;
}
