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

  fonts.packages = with pkgs; [
    corefonts
    geosanslight
    inconsolata-unstable
    libertine
    libre-baskerville
    emacs-all-the-icons-fonts
    roboto-mono
    nerd-fonts.fira-code
    roboto-slab
  ];

  nix.settings.experimental-features = "nix-command flakes";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ../pkgs/default.nix) ];

  # services.karabiner-elements.enable = true;

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

        # Visual optimization
        NSAutomaticWindowAnimationsEnabled = false;
        NSScrollAnimationEnabled = false;
        NSWindowResizeTime = .001;
      };

      CustomUserPreferences = {
        NSGlobalDomain = {
          NSBrowserColumnAnimationSpeedMultiplier = 0.0;
          NSScrollViewRubberbanding = 0;
          QLPanelAnimationDuration = 0.0;
          NSDocumentRevisionsWindowTransformAnimation = false;
          NSToolbarFullScreenAnimationDuration = 0.0;
        };

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

        "com.apple.finder".DisableAllAnimations = true;

        "com.apple.dock" = {
          springboard-show-duration = 0.0;
          springboard-hide-duration = 0.0;
          springboard-page-duration = 0.0;
        };
      };

      dock = {
        mru-spaces = false;
        expose-group-apps = true;

        # Disable animations
        autohide-time-modifier = 0.0;
        autohide-delay = 0.0;
        expose-animation-duration = 0.0;
        mineffect = "scale";
      };

    };

    activationScripts.extraActivation.text =
      let
        user = config.settings.username;
      in
      ''
        # These initializations may not be needed, but waiting until something blows up to confirm
        # sudo -u ${user} -- plutil -insert 'Window Settings' -json '{}' /Users/${user}/Library/Preferences/com.apple.Terminal.plist > /dev/null 2>&1 || true
        # sudo -u ${user} -- plutil -insert 'Window Settings'.Basic -json '{}' /Users/${user}/Library/Preferences/com.apple.Terminal.plist > /dev/null 2>&1 || true
        sudo -u ${user} -- plutil -replace 'Window Settings'.Basic.useOptionAsMetaKey -bool YES /Users/${user}/Library/Preferences/com.apple.Terminal.plist
        sudo -u ${user} -- plutil -replace 'Window Settings'.Basic.shellExitAction -integer 1 /Users/${user}/Library/Preferences/com.apple.Terminal.plist
        sudo -u ${user} -- defaults write com.apple.Terminal "Default Window Settings" -string "Basic"
      '';
  };

  security.pam.services.sudo_local.touchIdAuth = true;
}
