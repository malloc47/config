(final: prev: {
  albert = prev.callPackage albert/default.nix { };
  autoraise = prev.callPackage autoraise/default.nix { };
  geosanslight = prev.callPackage geosanslight/default.nix { };
  inconsolata-unstable = prev.callPackage inconsolata-unstable/default.nix { };
  aws-okta = prev.callPackage aws-okta/default.nix { };
  adguardian = prev.callPackage adguardian/default.nix { };
  term-do = prev.callPackage term-do/default.nix { };
  carve = prev.callPackage carve/default.nix { };
  swipe-aerospace = prev.callPackage swipe-aerospace/default.nix { };
  workmux = prev.callPackage workmux/default.nix { };
  worktrunk = prev.callPackage worktrunk/default.nix { };
  pertmux = prev.callPackage pertmux/default.nix { };
  clipaste = prev.callPackage clipaste/default.nix { };
  clipssh = prev.callPackage clipssh/default.nix { };
  claude-history = prev.callPackage claude-history/default.nix { };

  # Home Assistant custom component. Called through home-assistant's OWN
  # python3Packages so buildHomeAssistantComponent and its deps (paho-mqtt)
  # resolve against HA's interpreter, not the default python3Packages set.
  home-assistant-toniebox =
    final.home-assistant.python3Packages.callPackage home-assistant-toniebox/default.nix
      { };
})
