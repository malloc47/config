(final: prev: {
  adguardian = prev.callPackage adguardian/default.nix { };

  # Home Assistant custom components. Called through home-assistant's OWN
  # python3Packages so buildHomeAssistantComponent and their deps resolve
  # against HA's interpreter, not the default python3Packages set.
  home-assistant-toniebox =
    final.home-assistant.python3Packages.callPackage home-assistant-toniebox/default.nix
      { };
  home-assistant-gtfs2 =
    final.home-assistant.python3Packages.callPackage home-assistant-gtfs2/default.nix
      { };

  # Lovelace (frontend) card — plain JS, so a normal callPackage (not HA's
  # python set). Registered via services.home-assistant.customLovelaceModules.
  home-assistant-flex-table-card = prev.callPackage home-assistant-flex-table-card/default.nix { };
  ha-puppet = prev.callPackage ha-puppet/default.nix { };
  home-assistant-eink-dashboard-card =
    prev.callPackage home-assistant-eink-dashboard-card/default.nix
      { };
  home-assistant-family-calendar-card =
    prev.callPackage home-assistant-family-calendar-card/default.nix
      { };
  home-assistant-kindle-weather-card =
    prev.callPackage home-assistant-kindle-weather-card/default.nix
      { };
})
