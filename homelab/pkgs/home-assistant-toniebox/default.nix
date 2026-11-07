# Unofficial Home Assistant integration for the Toniebox, talking to the Tonie
# Cloud (my.tonies.com) API. Not in nixpkgs and not a core integration, so it is
# packaged here and consumed via services.home-assistant.customComponents (see
# nixos/modules/home-automation.nix), the same way as the packaged emporia_vue.
#
# Built with buildHomeAssistantComponent via home-assistant.python3Packages so it
# lands in HA's own Python set (3.14), matching the running interpreter. Its only
# runtime requirement is paho-mqtt>=2.0 (manifest: iot_class cloud_push — the
# Tonie cloud streams box state over MQTT), already in nixpkgs.
#
# Configured via the UI config flow with a Tonie Cloud account.
{
  lib,
  fetchFromGitHub,
  buildHomeAssistantComponent,
  paho-mqtt,
}:

buildHomeAssistantComponent rec {
  owner = "git4sim";
  domain = "toniebox";
  version = "4.0.2";

  src = fetchFromGitHub {
    owner = "git4sim";
    repo = "HA-Toniebox";
    rev = "v${version}";
    hash = "sha256-49McD1OtVVTKyOns3WQXdXwA1ximfDBIgLemcHsfsaM=";
  };

  dependencies = [
    paho-mqtt
  ];

  meta = {
    description = "Unofficial Home Assistant integration for the Toniebox via the Tonie Cloud";
    homepage = "https://github.com/git4sim/HA-Toniebox";
    changelog = "https://github.com/git4sim/HA-Toniebox/releases/tag/v${version}";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
