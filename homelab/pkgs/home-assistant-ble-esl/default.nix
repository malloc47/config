# eigger/hass-ble-esl — Home Assistant integration (domain `ble_esl`) that
# writes images to Bluetooth LE electronic shelf labels over GATT, including the
# two Poshiji XTE tags on aida (PSJ-420 4.2" and PSJ-290 2.9", both BWRY). No
# base station, cloud or reflashing: HA's own bluetooth stack (aida's onboard
# adapter, or an ESPHome proxy with active connections) talks to the tag.
# Rendering is its bundled imagespec renderer (`ble_esl.write` payloads, plus a
# designer side panel); see docs/ble-esl.md.
#
# Not in nixpkgs, so packaged in-repo (like home-assistant-toniebox) and consumed
# through services.home-assistant.customComponents. Built through
# home-assistant's OWN python3Packages so its deps resolve against HA's
# interpreter. Two of its requirements (blesession, imagespec) and imagespec's
# datamatrix extra (pystrich) are not in nixpkgs either; they are packaged
# alongside in this directory. The rest (bluetooth-sensor-state-data,
# sensor-state-data) are.
#
# Runtime state (designer templates) goes to /var/lib/hass/ble_esl, never the
# component directory, so the read-only store copy is fine.
{
  lib,
  callPackage,
  fetchFromGitHub,
  buildHomeAssistantComponent,
  bluetooth-sensor-state-data,
  sensor-state-data,
}:

let
  pystrich = callPackage ./pystrich.nix { };
  blesession = callPackage ./blesession.nix { };
  imagespec = callPackage ./imagespec.nix { inherit pystrich; };
in
buildHomeAssistantComponent rec {
  owner = "eigger";
  domain = "ble_esl";
  version = "1.10.2";

  src = fetchFromGitHub {
    owner = "eigger";
    repo = "hass-ble-esl";
    rev = "v${version}";
    hash = "sha256-OrMNUYiPNHftBTqE4K7TTMt3KOOe2TEN64xGlvuQpyo=";
  };

  dependencies = [
    blesession
    bluetooth-sensor-state-data
    imagespec
    pystrich # imagespec[datamatrix]
    sensor-state-data
  ];

  passthru = { inherit blesession imagespec pystrich; };

  meta = {
    description = "Home Assistant integration for Bluetooth LE electronic shelf labels";
    homepage = "https://github.com/eigger/hass-ble-esl";
    changelog = "https://github.com/eigger/hass-ble-esl/releases/tag/v${version}";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
