# flex-table-card (custom-cards/flex-table-card) — a generic Lovelace table card.
# There is no bespoke gtfs2 card; gtfs2's "Visualizing the data" wiki drives its
# fancier layouts with stock community cards, and flex-table-card is the one that
# renders gtfs2's parallel next_departures* list attributes as a table of the
# next N upcoming trips. Not in nixpkgs, so packaged in-repo and exposed via
# overlays.default, then registered through services.home-assistant.custom-
# LovelaceModules (alongside apexcharts-card / plotly-chart-card).
#
# It ships a single prebuilt flex-table-card.js at the repo root (no build step),
# so this just installs that file into $out where the HA module picks it up.
{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "flex-table-card";
  version = "1.4";

  src = fetchFromGitHub {
    owner = "custom-cards";
    repo = "flex-table-card";
    tag = "v${finalAttrs.version}";
    hash = "sha256-UDgPijnAE5DNb0Kp3fj3bPjWCTZcmnBumggqtzSaieE=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 ./flex-table-card.js $out
    runHook postInstall
  '';

  meta = {
    description = "Highly customizable Lovelace table card (renders gtfs2 next_departures as a table)";
    homepage = "https://github.com/custom-cards/flex-table-card";
    changelog = "https://github.com/custom-cards/flex-table-card/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.gpl3Only;
    maintainers = [ ];
  };
})
