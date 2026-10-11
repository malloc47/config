# eink-dashboard-card — in-repo Lovelace card that lays out the whole 800x480
# 1-bit screen for the reTerminal E1001 (hosts/aida/esphome/e1001-clock.yaml).
# Puppet screenshots it and the panel draws the time and date into the blank
# clock area itself; see the header of eink-dashboard-card.js for the config.
#
# Plain JS with no build step. Ships Inter (the family the panel's own clock
# uses) next to the card, which loads it relative to its own URL. nixpkgs' Inter
# is an unhinted variable font, so static instances of the weights the card
# uses are cut from it and run through ttfautohint: Puppet's Chromium renders
# text 1-bit (services.ha-puppet.monochromeText), and without hinting/dropout
# control strokes come out uneven and i-dots vanish at ~16px.
{
  lib,
  stdenvNoCC,
  inter,
  python3Packages,
  ttfautohint,
}:

stdenvNoCC.mkDerivation {
  pname = "eink-dashboard-card";
  version = "0.7.0";

  src = ./eink-dashboard-card.js;
  dontUnpack = true;

  nativeBuildInputs = [
    python3Packages.fonttools
    ttfautohint
  ];

  buildPhase = ''
    runHook preBuild
    for w in 500 600 700; do
      fonttools varLib.instancer -q ${inter}/share/fonts/truetype/InterVariable.ttf \
        wght=$w opsz=drop -o Inter-$w-instance.ttf
      ttfautohint --no-info Inter-$w-instance.ttf Inter-$w.ttf
    done
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 $src $out/eink-dashboard-card.js
    install -m0644 Inter-500.ttf Inter-600.ttf Inter-700.ttf $out/
    runHook postInstall
  '';

  meta = {
    description = "Full-screen Lovelace card for an 800x480 1-bit e-paper panel rendered via Puppet";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
