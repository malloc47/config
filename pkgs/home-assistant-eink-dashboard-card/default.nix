# eink-dashboard-card — in-repo Lovelace card that lays out the whole 800x480
# 1-bit screen for the reTerminal E1001 (hosts/aida/esphome/e1001-clock.yaml).
# Puppet screenshots it and the panel draws the time and date into the blank
# clock area itself; see the header of eink-dashboard-card.js for the config.
#
# Plain JS with no build step. Ships Inter (the family the panel's own clock
# uses) next to the card, which loads it relative to its own URL.
{
  lib,
  stdenvNoCC,
  inter,
}:

stdenvNoCC.mkDerivation {
  pname = "eink-dashboard-card";
  version = "0.1.2";

  src = ./eink-dashboard-card.js;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 $src $out/eink-dashboard-card.js
    install -m0644 ${inter}/share/fonts/truetype/InterVariable.ttf $out/InterVariable.ttf
    runHook postInstall
  '';

  meta = {
    description = "Full-screen Lovelace card for an 800x480 1-bit e-paper panel rendered via Puppet";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
