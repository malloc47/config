# eink-dashboard-card — in-repo Lovelace card that lays out the whole 800x480
# 1-bit screen for the reTerminal E1001 (hosts/aida/esphome/e1001-clock.yaml).
# Puppet screenshots it and the panel draws the time and date into the blank
# clock area itself; see the header of eink-dashboard-card.js for the config.
#
# Plain JS with no build step. Ships DejaVu Sans (regular, bold and condensed)
# next to the card, which loads it relative to its own URL: Puppet's Chromium
# renders text 1-bit (services.ha-puppet.monochromeText), and DejaVu's
# hand-tuned TrueType hinting gives the cleanest small text in that mode
# (Inter, even run through ttfautohint, left notched joins at 15-21px).
{
  lib,
  stdenvNoCC,
  dejavu_fonts,
}:

stdenvNoCC.mkDerivation {
  pname = "eink-dashboard-card";
  version = "0.9.0";

  src = ./eink-dashboard-card.js;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 $src $out/eink-dashboard-card.js
    for f in DejaVuSans DejaVuSans-Bold DejaVuSansCondensed DejaVuSansCondensed-Bold; do
      install -m0644 ${dejavu_fonts}/share/fonts/truetype/$f.ttf $out/$f.ttf
    done
    runHook postInstall
  '';

  meta = {
    description = "Full-screen Lovelace card for an 800x480 1-bit e-paper panel rendered via Puppet";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
