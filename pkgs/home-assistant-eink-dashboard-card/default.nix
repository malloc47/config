# eink-dashboard-card — in-repo Lovelace card that lays out the whole 800x480
# screen (1-bit or 4-gray) for the reTerminal E1001
# (hosts/aida/esphome/e1001-clock.yaml).
# Puppet screenshots it and the panel draws the time and date into the blank
# clock area itself; see the header of eink-dashboard-card.js for the config.
#
# Plain JS with no build step. The forecast handling and today/tomorrow
# outlook live in ../lovelace-weather/weather.js (shared with
# family-calendar-card), installed alongside and imported relative to the card.
# Ships DejaVu Sans (regular, bold and condensed) next to the card, which
# loads it relative to its own URL. DejaVu's
# hand-tuned TrueType hinting gives the cleanest small text on the panel: with
# 1-bit text (services.ha-puppet.monochromeText) Inter, even run through
# ttfautohint, left notched joins at 15-21px, and DejaVu's anti-aliased edges
# also reduce cleanly to the 4-gray palette aida uses now.
{
  lib,
  stdenvNoCC,
  dejavu_fonts,
}:

stdenvNoCC.mkDerivation {
  pname = "eink-dashboard-card";
  version = "0.14.0";

  src = ./eink-dashboard-card.js;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 $src $out/eink-dashboard-card.js
    install -m0644 ${../lovelace-weather/weather.js} $out/weather.js
    for f in DejaVuSans DejaVuSans-Bold DejaVuSansCondensed DejaVuSansCondensed-Bold; do
      install -m0644 ${dejavu_fonts}/share/fonts/truetype/$f.ttf $out/$f.ttf
    done
    runHook postInstall
  '';

  meta = {
    description = "Full-screen Lovelace card for an 800x480 1-bit or 4-gray e-paper panel rendered via Puppet";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
