# kindle-weather-card — in-repo Lovelace card that lays out the whole
# 1072x1448 16-gray screen of a wall-mounted Kindle Paperwhite 3.
# Puppet screenshots it and the Kindle's dashboard loop
# (hosts/aida/kindle-weather) downloads and draws the image between suspends;
# see the header of kindle-weather-card.js for the config and
# docs/kindle-weather.md for the device side.
#
# Plain JS with no build step. The forecast handling and today/tomorrow
# outlook live in ../lovelace-common/weather.js (shared with the other cards),
# installed alongside and imported relative to the card. Ships DejaVu Sans, as
# eink-dashboard-card does, loaded relative to the card's own URL.
{
  lib,
  stdenvNoCC,
  dejavu_fonts,
}:

stdenvNoCC.mkDerivation {
  pname = "kindle-weather-card";
  version = "0.2.0";

  src = ./kindle-weather-card.js;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 $src $out/kindle-weather-card.js
    install -m0644 ${../lovelace-common/weather.js} $out/weather.js
    for f in DejaVuSans DejaVuSans-Bold DejaVuSansCondensed DejaVuSansCondensed-Bold; do
      install -m0644 ${dejavu_fonts}/share/fonts/truetype/$f.ttf $out/$f.ttf
    done
    runHook postInstall
  '';

  meta = {
    description = "Full-screen Lovelace weather card for a 1072x1448 16-gray Kindle, rendered via Puppet";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
