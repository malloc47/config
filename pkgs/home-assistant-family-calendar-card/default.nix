# family-calendar-card — in-repo Lovelace card for the kitchen's touchscreen
# family calendar (an Apolosign 15.6" panel running Kiosk Satellite): clock,
# today/tomorrow weather outlook, show/hide chips per calendar, a week grid and
# a strip of ordinary Lovelace cards for room controls. See the header of
# family-calendar-card.js for the config, and
# hosts/aida/home-assistant/family-calendar-dashboard.yaml for its dashboard.
#
# Plain JS with no build step. The weather outlook comes from
# ../lovelace-weather/weather.js (shared with eink-dashboard-card, so both
# word the forecast the same way), installed alongside and imported relative
# to the card.
{
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "family-calendar-card";
  version = "0.2.0";

  src = ./family-calendar-card.js;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 $src $out/family-calendar-card.js
    install -m0644 ${../lovelace-weather/weather.js} $out/weather.js
    runHook postInstall
  '';

  meta = {
    description = "Full-screen Lovelace family calendar for a wall touchscreen";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
