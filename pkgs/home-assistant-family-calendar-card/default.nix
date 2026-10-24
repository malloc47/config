# family-calendar-card — in-repo Lovelace card for the kitchen's touchscreen
# family calendar (an Apolosign 15.6" panel running Kiosk Satellite): clock,
# today/tomorrow weather outlook, show/hide chips per calendar, a week grid, a
# strip of ordinary Lovelace cards for room controls and a notification bar.
# See the header of family-calendar-card.js for the config, and
# hosts/aida/home-assistant/family-calendar-dashboard.yaml for its dashboard.
#
# Plain JS with no build step. The weather outlook and the bottom bar's
# notices come from ../lovelace-common/weather.js and banner.js (shared with
# eink-dashboard-card, so both word the forecast and show the same notices),
# installed alongside and imported relative to the card.
{
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "family-calendar-card";
  version = "0.9.2";

  src = ./family-calendar-card.js;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    install -m0644 $src $out/family-calendar-card.js
    install -m0644 ${../lovelace-common/weather.js} $out/weather.js
    install -m0644 ${../lovelace-common/banner.js} $out/banner.js
    runHook postInstall
  '';

  meta = {
    description = "Full-screen Lovelace family calendar for a wall touchscreen";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
