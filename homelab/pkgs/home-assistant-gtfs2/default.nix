# vingerha/gtfs2 — Home Assistant integration that loads a full GTFS timetable
# into a local SQLite DB (via pygtfs) and OVERLAYS GTFS-RT on top. Unlike the
# core `gtfs` (schedule only) and the packaged `gtfs_realtime` (realtime only,
# shows "Unknown" when the RT feed has no entry for a stop), gtfs2 falls back to
# the SCHEDULED time when there is no live data and upgrades it with RT delays
# when there is — which is why it replaces gtfs_realtime for the Bee-Line here.
#
# Not in nixpkgs, so packaged in-repo (like home-assistant-toniebox) and exposed
# via overlays.default, then consumed through services.home-assistant.custom-
# Components. Built with buildHomeAssistantComponent through home-assistant's OWN
# python3Packages so its deps resolve against HA's interpreter (3.14).
#
# Configured via the UI config flow (no API key): a static GTFS zip URL plus an
# optional GTFS-RT trip-updates URL, then pick route/stop.
{
  lib,
  fetchFromGitHub,
  buildHomeAssistantComponent,
  pygtfs,
  gtfs-realtime-bindings,
  protobuf,
  sqlalchemy,
  six,
}:

buildHomeAssistantComponent rec {
  owner = "vingerha";
  domain = "gtfs2";
  # Upstream tags are bare (no leading "v"); note their manifest's internal
  # version field lags the tag (says 0.5.10.2 at tag 0.5.10.3) — harmless.
  version = "0.5.10.3";

  src = fetchFromGitHub {
    owner = "vingerha";
    repo = "gtfs2";
    rev = version;
    hash = "sha256-3ufQM98VQFPyAC13Vhxwr0gQS/RddiKm9YYPUM+Wsh8=";
  };

  # The upstream manifest hard-pins pygtfs==0.1.9 and gtfs-realtime-bindings==
  # 1.0.0, but nixpkgs ships 0.1.11 / 2.0.0. HA validates a custom component's
  # manifest requirements against what is installed and, on a mismatch, tries to
  # pip-install the exact pin at runtime — which fails on read-only NixOS. Relax
  # the pins to the nixpkgs versions. The GTFS-RT protobuf API is stable across
  # the bindings major bump (HA core already runs bindings 2.0.0 for the mta and
  # gtfs_realtime integrations).
  postPatch = ''
    substituteInPlace custom_components/gtfs2/manifest.json \
      --replace-fail '"pygtfs==0.1.9"' '"pygtfs==0.1.11"' \
      --replace-fail '"gtfs-realtime-bindings==1.0.0"' '"gtfs-realtime-bindings==2.0.0"'
  '';

  dependencies = [
    pygtfs
    gtfs-realtime-bindings
    protobuf
    sqlalchemy # gtfs_helper.py imports sqlalchemy.sql directly
    six # bundled requests_testadapter.py imports six
  ];

  meta = {
    description = "Home Assistant GTFS integration: static timetable (pygtfs) with a GTFS-RT overlay";
    homepage = "https://github.com/vingerha/gtfs2";
    changelog = "https://github.com/vingerha/gtfs2/releases/tag/${version}";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
