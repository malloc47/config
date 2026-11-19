# mosh from master, for OSC 8 hyperlinks (mobile-shell/mosh#1360, merged 2026-03-22).
# 1.4.0 (Oct 2022) is still the latest release and drops OSC 8, so a link an agent
# prints inside tmux loses its URL over mosh. Both ends need this build: the server
# sends screen updates to the client as escape sequences that the client re-parses
# (src/statesync/completeterminal.cc), so each side has to understand OSC 8.
# Drop this override once a release containing #1360 reaches nixpkgs.
{
  lib,
  mosh,
  fetchFromGitHub,
}:

mosh.overrideAttrs (old: {
  version = "1.4.0-unstable-2026-03-22";

  src = fetchFromGitHub {
    owner = "mobile-shell";
    repo = "mosh";
    rev = "decd9b705eb81626f694335b8d5940538beb06da";
    hash = "sha256-SsIj2JCDw7qkJ0NiX0FEQVYM0ATFpC9O/m7jycri0nU=";
  };

  # nixpkgs carries a fetchpatch of a master commit (protobuf 23 build fix) on top
  # of 1.4.0; master already has it. Keep only nixpkgs' local path patches.
  patches = lib.filter (p: !lib.isDerivation p) old.patches;
})
