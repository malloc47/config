{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  gnumake,
  gcc,
  boost,
}:

stdenv.mkDerivation rec {
  pname = "term-do";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "malloc47";
    repo = pname;
    rev = "540ba27fd48d5a952b9e7469e45f83af49eda396";
    sha256 = "0wnny9n3vbik1b69lyxwdfy7rrxwclxngnbpv1scs6d411gzklhm";
  };

  nativeBuildInputs = [
    makeWrapper
  ];

  buildInputs = [
    gnumake
    gcc
    boost
  ];

  buildPhase = ''
    make all
  '';

  installPhase = ''
    make DESTDIR=$out install
    mkdir -p $out/etc/term-do/
    cp launcher $out/etc/term-do/
    makeWrapper $out/bin/term-core $out/bin/term-do \
        --set TD_PLUGINS $out/lib/term-do/plugins/ \
        --set TD_LAUNCHER $out/etc/term-do/launcher
  '';

  meta = {
    description = "Hybrid of gnome-do and ido mode for a standard vt100 terminal";
    homepage = "https://github.com/malloc47/term-do";
    license = lib.licenses.bsd2;
    platforms = lib.platforms.linux;
  };
}
