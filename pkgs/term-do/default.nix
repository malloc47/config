{ lib
, stdenv
, fetchFromGitHub
, makeWrapper
, gnumake
, gcc
, boost}:

stdenv.mkDerivation rec {
  pname = "term-do";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "malloc47";
    repo = pname;
    rev = "7048f78f1d01f1edb01fb1869c41d617c938f8b4";
    sha256 = "1pw4583sc4yvd2x5i5f470l6pxcpyykk6a5c0di1c7kmfr5rn1wf";
  };

  nativeBuildInputs = [
    makeWrapper
  ];

  buildInputs = [
    gnumake
    gcc
    boost
  ];

  outputs = [ "out" ];

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
