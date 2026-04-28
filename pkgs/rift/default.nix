{
  lib,
  stdenv,
  fetchurl,
}:

stdenv.mkDerivation rec {
  pname = "rift";
  version = "0.4.0";

  src = fetchurl {
    url = "https://github.com/acsandmann/rift/releases/download/v${version}/rift-universal-macos-${version}.tar.gz";
    hash = "sha256-i5o6QfK6wJVHJaQDbCuJrUiM94u7XaPLvaBF30usfBg=";
  };

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp rift rift-cli $out/bin/
    chmod +x $out/bin/rift $out/bin/rift-cli

    runHook postInstall
  '';

  meta = with lib; {
    description = "A tiling window manager for macOS with built-in focus-follows-mouse";
    homepage = "https://github.com/acsandmann/rift";
    license = licenses.mit;
    platforms = [ "aarch64-darwin" "x86_64-darwin" ];
    mainProgram = "rift";
  };
}
