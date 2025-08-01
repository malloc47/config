{
  lib,
  fetchurl,
  undmg,
  stdenv,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "swipe-aerospace";
  version = "0.2.2";
  src = fetchurl {
    url = "https://github.com/MediosZ/SwipeAeroSpace/releases/download/${finalAttrs.version}/SwipeAeroSpace.dmg";
    hash = "sha256-CWrQiNXQh4c9pNlJGzLzycfx6W4paZG/2iUhBhAikTw=";
  };

  sourceRoot = "SwipeAeroSpace.app";

  nativeBuildInputs = [ undmg ];

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/Applications/SwipeAeroSpace.app"
    cp -R . "$out/Applications/SwipeAeroSpace.app"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Gestures for moving between Aerospace workspaces";
    homepage = "https://github.com/MediosZ/SwipeAeroSpace";
    license = licenses.mit;
    platforms = platforms.darwin;
  };
})
