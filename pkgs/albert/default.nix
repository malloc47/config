{
  lib,
  fetchurl,
  undmg,
  stdenv,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "albert";
  version = "v0.31.1";
  src =
    let
      inherit (stdenv.hostPlatform) system;
      selectSystem = attrs: attrs.${system} or (throw "Unsupported system: ${system}");
      suffix = selectSystem {
        x86_64-darwin = "x86_64";
        aarch64-darwin = "arm64";
      };
      hash = selectSystem {
        x86_64-darwin = "";
        aarch64-darwin = "sha256-SJAwhrzFjT2bfqzqrhV3TV6kEvobuuL4JF4ChW7UJ2o=";
      };

    in
    fetchurl {
      url = "https://github.com/albertlauncher/albert/releases/download/${finalAttrs.version}/Albert-${finalAttrs.version}-${suffix}.dmg";
      inherit hash;
    };

  sourceRoot = "Albert.app";

  nativeBuildInputs = [ undmg ];

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/Applications/Albert.app"
    cp -R . "$out/Applications/Albert.app"
    mkdir "$out/bin"
    ln -s "$out/Applications/Albert.app/Contents/MacOS/Albert" "$out/bin/albert"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Application launcher";
    homepage = "https://albertlauncher.github.io/";
    license = licenses.unfree;
    platforms = platforms.darwin;
  };
})
