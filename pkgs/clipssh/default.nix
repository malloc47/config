{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeWrapper,
  bash,
  coreutils,
  gnugrep,
  openssh,
  xclip,
  wl-clipboard,
  pngpaste,
}:

stdenvNoCC.mkDerivation rec {
  pname = "clipssh";
  version = "1.0.0-unstable-2026-05-20";

  src = fetchFromGitHub {
    owner = "samuellawrentz";
    repo = "clipssh";
    rev = "d2f35a31af60fd03945a9135e57ca62e35215731";
    hash = "sha256-sHWvPxNPEyYehJAhva01DhhGXBBYmkiHTJsM3LOhCYw=";
  };

  nativeBuildInputs = [ makeWrapper ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 clipssh "$out/bin/clipssh"
    patchShebangs "$out/bin/clipssh"

    wrapProgram "$out/bin/clipssh" \
      --prefix PATH : "${
        lib.makeBinPath (
          [
            coreutils
            gnugrep
            openssh
          ]
          ++ lib.optionals stdenvNoCC.hostPlatform.isLinux [
            xclip
            wl-clipboard
          ]
          ++ lib.optionals stdenvNoCC.hostPlatform.isDarwin [
            pngpaste
          ]
        )
      }"

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck

    "$out/bin/clipssh" --version | grep -F "clipssh 1.0.0"

    runHook postInstallCheck
  '';

  meta = {
    description = "Send clipboard screenshots to remote SSH hosts";
    homepage = "https://github.com/samuellawrentz/clipssh";
    license = lib.licenses.mit;
    mainProgram = "clipssh";
    platforms = lib.platforms.unix;
  };
}
