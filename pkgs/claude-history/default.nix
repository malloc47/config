{
  lib,
  stdenv,
  stdenvNoCC,
  fetchurl,
  autoPatchelfHook,
}:

let
  version = "0.1.64";

  sources = {
    "x86_64-linux" = {
      asset = "claude-history-linux-amd64.tar.gz";
      hash = "sha256-/TYrTUxnemlblukg02XCY8ni9mwHcpnZM0MIlplyLpU=";
    };
    "aarch64-darwin" = {
      asset = "claude-history-darwin-arm64.tar.gz";
      hash = "sha256-afAnpZ2zIgLaU9WATL4VKuK9GUHG4V/29fF37to7DEI=";
    };
  };

  source =
    sources.${stdenv.hostPlatform.system}
      or (throw "claude-history: unsupported platform ${stdenv.hostPlatform.system}");
in
stdenvNoCC.mkDerivation {
  pname = "claude-history";
  inherit version;

  src = fetchurl {
    url = "https://github.com/raine/claude-history/releases/download/v${version}/${source.asset}";
    inherit (source) hash;
  };

  sourceRoot = ".";

  nativeBuildInputs = lib.optionals stdenvNoCC.hostPlatform.isLinux [
    autoPatchelfHook
  ];

  buildInputs = lib.optionals stdenvNoCC.hostPlatform.isLinux [
    stdenv.cc.cc.lib
  ];

  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 claude-history "$out/bin/claude-history"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    "$out/bin/claude-history" --version
    runHook postInstallCheck
  '';

  meta = {
    description = "Fuzzy-search Claude Code conversation history from the terminal";
    homepage = "https://github.com/raine/claude-history";
    license = lib.licenses.mit;
    mainProgram = "claude-history";
    platforms = builtins.attrNames sources;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
}
