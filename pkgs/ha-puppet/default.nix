# Puppet (balloob/home-assistant-addons) — screenshots Home Assistant pages for
# e-ink panels: GET /<dashboard>/<view>?viewport=800x480&colors=000000,FFFFFF
# returns a PNG of that page rendered by headless Chromium and reduced to the
# given palette. Upstream only ships it as an HA OS add-on (built on the host,
# no published image), but it is a plain Node app, so it is packaged directly.
#
# Patched so it runs as a NixOS service:
#   - reads its options JSON from $PUPPET_OPTIONS_FILE (upstream only looks for
#     ./options-dev.json or the add-on's /data/options.json), so the access
#     token can be generated at runtime from an agenix secret;
#   - listens on $PUPPET_LISTEN_HOST (default 127.0.0.1) instead of all
#     interfaces, since anyone who can reach it can screenshot whatever the
#     token's user can see;
#   - defaults the Chromium path to nixpkgs' chromium.
{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  autoPatchelfHook,
  makeWrapper,
  stdenv,
  nodejs,
  chromium,
}:

buildNpmPackage {
  pname = "ha-puppet";
  version = "2.6.0-unstable-2026-08-10";

  src = fetchFromGitHub {
    owner = "balloob";
    repo = "home-assistant-addons";
    rev = "8cafe6e2fe1f240918001b7ab58e8263b477ed04";
    hash = "sha256-HsuIl+zS+EaDj40D8jhTg/DRyAGhN/wrFMedv5S3HtQ=";
  };
  sourceRoot = "source/puppet/ha-puppet";

  npmDepsHash = "sha256-rg5Tm01NTrBbqwxoOGkETI+TLVH1rlGS+yZkbHYSOo8=";

  # Puppeteer would otherwise download its own Chrome; nixpkgs' chromium is used.
  env.PUPPETEER_SKIP_DOWNLOAD = "1";
  dontNpmBuild = true;

  # sharp's prebuilt @img/sharp-linux-x64 addon and bundled libvips.
  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];
  buildInputs = [ stdenv.cc.cc.lib ];

  postPatch = ''
    substituteInPlace const.js \
      --replace-fail '["./options-dev.json", "/data/options.json"].find(' \
                     '[process.env.PUPPET_OPTIONS_FILE, "./options-dev.json", "/data/options.json"].filter(Boolean).find(' \
      --replace-fail '"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"' '"${lib.getExe chromium}"'
    substituteInPlace http.js \
      --replace-fail 'server.listen(port);' 'server.listen(port, process.env.PUPPET_LISTEN_HOST || "127.0.0.1");'
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/ha-puppet $out/bin
    cp -r . $out/lib/ha-puppet
    rm -f $out/lib/ha-puppet/options-dev.json.sample
    # npm installs sharp's musl prebuilds too; only the glibc ones load here.
    rm -rf $out/lib/ha-puppet/node_modules/@img/*musl*
    makeWrapper ${lib.getExe nodejs} $out/bin/ha-puppet \
      --add-flags $out/lib/ha-puppet/http.js \
      --chdir $out/lib/ha-puppet
    runHook postInstall
  '';

  meta = {
    description = "Screenshot Home Assistant dashboards for e-ink displays (Puppet add-on, packaged standalone)";
    homepage = "https://github.com/balloob/home-assistant-addons/tree/main/puppet";
    license = lib.licenses.asl20;
    platforms = lib.platforms.linux;
    mainProgram = "ha-puppet";
  };
}
