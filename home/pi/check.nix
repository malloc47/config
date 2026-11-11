# Type-checks the pi package against the pi it runs on, and runs its unit tests. pi's extension
# API can change in any release, so this is what a bump of llm-agents gets checked against. pi
# installs as a compiled binary without its type definitions, so this installs the npm package it
# was built from, from the same tarball and lockfile.
{ pkgs, pi }:
let
  piTypes = pkgs.stdenvNoCC.mkDerivation {
    pname = "pi-types";
    inherit (pi) version src npmDeps;
    nativeBuildInputs = [
      pkgs.nodejs_24
      pkgs.npmHooks.npmConfigHook
    ];
    makeCacheWritable = true;
    npmFlags = [ "--ignore-scripts" ];
    dontBuild = true;
    installPhase = ''
      mkdir -p "$out/node_modules/@earendil-works/pi-coding-agent"
      cp -R node_modules/. "$out/node_modules/"
      cp -R package.json dist "$out/node_modules/@earendil-works/pi-coding-agent/"
    '';
    dontFixup = true;
  };
  tsconfig = pkgs.writeText "tsconfig.json" (
    builtins.toJSON {
      compilerOptions = {
        target = "es2024";
        lib = [ "es2024" ];
        module = "nodenext";
        moduleResolution = "nodenext";
        strict = true;
        noEmit = true;
        allowImportingTsExtensions = true;
        erasableSyntaxOnly = true;
        verbatimModuleSyntax = true;
        skipLibCheck = true;
        types = [ "node" ];
        typeRoots = [ "${piTypes}/node_modules/@types" ];
        paths = {
          "@earendil-works/pi-coding-agent" = [
            "${piTypes}/node_modules/@earendil-works/pi-coding-agent/dist/index.d.ts"
          ];
          "@earendil-works/pi-tui" = [ "${piTypes}/node_modules/@earendil-works/pi-tui/dist/index.d.ts" ];
        };
      };
      include = [ "." ];
    }
  );
in
pkgs.runCommand "pi-personal-check"
  {
    nativeBuildInputs = [
      pkgs.nodejs_24
      pkgs.typescript
    ];
  }
  ''
    cp -R ${./.} pkg
    chmod -R u+w pkg
    cd pkg
    cp ${tsconfig} tsconfig.json
    tsc -p .
    node --test lib/*.test.ts
    touch "$out"
  ''
