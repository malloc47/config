{
  lib,
  rustPlatform,
  fetchFromGitHub,
  cmake,
  perl,
  pkg-config,
  libcap,
  libiconv,
  stdenv,
  runCommand,
}:

let
  zeroboxSrc = fetchFromGitHub {
    owner = "afshinm";
    repo = "zerobox";
    rev = "e7f83f613cdf986a1661dbbab93bc3020cacc66d";
    hash = "sha256-1FCRyJMYD+0ME+JqSnkO0vjR/lxTOL2Jbru00KuECco=";
  };

  # Zerobox vendors crates from openai/codex via scripts/sync.sh.
  # We fetch the pinned codex release and replicate the sync + patch steps.
  codexSrc = fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "rust-v0.118.0";
    hash = "sha256-FdtV+CIqTInnegcXrXBxw4aE0JnNDh4GdYKwUDjSk9Y=";
  };

  # Produce a complete source tree with upstream/ populated and patches applied.
  # This must happen before cargo vendor, since Cargo.toml references upstream/ workspace members.
  fullSrc = runCommand "zerobox-src" { } (
    let
      codexRs = "${codexSrc}/codex-rs";
      crates = [
        "sandboxing"
        "linux-sandbox"
        "windows-sandbox-rs"
        "process-hardening"
        "protocol"
        "execpolicy"
        "network-proxy"
      ];
      utils = [
        "absolute-path"
        "string"
        "pty"
        "image"
        "cache"
        "template"
        "home-dir"
        "rustls-provider"
      ];
      copyCrates = lib.concatMapStringsSep "\n" (c: "cp -r ${codexRs}/${c} $out/upstream/${c}") crates;
      copyUtils = lib.concatMapStringsSep "\n" (
        u: "cp -r ${codexRs}/utils/${u} $out/upstream/utils/${u}"
      ) utils;
    in
    ''
      cp -r ${zeroboxSrc} $out
      chmod -R u+w $out

      mkdir -p $out/upstream/utils
      ${copyCrates}
      ${copyUtils}
      cp -r ${codexRs}/vendor $out/upstream/vendor
      chmod -R u+w $out/upstream

      # Patch windows-sandbox-rs path dep -> workspace
      sed -i 's|\[dependencies\.codex-protocol\]|codex-protocol = { workspace = true }|' \
        $out/upstream/windows-sandbox-rs/Cargo.toml
      sed -i '/^package = "codex-protocol"/d' $out/upstream/windows-sandbox-rs/Cargo.toml
      sed -i '/^path = "\.\.\/protocol"/d' $out/upstream/windows-sandbox-rs/Cargo.toml

      # Remove /run/current-system/sw from platform defaults.
      # On NixOS, bwrap fails with "Can't mkdir /run/current-system/sw"
      # because /run is already ro-bound (added by the network policy for
      # DNS resolution) and bwrap can't create subdirectories inside a
      # read-only bind mount.  The /run binding already covers this path.
      sed -i '/"\/run\/current-system\/sw"/d' \
        $out/upstream/linux-sandbox/src/bwrap.rs

      # Apply zerobox patches
      cd $out
      for p in scripts/upstream-secret-substitution.patch \
               scripts/upstream-platform-defaults.patch \
               scripts/upstream-deny-default-write.patch \
               scripts/upstream-no-preemptive-codex-protect.patch; do
        if [ -f "$p" ]; then
          patch -p1 < "$p" || patch -p0 < "$p" || true
        fi
      done
    ''
  );
in
rustPlatform.buildRustPackage {
  pname = "zerobox";
  version = "0.1.11";

  src = fullSrc;

  cargoHash = "sha256-tX6TByWvDnG0BmkXGBLTZIhHkhj9t8NbvTasYkzJF+Y=";

  nativeBuildInputs = [
    cmake
    perl
  ]
  ++ lib.optionals stdenv.hostPlatform.isLinux [
    pkg-config
  ];

  buildInputs =
    lib.optionals stdenv.hostPlatform.isDarwin [
      libiconv
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      libcap
    ];

  buildAndTestSubdir = "crates/zerobox";

  # Tests require sandbox capabilities not available in the Nix build sandbox
  doCheck = false;

  meta = {
    description = "Sandbox any command with file, network, and credential controls";
    homepage = "https://github.com/afshinm/zerobox";
    license = lib.licenses.asl20;
    mainProgram = "zerobox";
  };
}
