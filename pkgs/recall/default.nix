{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

# Built from source so the conspectus session-deep-link patch lands.
# Upstream recall 0.5.0 has no flag to open the TUI scrolled to a
# specific session id; without that the binary is useless as a
# launch target from conspectus (operator picks a session, then has
# to pick it again from recall's search). The patch in
# `./session-flag.patch` adds `--session <ID>`. Conspectus
# feature-detects `recall --help` for the flag before enabling the
# multi-harness viewer backend, so a swap back to upstream (e.g.
# once a PR lands) is a one-line change here.

rustPlatform.buildRustPackage rec {
  pname = "recall";
  version = "0.5.0-conspectus-session";

  src = fetchFromGitHub {
    owner = "zippoxer";
    repo = "recall";
    rev = "80509066698c8797be6cfdbc5dd2f910759846bd";
    hash = "sha256-9hD5wInA391kOA8ShxFv1PiT2ZOc6p3TJaXY2kXZ80k=";
  };

  patches = [ ./session-flag.patch ];

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "crossterm-0.28.1" = "sha256-6qCtfSMuXACKFb9ATID39XyFDIEMFDmbx6SSmNe+728=";
    };
  };

  # The patched lockfile lives at ./Cargo.lock; recall ships a
  # matching lockfile in the source tree, so disable the auto-copy
  # to avoid the "Cargo.lock differs" error.
  postPatch = ''
    cp ${./Cargo.lock} Cargo.lock
  '';

  doCheck = false; # upstream test harness pulls fixtures we don't vendor

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    "$out/bin/recall" --version
    "$out/bin/recall" --help 2>&1 | grep -q -- '--session' \
      || { echo "patched recall missing --session flag" >&2; exit 1; }
    runHook postInstallCheck
  '';

  meta = {
    description = "Search and resume Claude Code, Codex, OpenCode, and Factory/Droid conversations (with conspectus session deep-link patch)";
    homepage = "https://github.com/zippoxer/recall";
    license = lib.licenses.mit;
    mainProgram = "recall";
    platforms = lib.platforms.unix;
  };
}
