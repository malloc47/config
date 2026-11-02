{
  lib,
  fetchFromGitHub,
  rustPlatform,
  coreutils,
  python3,
}:

rustPlatform.buildRustPackage rec {
  pname = "tramp-rpc-server";
  # The Emacs client requires an exact protocol-revision match, so keep this in
  # step with the tramp-rpc Elisp version. Clients should use this binary via
  # tramp-rpc-deploy-never-deploy + tramp-rpc-deploy-remote-binary-path rather
  # than auto-deploying their own.
  version = "0.16.0";

  src = fetchFromGitHub {
    owner = "ArthurHeymans";
    repo = "emacs-tramp-rpc";
    rev = "v${version}";
    hash = "sha256-kmb0tolO3E/cXqvdzTUQFKPGgDXSO0uN+98RXvmMvBk=";
  };

  cargoHash = "sha256-o+JyVmN8KzIiokr9UKpxwP+cSUf72RtH+/m0Rfck78g=";

  # Upstream's own derivation (default.nix) uses these for the server tests.
  nativeCheckInputs = [
    coreutils
    python3
  ];

  meta = {
    description = "Remote-side RPC server for the tramp-rpc Emacs TRAMP backend";
    homepage = "https://github.com/ArthurHeymans/emacs-tramp-rpc";
    license = lib.licenses.gpl3Plus;
    mainProgram = "tramp-rpc-server";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
