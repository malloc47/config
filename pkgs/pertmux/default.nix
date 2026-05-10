{
  lib,
  fetchCrate,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "pertmux";
  version = "0.3.3";

  src = fetchCrate {
    inherit pname version;
    hash = "sha256-HBiHvdrtCK5Mz0dYf9HPuOnq4eRi4PiqCw7OLBFJprQ=";
  };

  cargoHash = "sha256-FBZG4ZZ7soK4xGJc0F+H7tRinkfB7xLw7t1hTTwuKB8=";

  # Upstream tests assume host CA/git runtime state that is unavailable in the
  # Nix build sandbox.
  doCheck = false;

  meta = {
    description = "TUI dashboard linking GitLab/GitHub MRs to local branches, tmux sessions, and AI coding agents";
    homepage = "https://pertmux.dev";
    license = lib.licenses.mit;
    mainProgram = "pertmux";
  };
}
