{
  lib,
  fetchCrate,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "workmux";
  version = "0.1.204";

  src = fetchCrate {
    inherit pname version;
    hash = "sha256-YYhnQvkZl8+oSxTQjfryATlh6YUUr/vUvtPeg2xwJO0=";
  };

  cargoHash = "sha256-eFgoPaa7FZDIEVde55D4zorsqmBUlpQ9o4RuagY0mVQ=";

  # Upstream tests exercise git/tmux workflow state that is not reliable in the
  # Nix build sandbox.
  doCheck = false;

  meta = {
    description = "Opinionated workflow tool that orchestrates git worktrees and tmux";
    homepage = "https://github.com/raine/workmux";
    license = lib.licenses.mit;
    mainProgram = "workmux";
  };
}
