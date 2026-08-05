{
  lib,
  fetchCrate,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "worktrunk";
  version = "0.71.0";

  src = fetchCrate {
    inherit pname version;
    hash = "sha256-U+8ZpBEfnJSS25Zs8mgsXefZrGQZb4KGvY0v24OQLhw=";
  };

  cargoHash = "sha256-NeP2y4B0R775wQO1nJqx93bOuHTZZsHCxHrjNEQ5qcA=";

  buildFeatures = [ "git-wt" ];

  # Upstream tests exercise git/worktree recovery state that is not reliable in
  # the Nix build sandbox.
  doCheck = false;

  meta = {
    description = "CLI for Git worktree management, designed for parallel AI agent workflows";
    homepage = "https://worktrunk.dev";
    license = with lib.licenses; [
      mit
      asl20
    ];
    mainProgram = "wt";
  };
}
