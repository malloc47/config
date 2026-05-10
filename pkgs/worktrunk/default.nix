{
  lib,
  fetchCrate,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "worktrunk";
  version = "0.36.0";

  src = fetchCrate {
    inherit pname version;
    hash = "sha256-bMWhAamq2Xya1KA+httyGuTNZj19GRR+CvRRRlozhpY=";
  };

  cargoHash = "sha256-SQSbAHzXlGKHW/Q5YO3+IE5u9o84L9jsYm0u8z30i2s=";

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
