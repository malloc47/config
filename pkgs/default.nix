(
  self: super: {
    geosanslight = super.callPackage geosanslight/default.nix {};
    leiningen = super.callPackage leiningen/default.nix {};
  }
)
