(
  self: super: {
    geosanslight = super.callPackage geosanslight/default.nix {};
    inconsolata-unstable = super.callPackage inconsolata-unstable/default.nix {};
    aws-okta = super.callPackage aws-okta/default.nix {};
  }
)
