(
  self: super: {
    geosanslight = super.callPackage geosanslight/default.nix {};
    aws-okta = super.callPackage aws-okta/default.nix {};
  }
)
