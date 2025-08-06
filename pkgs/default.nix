(
  final: prev: {
    albert = prev.callPackage albert/default.nix {};
    autoraise = prev.callPackage autoraise/default.nix {};
    geosanslight = prev.callPackage geosanslight/default.nix {};
    inconsolata-unstable = prev.callPackage inconsolata-unstable/default.nix {};
    aws-okta = prev.callPackage aws-okta/default.nix {};
    term-do = prev.callPackage term-do/default.nix {};
    carve = prev.callPackage carve/default.nix {};
    swipe-aerospace = prev.callPackage swipe-aerospace/default.nix {};
  }
)
