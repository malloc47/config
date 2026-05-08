# darwin/modules

Reserved for nix-darwin-only modules — files that set `system.defaults.*`,
`launchd.*`, `homebrew.*`, or otherwise depend on options only present in a
darwin evaluation. Currently empty; cross-target modules live in
`/modules/`, NixOS-only modules in `/nixos/modules/`, home-manager modules
in `/home/modules/`.
