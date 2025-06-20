{ pkgs, config, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
  [ 
    pkgs.vim
  ];
 
  nix.settings.experimental-features = "nix-command flakes";
 
  system.stateVersion = 6;
 
  nixpkgs.hostPlatform = "aarch64-darwin";
}
