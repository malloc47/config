{ pkgs, ... }: 

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
  [ 
    pkgs.vim
  ];
 
  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";
 
  # Enable alternative shell support in nix-darwin.
  # programs.fish.enable = true;
 
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
 
  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}

