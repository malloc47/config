{
  description = "nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }: {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#cesare
    darwinConfigurations."cesare" = nix-darwin.lib.darwinSystem {
      modules = [ 
        { 
          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null; 
        }
        ./configuration.nix
      ];
    };
  };
}
