{
  description = "nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    #nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    #home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }: let
    username = "malloc47";
    useremail = "malloc47@gmail.com";
    hostname = "cesare";

    specialArgs =
      inputs
      // {
        inherit username useremail hostname;
      };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#cesare
    darwinConfigurations."${hostname}" = nix-darwin.lib.darwinSystem {
      inherit specialArgs;
      modules = [ 
        { 
          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null; 
        }
        ./configuration.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${username} = import ./home.nix;
        }
      ];
    };
  };
}
