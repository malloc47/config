{
  description = "nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    self.submodules = true;
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }: {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#cesare
    darwinConfigurations = nixpkgs.lib.genAttrs ["cesare"] (hostname: nix-darwin.lib.darwinSystem {
      modules = [ 
        { 
          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null; 
        }
        ../modules/settings.nix
        ../modules/user.nix
        ./configuration.nix
        ./darwin.nix
        home-manager.darwinModules.home-manager
        {networking.hostName = hostname;}
        ({ config, ... }:
         {
           home-manager.useGlobalPkgs = true;
           home-manager.useUserPackages = true;
           home-manager.users.${config.settings.username} = import ./home.nix;
           system.primaryUser = config.settings.username;
         })
      ];
    });
  };
}
