{
  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/25.05";
      disko.url = "github:nix-community/disko";
      disko.inputs.nixpkgs.follows = "nixpkgs";
      home-manager.url = "github:nix-community/home-manager/release-25.05";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";
      self.submodules = true;
    };

  outputs = inputs@{ nixpkgs, disko, home-manager, ...  }: {
    nixosConfigurations = nixpkgs.lib.genAttrs ["salome"] (hostname: nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        home-manager.nixosModules.home-manager
        disko.nixosModules.disko
        ../modules/settings.nix
        ../modules/user.nix
        { 
          nix.nixPath = [
            "nixpkgs=${inputs.nixpkgs}" 
            #"nixos-config=/etc/nixos/flake.nix"
            "nixpkgs-overlays=/etc/nixos/overlays-compat/"
          ]; 
          nix.registry.nixpkgs.flake = inputs.nixpkgs;
          nix.registry.self.flake = inputs.self;
        }
        ../modules/nixpkgs.nix
        ./configuration.nix
        ./hardware-configuration.nix
        {networking.hostName = hostname;}
        ({ config, ... }:
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${config.settings.username} = ../config/home.nix; # ./home;
        })
      ];
    });

    packages.aarch64-linux.term-do = nixpkgs.legacyPackages.aarch64-linux.callPackage ../pkgs/term-do/default.nix {};

  };
}
