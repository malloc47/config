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

  outputs = { nixpkgs, disko, home-manager, ...  }: {
    nixosConfigurations = nixpkgs.lib.genAttrs ["salome"] (hostname: nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        home-manager.nixosModules.home-manager
        disko.nixosModules.disko
        ../modules/settings.nix
        ../modules/user.nix
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
  };
}
