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

  outputs = 
    {
      nixpkgs,
      disko,
      home-manager,
      ...
    }:
    {
      nixosConfigurations.salome = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          ../modules/settings.nix
          home-manager.nixosModules.home-manager
          disko.nixosModules.disko
          ./configuration.nix
          ./hardware-configuration.nix
          ({ config, ... }:
          {
            settings = {
              vm = true;
              username = "malloc47";
              fontSize = 9.0;
              xkbFile = "vm";
              terminal = "kitty";
            };

            users.users.${config.settings.username} = {
              isNormalUser = true;
              createHome = true;
              home = "/home/${config.settings.username}";
              description = "Jarrell Waggoner";
              extraGroups = ["audio" "docker" "networkmanager" "wheel" "lxd"];
              uid = 1000;
            };
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${config.settings.username} = ./home.nix;
          })
        ];
      };
    };
}
