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
    nixosConfigurations = {
      salome = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          home-manager.nixosModules.home-manager
          disko.nixosModules.disko
          ../modules/settings.nix
          ../modules/user.nix
          ../modules/nixpkgs.nix
          ../modules/virtualization.nix
          ../modules/networking.nix
          ../modules/ssh.nix
          ../modules/sound.nix
          ../modules/gui.nix
          ./configuration.nix
          ./hardware-configuration.nix
          {networking.hostName = "salome";}
          ({ config, ... }:
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${config.settings.username}.imports = [ ../config/home.nix ];
            })
        ];
      };

      vm-iso = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          ({ pkgs, modulesPath, ... }: {
            imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix") ];
            environment.systemPackages = [ pkgs.neovim ];
          })
          {virtualisation.vmware.guest.enable = true;}
        ];
      };
    };

    packages.aarch64-linux.term-do = nixpkgs.legacyPackages.aarch64-linux.callPackage ../pkgs/term-do/default.nix {};

  };
}
