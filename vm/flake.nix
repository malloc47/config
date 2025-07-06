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

      # nix build .#nixosConfigurations.vm-iso.config.system.build.isoImage
      vm-iso = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          ({ pkgs, modulesPath, ... }: {
            imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix") ];
          })
          ../modules/settings.nix
          ({config, ...}: {
            # Enable  guest tools so that we can extract the IP address from the guest
            virtualisation.vmware.guest.enable = true;
            users.users.root.openssh.authorizedKeys.keys =
              [ (builtins.readFile ../personal/ssh/${config.settings.profile}/id_rsa.pub) ];
          })
        ];
      };
    };

    packages.aarch64-linux.term-do = nixpkgs.legacyPackages.aarch64-linux.callPackage ../pkgs/term-do/default.nix {};

  };
}
