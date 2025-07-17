{
  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/25.05";

      nix-darwin = {
        url = "github:LnL7/nix-darwin/nix-darwin-25.05";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      disko = {
        url = "github:nix-community/disko";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      home-manager = {
        url = "github:nix-community/home-manager/release-25.05";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      self.submodules = true;
    };

  outputs = inputs@{ self, nixpkgs, nix-darwin, disko, home-manager, ...  }: {
    nixosConfigurations = {
      salome = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          home-manager.nixosModules.home-manager
          disko.nixosModules.disko
          modules/settings.nix
          modules/user.nix
          modules/nixpkgs.nix
          modules/virtualization.nix
          modules/networking.nix
          modules/ssh.nix
          modules/sound.nix
          modules/gui.nix
          hosts/salome.nix
          nixos/configuration-flake.nix
          hardware/vmware-fusion-arm.nix
          disk/vmware-fusion.nix
          {
            networking.hostName = "salome";
            system.stateVersion = "25.05";
          }
          ({ config, ... }:
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${config.settings.username}.imports = [ config/home.nix ];
            })
        ];
      };

      drw = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          modules/settings.nix
          modules/user.nix
          modules/nixpkgs.nix
          modules/virtualization.nix
          modules/networking.nix
          modules/ssh.nix
          modules/sound.nix
          modules/gui.nix
          hosts/drw-lxc.nix
          nixos/configuration-flake.nix
          hardware/lxc.nix
          {
            networking.hostName = "drw";
            system.stateVersion = "25.05";
          }
          ({ config, ... }:
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${config.settings.username}.imports = [ config/home.nix ];
            })
        ];
      };

      drw-vmware = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          home-manager.nixosModules.home-manager
          disko.nixosModules.disko
          modules/settings.nix
          modules/user.nix
          modules/nixpkgs.nix
          modules/virtualization.nix
          modules/networking.nix
          modules/ssh.nix
          modules/sound.nix
          modules/gui.nix
          hosts/drw-vmware.nix
          nixos/configuration-flake.nix
          hardware/vmware-fusion-arm.nix
          disk/vmware-fusion.nix
          {
            networking.hostName = "drw-vmware";
            system.stateVersion = "25.05";
          }
          ({ config, ... }:
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${config.settings.username}.imports = [ config/home.nix ];
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
          modules/settings.nix
          ({config, ...}: {
            # Enable guest tools so that we can extract the IP address from the guest
            virtualisation.vmware.guest.enable = true;
            users.users.root.openssh.authorizedKeys.keys =
              [ (builtins.readFile personal/ssh/${config.settings.profile}/id_rsa.pub) ];
          })
        ];
      };
    };

    darwinConfigurations = {
      cesare = nix-darwin.lib.darwinSystem {
        modules = [
          modules/settings.nix
          modules/user.nix
          modules/ssh.nix
          darwin/configuration.nix
          hosts/cesare.nix
          home-manager.darwinModules.home-manager
          ({ config, ... }: {
            networking.hostName = "cesare";

            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${config.settings.username} = import ./darwin/home.nix;
            # Doing this to handle existing vmware files
            home-manager.backupFileExtension = "backup";

            # Set Git commit hash for darwin-version.
            system.configurationRevision = self.rev or self.dirtyRev or null;
            system.primaryUser = config.settings.username;

            nixpkgs.hostPlatform = "aarch64-darwin";

            system.stateVersion = 6;
          })
        ];
      };

      nylmd-jwaggon1 = nix-darwin.lib.darwinSystem {
        modules = [
          modules/settings.nix
          modules/user.nix
          modules/ssh.nix
          darwin/configuration.nix
          hosts/nylmd-jwaggon1.nix
          home-manager.darwinModules.home-manager
          ({ config, ... }: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${config.settings.username} = import ./darwin/home.nix;
            # Doing this to handle existing vmware files
            home-manager.backupFileExtension = "backup";

            # Set Git commit hash for darwin-version.
            system.configurationRevision = self.rev or self.dirtyRev or null;
            system.primaryUser = config.settings.username;

            nixpkgs.hostPlatform = "aarch64-darwin";

            system.stateVersion = 6;
          })
        ];
      };
    };

    packages.aarch64-linux.term-do = nixpkgs.legacyPackages.aarch64-linux.callPackage ../pkgs/term-do/default.nix {};

  };
}
