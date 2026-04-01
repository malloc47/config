{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # For VM
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # For MacOS
    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-homebrew.url = "github:zhaofengli/nix-homebrew";

    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    sendkeys-tap = {
      url = "github:socsieng/homebrew-tap";
      flake = false;
    };
    albert-tap = {
      url = "github:malloc47/homebrew-albert";
      flake = false;
    };

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    self.submodules = true;
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      disko,
      nix-darwin,
      nix-homebrew,
      nixos-hardware,
      agenix,
      git-hooks,
      ...
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      nixosConfigurations = {
        salome = nixpkgs.lib.nixosSystem rec {
          system = "aarch64-linux";
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
            };
          };
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
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [
                  config/home.nix
                  config/home-dev.nix
                  config/home-gui.nix
                  config/home-vm.nix
                ];
              }
            )
          ];
        };

        drw = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
            };
          };
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
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [
                  config/home.nix
                  config/home-dev.nix
                  config/home-gui.nix
                  config/home-vm.nix
                ];
              }
            )
          ];
        };

        drw-vmware = nixpkgs.lib.nixosSystem rec {
          system = "aarch64-linux";
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
            };
          };
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
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [
                  config/home.nix
                  config/home-dev.nix
                  config/home-gui.nix
                  config/home-vm.nix
                ];
              }
            )
          ];
        };

        # nix build .#nixosConfigurations.vm-iso.config.system.build.isoImage
        vm-iso = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (
              { pkgs, modulesPath, ... }:
              {
                imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix") ];
              }
            )
            modules/settings.nix
            (
              { config, ... }:
              {
                # Enable guest tools so that we can extract the IP address from the guest
                virtualisation.vmware.guest.enable = true;
                users.users.root.openssh.authorizedKeys.keys = [
                  (builtins.readFile personal/ssh/${config.settings.profile}/id_ed25519.pub)
                ];
              }
            )
          ];
        };

        aida = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
            };
          };
          modules = [
            home-manager.nixosModules.home-manager
            disko.nixosModules.disko
            agenix.nixosModules.default
            hardware/gmktec-g10.nix
            modules/settings.nix
            modules/user.nix
            modules/nixpkgs.nix
            modules/ssh.nix
            hosts/aida.nix
            nixos/configuration-flake.nix
            disk/gmktec-g10.nix
            {
              networking.hostName = "aida";
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [ config/home.nix ];
              }
            )
          ];
        };

        aroldo = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
            };
          };
          modules = [
            home-manager.nixosModules.home-manager
            disko.nixosModules.disko
            agenix.nixosModules.default
            hardware/racknerd-vps.nix
            modules/settings.nix
            modules/user.nix
            modules/nixpkgs.nix
            modules/ssh.nix
            hosts/aroldo.nix
            nixos/configuration-flake.nix
            disk/racknerd-vps.nix
            {
              networking.hostName = "aroldo";
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [ config/home.nix ];
              }
            )
          ];
        };

        attila = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
            };
          };
          modules = [
            home-manager.nixosModules.home-manager
            disko.nixosModules.disko
            agenix.nixosModules.default
            nixos-hardware.nixosModules.dell-xps-13-9315
            hardware/dell-xps-9315.nix
            modules/settings.nix
            modules/user.nix
            modules/nixpkgs.nix
            modules/ssh.nix
            hosts/attila.nix
            nixos/configuration-flake.nix
            disk/dell-xps-9315.nix
            {
              networking.hostName = "attila";
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [
                  config/home.nix
                  config/home-dev.nix
                ];
              }
            )
          ];
        };
      };

      darwinConfigurations = {
        cesare = nix-darwin.lib.darwinSystem rec {
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              system = "aarch64-darwin";
            };
          };
          modules = [
            modules/settings.nix
            modules/user.nix
            modules/ssh.nix
            agenix.darwinModules.default
            nix-homebrew.darwinModules.nix-homebrew
            darwin/homebrew.nix
            darwin/configuration.nix
            hosts/cesare.nix
            home-manager.darwinModules.home-manager
            (
              { config, ... }:
              {
                networking.hostName = "cesare";

                # TODO: need these on linux hosts too?
                home-manager.extraSpecialArgs = specialArgs;
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username} = import ./darwin/home.nix;
                # Doing this to handle existing vmware files
                home-manager.backupFileExtension = "backup";

                # Set Git commit hash for darwin-version.
                system.configurationRevision = self.rev or self.dirtyRev or null;
                system.primaryUser = config.settings.username;

                nixpkgs.hostPlatform = "aarch64-darwin";

                environment.systemPackages = [ agenix.packages.aarch64-darwin.default ];

                homebrew.casks = [
                  "claude"
                  "tailscale"
                ];

                system.stateVersion = 6;
              }
            )
          ];
        };

        nylmd-jwaggon1 = nix-darwin.lib.darwinSystem rec {
          specialArgs = {
            inherit inputs;
            pkgs-unstable = import nixpkgs-unstable {
              system = "aarch64-darwin";
            };
          };
          modules = [
            modules/settings.nix
            modules/user.nix
            modules/ssh.nix
            nix-homebrew.darwinModules.nix-homebrew
            darwin/homebrew.nix
            darwin/configuration.nix
            hosts/nylmd-jwaggon1.nix
            home-manager.darwinModules.home-manager
            (
              { config, ... }:
              {
                home-manager.extraSpecialArgs = specialArgs;
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username} = import ./darwin/home.nix;
                # Doing this to handle existing vmware files
                home-manager.backupFileExtension = "backup";

                # Set Git commit hash for darwin-version.
                system.configurationRevision = self.rev or self.dirtyRev or null;
                system.primaryUser = config.settings.username;

                system.defaults.universalaccess.reduceMotion = true;

                nixpkgs.hostPlatform = "aarch64-darwin";

                system.stateVersion = 6;
              }
            )
          ];
        };
      };

      packages.aarch64-linux.term-do =
        nixpkgs.legacyPackages.aarch64-linux.callPackage ../pkgs/term-do/default.nix
          { };

      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-tree);

      checks = forAllSystems (system: {
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt-rfc-style.enable = true;
          };
        };
      });

      devShells = forAllSystems (system: {
        default = nixpkgs.legacyPackages.${system}.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          buildInputs = self.checks.${system}.pre-commit-check.enabledPackages;
        };
      });
    };
}
