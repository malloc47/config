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

    stylix = {
      url = "github:danth/stylix/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
    };

    personal = {
      url = "git+ssh://git@github.com/malloc47/personal";
      flake = false;
    };
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
      stylix,
      personal,
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
            self.nixosModules.settings
            self.nixosModules.user
            self.nixosModules.nixpkgs
            self.nixosModules.virtualization
            self.nixosModules.networking
            self.nixosModules.ssh
            self.nixosModules.sound
            self.nixosModules.gui
            self.nixosModules.configuration-flake
            self.nixosModules.stylix
            self.nixosModules.theme
            self.hardwareModules.vmware-fusion-arm
            self.diskModules.vmware-fusion
            ./hosts/salome.nix
            { settings.sshKeys = "${personal}/ssh"; }
            {
              networking.hostName = "salome";
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username} = {
                  imports = [
                    self.homeManagerModules.osconfig-bridge
                    self.homeManagerModules.home
                    self.homeManagerModules.home-dev
                    self.homeManagerModules.home-gui
                    self.homeManagerModules.home-vm
                  ];
                  # Emacs uses its own solarized-theme; don't let Stylix manage it
                  stylix.targets.emacs.enable = false;
                  # wm.nix / rofi.nix set these explicitly
                  stylix.targets.i3.enable = false;
                  stylix.targets.rofi.enable = false;
                };
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
            self.nixosModules.settings
            { settings.sshKeys = "${personal}/ssh"; }
            (
              { config, ... }:
              {
                # Enable guest tools so that we can extract the IP address from the guest
                virtualisation.vmware.guest.enable = true;
                users.users.root.openssh.authorizedKeys.keys = [
                  (builtins.readFile (config.settings.sshKeys + "/${config.settings.profile}/id_ed25519.pub"))
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
            self.nixosModules.settings
            self.nixosModules.user
            self.nixosModules.nixpkgs
            self.nixosModules.ssh
            self.nixosModules.configuration-flake
            self.nixosModules.stylix
            self.nixosModules.theme
            self.hardwareModules.gmktec-g10
            self.diskModules.gmktec-g10
            ./hosts/aida.nix
            { settings.sshKeys = "${personal}/ssh"; }
            {
              networking.hostName = "aida";
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [
                  self.homeManagerModules.osconfig-bridge
                  self.homeManagerModules.home
                ];
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
            self.nixosModules.settings
            self.nixosModules.user
            self.nixosModules.nixpkgs
            self.nixosModules.ssh
            self.nixosModules.configuration-flake
            self.nixosModules.stylix
            self.nixosModules.theme
            self.hardwareModules.racknerd-vps
            self.diskModules.racknerd-vps
            ./hosts/aroldo.nix
            { settings.sshKeys = "${personal}/ssh"; }
            {
              networking.hostName = "aroldo";
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username}.imports = [
                  self.homeManagerModules.osconfig-bridge
                  self.homeManagerModules.home
                ];
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
            self.nixosModules.settings
            self.nixosModules.user
            self.nixosModules.nixpkgs
            self.nixosModules.ssh
            self.nixosModules.configuration-flake
            self.nixosModules.stylix
            self.nixosModules.theme
            self.hardwareModules.dell-xps-9315
            self.diskModules.dell-xps-9315
            ./hosts/attila.nix
            { settings.sshKeys = "${personal}/ssh"; }
            {
              networking.hostName = "attila";
              system.stateVersion = "25.11";
            }
            (
              { config, ... }:
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username} = {
                  imports = [
                    self.homeManagerModules.osconfig-bridge
                    self.homeManagerModules.home
                    self.homeManagerModules.home-dev
                    self.homeManagerModules.home-ai
                  ];
                  # Emacs uses its own solarized-theme; don't let Stylix manage it
                  stylix.targets.emacs.enable = false;
                  programs.ai-session = {
                    enable = true;
                    webServer.enable = true;
                  };
                };
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
            self.nixosModules.settings
            self.nixosModules.user
            self.nixosModules.ssh
            { settings.sshKeys = "${personal}/ssh"; }
            agenix.darwinModules.default
            nix-homebrew.darwinModules.nix-homebrew
            self.darwinModules.homebrew
            self.darwinModules.configuration
            self.darwinModules.stylix
            self.darwinModules.theme
            ./hosts/cesare.nix
            home-manager.darwinModules.home-manager
            (
              { config, ... }:
              {
                networking.hostName = "cesare";

                # TODO: need these on linux hosts too?
                home-manager.extraSpecialArgs = specialArgs;
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${config.settings.username} = {
                  imports = [ self.darwinModules.home ];
                  # Emacs uses its own solarized-theme; don't let Stylix manage it
                  stylix.targets.emacs.enable = false;
                  # wm.nix / rofi.nix set these explicitly
                  stylix.targets.i3.enable = false;
                  stylix.targets.rofi.enable = false;
                };
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

      };

      nixosModules = {
        settings = ./modules/settings.nix;
        user = ./modules/user.nix;
        ssh = ./modules/ssh.nix;
        nixpkgs = ./modules/nixpkgs.nix;
        networking = ./modules/networking.nix;
        virtualization = ./modules/virtualization.nix;
        sound = ./modules/sound.nix;
        gui = ./modules/gui.nix;
        motd = ./modules/motd.nix;
        vmware-guest = ./modules/vmware-guest.nix;
        configuration-flake = ./nixos/configuration-flake.nix;
        stylix = stylix.nixosModules.stylix;
        theme = ./config/theme.nix;
      };

      homeManagerModules = {
        osconfig-bridge = ./config/home-osconfig-bridge.nix;
        home = ./config/home.nix;
        home-dev = ./config/home-dev.nix;
        home-gui = ./config/home-gui.nix;
        home-vm = ./config/home-vm.nix;
        ssh = ./config/home-ssh.nix;
        stylix = stylix.homeModules.stylix;
        theme = ./config/theme.nix;
        home-ai = import ./config/home-ai.nix { inherit inputs; };
      };

      darwinModules = {
        configuration = ./darwin/configuration.nix;
        homebrew = ./darwin/homebrew.nix;
        home = ./darwin/home.nix;
        stylix = stylix.darwinModules.stylix;
        theme = ./config/theme.nix;
      };

      overlays.default = import ./pkgs/default.nix;

      hardwareModules = {
        lxc = ./hardware/lxc.nix;
        vmware-fusion-arm = ./hardware/vmware-fusion-arm.nix;
        gmktec-g10 = ./hardware/gmktec-g10.nix;
        racknerd-vps = ./hardware/racknerd-vps.nix;
        dell-xps-9315 = ./hardware/dell-xps-9315.nix;
      };

      diskModules = {
        vmware-fusion = ./disk/vmware-fusion.nix;
        gmktec-g10 = ./disk/gmktec-g10.nix;
        racknerd-vps = ./disk/racknerd-vps.nix;
        dell-xps-9315 = ./disk/dell-xps-9315.nix;
      };

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
