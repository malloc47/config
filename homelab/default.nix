# Homelab / home-network hosts. Nothing outside this directory imports it
# except flake.nix, and nothing in it reaches the shared base by relative
# path: it consumes the base only through its flake outputs (`base.*`), the
# same way work-config does. That keeps the base free of homelab concerns and
# lets this directory become its own flake that takes `config` as an input.
{ inputs, base }:
let
  inherit (inputs)
    nixpkgs
    nixpkgs-unstable
    home-manager
    disko
    agenix
    personal
    ;
  pkgsUnstableFor =
    system:
    import nixpkgs-unstable {
      inherit system;
      config = {
        allowUnfree = true;
        allowUnsupportedSystem = true;
      };
      overlays = [ ];
    };
  overlay = import ./pkgs/default.nix;
in
{
  inherit overlay;

  nixosConfigurations = {
    aida = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
        pkgs-unstable = pkgsUnstableFor system;
      };
      modules = [
        home-manager.nixosModules.home-manager
        disko.nixosModules.disko
        agenix.nixosModules.default
        base.nixosModules.settings
        base.nixosModules.user
        base.nixosModules.nixpkgs
        base.nixosModules.ssh
        base.nixosModules.mosh
        base.nixosModules.motd
        base.nixosModules.configuration-flake
        base.nixosModules.stylix
        base.nixosModules.theme
        ./hardware/gmktec-g10.nix
        ./disk/gmktec-g10.nix
        ./hosts/aida.nix
        { nixpkgs.overlays = [ overlay ]; }
        { settings.sshKeys = "${personal}/ssh"; }
        {
          networking.hostName = "aida";
          system.stateVersion = "26.05";
        }
        (
          { config, ... }:
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = specialArgs;
            home-manager.users.${config.settings.username} = {
              imports = [
                base.homeManagerModules.osconfig-bridge
                base.homeManagerModules.home
                base.homeManagerModules.home-agenix
                base.homeManagerModules.home-sqlit
              ];
              programs.sqlit.enable = true;
            };
          }
        )
      ];
    };

    aroldo = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
        pkgs-unstable = pkgsUnstableFor system;
      };
      modules = [
        home-manager.nixosModules.home-manager
        disko.nixosModules.disko
        agenix.nixosModules.default
        base.nixosModules.settings
        base.nixosModules.user
        base.nixosModules.nixpkgs
        base.nixosModules.ssh
        base.nixosModules.mosh
        base.nixosModules.motd
        base.nixosModules.configuration-flake
        base.nixosModules.stylix
        base.nixosModules.theme
        ./hardware/racknerd-vps.nix
        ./disk/racknerd-vps.nix
        ./hosts/aroldo.nix
        { nixpkgs.overlays = [ overlay ]; }
        { settings.sshKeys = "${personal}/ssh"; }
        {
          networking.hostName = "aroldo";
          system.stateVersion = "26.05";
        }
        (
          { config, ... }:
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = specialArgs;
            home-manager.users.${config.settings.username}.imports = [
              base.homeManagerModules.osconfig-bridge
              base.homeManagerModules.home
              base.homeManagerModules.home-agenix
            ];
          }
        )
      ];
    };
  };
}
