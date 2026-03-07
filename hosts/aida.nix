{ config, pkgs, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  services.openssh.enable = true;
}
