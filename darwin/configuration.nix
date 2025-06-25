{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    curl
    gitMinimal
    docker-compose
    exfat
    feh
    man-pages
    man-pages-posix
    vim
    wget
  ];

  nix.settings.experimental-features = "nix-command flakes";

  system.stateVersion = 6;

  nixpkgs.hostPlatform = "aarch64-darwin";
}
