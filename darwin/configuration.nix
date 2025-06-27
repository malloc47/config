{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    curl
    docker-compose
    exfat
    feh
    gitMinimal
    man-pages
    man-pages-posix
    vim
    watch
    wget
  ];

  nix.settings.experimental-features = "nix-command flakes";

  system.stateVersion = 6;

  nixpkgs.hostPlatform = "aarch64-darwin";
}
