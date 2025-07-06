{ lib, pkgs, config, ... }: {
  imports = [
    ../modules/settings.nix
  ];

  time.timeZone = "America/New_York";

  virtualisation.vmware.guest.enable = lib.mkIf (config.settings.vm) true;

  nix.settings.experimental-features = "nix-command flakes";
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; map lib.lowPrio [
    curl
    gitMinimal
    docker-compose
    exfat
    feh
    man-pages
    man-pages-posix
    vim
    wget
    xorg.xkill
  ];

  documentation.enable = true;
  documentation.man.enable = true;
  documentation.dev.enable = true;

  environment.wordlist.enable = true;

  fonts.packages = with pkgs; [
    corefonts
    geosanslight
    inconsolata-unstable
    libertine
    libre-baskerville
    emacs-all-the-icons-fonts
  ];

  programs.zsh.enable = true;

  services.journald.extraConfig = ''
      SystemMaxUse=2G
  '';

  users.users.root.openssh.authorizedKeys.keys =
    [ (builtins.readFile ../personal/ssh/${config.settings.profile}/id_rsa.pub) ];

}
