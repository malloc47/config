{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../modules/settings.nix
    ./git.nix
    ./shell.nix
    ./modules/ghostty-terminfo.nix
  ];

  programs.ghostty-terminfo.enable = true;

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs.nix;

  home.packages = with pkgs; [
    home-manager
    jq
    killall
    moreutils
    mosh
    pv
    ripgrep
    tree
    unzip
    zip
  ];

  home.sessionPath = [ "$HOME/bin" ];

  home.sessionVariables = {
    EDITOR = lib.mkForce "emacsclient -t";
  };

  home.file.".hushlogin".text = "";
  home.file.".inputrc".source = ./.inputrc;
  xdg.configFile.".user-dirs.dirs".source = ./.user-dirs.dirs;

  services.ssh-agent.enable = true;

  home.stateVersion = "25.05";
}
