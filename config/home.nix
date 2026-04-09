{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../modules/settings.nix
    ./git.nix
    ./shell.nix
  ];

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs.nix;

  home.packages = with pkgs; [
    home-manager
    jq
    killall
    moreutils
    pv
    ripgrep
    tree
    unzip
    zip
  ];

  home.sessionPath = [ "$HOME/bin" ];

  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.file.".hushlogin".text = "";
  home.file.".inputrc".source = ./.inputrc;
  xdg.configFile.".user-dirs.dirs".source = ./.user-dirs.dirs;

  home.stateVersion = "25.05";
}
