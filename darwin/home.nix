{ config, pkgs, ... }:
{
  imports = [
    ../modules/settings.nix
  ];

  home = {
    stateVersion = "23.05";
    packages = with pkgs; [
      clojure
      nixos-anywhere
      autoraise
      pkgs.nixfmt-rfc-style
      rsync
    ];
  };

  programs.git = {
    enable = true;
    userName = config.settings.name;
    userEmail = config.settings.email;
    aliases = {
      s = "status -s -uno";
      gl = "log --oneline --graph";
    };
    ignores = [".#*" "*.desktop" "*.lock"];
    lfs.enable = true;
    extraConfig = {
      branch.autosetuprebase = "never";
      push.default = "simple";
      core.pager = "less -F -X";
      pull.ff = "only";
      init.defaultBranch = "main";
    };
  };

  programs.zsh.enable = true;

  programs.vim = {
    enable = true;
    defaultEditor = true;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}

