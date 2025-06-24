{ config, pkgs, ... }:
{
  imports = [
    ../modules/settings.nix
    ../config/terminal.nix
    ../config/emacs.nix
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
    sessionPath = [
      "$HOME/bin"
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

  programs.vim = {
    enable = true;
    defaultEditor = true;
  };

  home.file."vmware-preferences" = {
    source = ../config/vmware-preferences;
    target = "Library/Preferences/VMware\ Fusion/preferences";
  };

  home.file."bin/vmrun".source =
    config.lib.file.mkOutOfStoreSymlink
    "/Applications/VMware\ Fusion.app/Contents/Library/vmrun" ;

  home.file."bin/vmcli".source =
    config.lib.file.mkOutOfStoreSymlink
    "/Applications/VMware\ Fusion.app/Contents/Library/vmcli" ;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
