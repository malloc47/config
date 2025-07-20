{ config, osConfig, pkgs, ... }:
{
  imports = [
    ../modules/settings.nix
    ../config/shell.nix
    ../config/emacs.nix
    ../config/git.nix
    ./clipcat.nix
  ];

  settings = osConfig.settings;

  home = {
    packages = with pkgs; [
      autoraise
      clojure
      go-task
      nixos-anywhere
      nixfmt-rfc-style
      rsync
    ];

    sessionPath = ["$HOME/bin"];
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

  home.file."bin/vmnet-cli".source =
    config.lib.file.mkOutOfStoreSymlink
    "/Applications/VMware\ Fusion.app/Contents/Library/vmnet-cli" ;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.stateVersion = "25.05";
}
