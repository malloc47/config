{
  config,
  osConfig,
  pkgs,
  ...
}:
{
  imports = [
    ../modules/settings.nix
    ../config/shell.nix
    ../config/emacs.nix
    ../config/git.nix
    ../config/terminal.nix
    ./wm.nix
  ];

  settings = osConfig.settings;

  xdg.configFile."nixpkgs/config.nix".source = ../config/nixpkgs.nix;

  home = {
    packages = with pkgs; [
      clojure
      go-task
      nixos-anywhere
      nixfmt-rfc-style
      rsync
      python3
    ];

    sessionPath = [ "$HOME/bin" ];
  };

  programs.vim = {
    enable = true;
    defaultEditor = true;
  };

  home.file."DefaultKeyBinding.dict" = {
    source = ./DefaultKeyBinding.dict;
    target = "Library/KeyBindings/DefaultKeyBinding.dict";
  };

  home.file.".hushlogin".text = "";

  home.file."vmware-preferences" = {
    source = ../config/vmware-preferences;
    target = "Library/Preferences/VMware\ Fusion/preferences";
  };

  home.file."bin/vmrun".source =
    config.lib.file.mkOutOfStoreSymlink "/Applications/VMware\ Fusion.app/Contents/Library/vmrun";

  home.file."bin/vmcli".source =
    config.lib.file.mkOutOfStoreSymlink "/Applications/VMware\ Fusion.app/Contents/Library/vmcli";

  home.file."bin/vmnet-cli".source =
    config.lib.file.mkOutOfStoreSymlink "/Applications/VMware\ Fusion.app/Contents/Library/vmnet-cli";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.stateVersion = "25.05";
}
