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
    ./xwm.nix
    ./audio.nix
  ];

  settings = osConfig.settings;

  xdg.configFile."nixpkgs/config.nix".source = ../config/nixpkgs.nix;

  home = {
    packages = with pkgs; [
      clojure
      go-task
      nixfmt-rfc-style
      nixos-anywhere
      python3
      rsync
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

  home.file."android-forward-key" = {
    target = "bin/android-forward-key";
    executable = true;
    text = ''
        #!/usr/bin/env bash
        set -e

        KEY_NAME="$1"
        declare -A KEYS=( ["play_or_pause"]="85" ["scan_next_track"]="87" ["scan_prev_track"]="88")

        [ ! -v KEYS["$KEY_NAME"] ] && echo "Key name not found!" && exit

        if /opt/homebrew/bin/adb get-state &> /dev/null ; then
          adb shell input keyevent ''${KEYS["$KEY_NAME"]}
          osascript -e "display notification \"Sent $KEY_NAME to Android\" with title \"Keyboard\""
        else
          echo "Device not found!"
        fi
      '';
  };


  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.stateVersion = "25.05";
}
