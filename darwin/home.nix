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

  launchd.agents.autoraise = {
    enable = true;
    config = {
      ProgramArguments = [ "${pkgs.autoraise}/bin/autoraise" ];
      ProcessType = "Interactive";
      KeepAlive = {SuccessfulExit = true;};
      RunAtLoad = true;
    };
  };

  programs.vim = {
    enable = true;
    defaultEditor = true;
  };

  targets.darwin.keybindings = {
    "&#xF729;" = "moveToBeginningOfLine:";
    "&#xF72B;" = "moveToEndOfLine:";
    "$&#xF729;" = "moveToBeginningOfLineAndModifySelection:";
    "$&#xF72B;" = "moveToEndOfLineAndModifySelection:";
    "^&#xF729;" = "moveToBeginningOfDocument:";
    "^&#xF72B;" = "moveToEndOfDocument:";
    "^$&#xF729;" = "moveToBeginningOfDocumentAndModifySelection:";
    "^$&#xF72B;" = "moveToEndOfDocumentAndModifySelection:";
  };

  services.skhd = {
    enable = false;
    config = ''
      alt - o : ${pkgs.clipcat}/bin/clipcat-menu

      .blacklist [
        "vmware fusion"
      ]
    '';
  };

  programs.aerospace = {
    enable = true;
    userSettings = {

      enable-normalization-flatten-containers = true;
      enable-normalization-opposite-orientation-for-nested-containers = true;
      default-root-container-layout = "tiles";
      default-root-container-orientation = "auto";
      automatically-unhide-macos-hidden-apps = true;

      mode.main.binding = {
        alt-enter = ''
          exec-and-forget osascript -e '
            tell application "Terminal"
                do script
                activate
            end tell'
        '';
        alt-o = "exec-and-forget PATH=$PATH:${pkgs.choose-gui}/bin ${pkgs.clipcat}/bin/clipcat-menu";
        alt-h = "focus left";
        alt-j = "focus down";
        alt-k = "focus up";
        alt-l = "focus right";
        alt-shift-h = "move left";
        alt-shift-j = "move down";
        alt-shift-k = "move up";
        alt-shift-l = "move right";
        alt-space = "layout tiles accordion";
        alt-shift-space = "layout vertical horizontal";
        alt-f = "layout floating tiling";
        alt-shift-f = "fullscreen";
        alt-backtick = "workspace 1";
        alt-1 = "workspace 1";
        alt-2 = "workspace 2";
        alt-3 = "workspace 3";
        alt-4 = "workspace 4";
        alt-5 = "workspace 5";
        alt-6 = "workspace 6";
        alt-7 = "workspace 7";
        alt-8 = "workspace 8";
        alt-9 = "workspace 9";
        alt-shift-1 = "move-node-to-workspace 1";
        alt-shift-2 = "move-node-to-workspace 2";
        alt-shift-3 = "move-node-to-workspace 3";
        alt-shift-4 = "move-node-to-workspace 4";
        alt-shift-5 = "move-node-to-workspace 5";
        alt-shift-6 = "move-node-to-workspace 6";
        alt-shift-7 = "move-node-to-workspace 7";
        alt-shift-8 = "move-node-to-workspace 8";
        alt-shift-9 = "move-node-to-workspace 9";
        alt-tab = "workspace-back-and-forth";
        alt-shift-equal = "balance-sizes";
        alt-minus = "workspace prev";
        alt-equal = "workspace next";
        alt-r = "mode resize";
        alt-esc = "mode vm";

      };
      mode.resize.binding = {
        h = "resize width -200";
        j = "resize height +200";
        k = "resize height -200";
        l = "resize width +200";
        enter = "mode main";
        esc = "mode main";
      };
      on-focus-changed = [
        "exec-and-forget ${config.home.homeDirectory}/bin/aerospace-focus"
      ];
      # Free all the keybindings for the vm
      mode.vm.binding = {
        alt-backtick = "workspace 1";
        alt-minus = "workspace prev";
        alt-equal = "workspace next";
        alt-esc = "mode main";
        alt-shift-ctrl-h = "focus left";
        alt-shift-ctrl-j = "focus down";
        alt-shift-ctrl-k = "focus up";
        alt-shift-ctrl-l = "focus right";
      };
      on-window-detected = [{
          "if".app-id = "com.vmware.fusion";
          check-further-callbacks = false;
          run = ["move-node-to-workspace 2"];
        }
      ];

      gaps = {
        inner.horizontal = 4;
        inner.vertical =   4;
        outer.left =       4;
        outer.bottom =     4;
        outer.top =        4;
        outer.right =      4;
      };
      workspace-to-monitor-force-assignment = {
        "1" = "main";
        "2" = "main";
        "3" = "main";
        "4" = "main";
        "5" = "main";
        "6" = "main";
        "7" = "main";
        "8" = "main";
        "9" = "main";
      };
    };
  };

  home.file."bin/aerospace-focus" = {
    target = "bin/aerospace-focus";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      focused=$(${pkgs.aerospace}/bin/aerospace list-windows --focused --format "%{app-bundle-id}")
      if [[ "$focused" == "com.vmware.fusion" ]]; then
          ${pkgs.aerospace}/bin/aerospace mode vm
      else
          ${pkgs.aerospace}/bin/aerospace mode main
      fi
    '';
  };

  launchd.agents.aerospace = {
    enable = true;
    config = {
      ProgramArguments = [ "${pkgs.aerospace}/Applications/Aerospace.app/Contents/MacOS/AeroSpace" ];
      ProcessType = "Interactive";
      KeepAlive = {SuccessfulExit = true;};
      RunAtLoad = true;
      StandardOutPath = "/tmp/aerospace.log";
      StandardErrorPath = "/tmp/aerospace.err.log";
    };
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
