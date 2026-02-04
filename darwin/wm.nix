{
  config,
  pkgs,
  pkgs-unstable,
  ...
}:
let
  mod = "alt";
  pkgs-aerospace = pkgs-unstable.aerospace;
in
with pkgs.lib;
{
  imports = [
    ../modules/settings.nix
  ];

  home.packages = with pkgs; [
    autoraise
    swipe-aerospace
    # albert
    flameshot
  ];

  launchd.agents.autoraise = {
    enable = true;
    config = {
      ProgramArguments = [
        "${pkgs.autoraise}/bin/autoraise"
        "-delay"
        "0"
        "-focusDelay"
        "1"
        "-mouseDelta"
        "0.5"
      ];
      ProcessType = "Interactive";
      KeepAlive = {
        SuccessfulExit = true;
      };
      RunAtLoad = true;
    };
  };

  programs.aerospace = {
    enable = true;
    package = pkgs-aerospace;
    launchd.enable = true;
    userSettings = {
      enable-normalization-flatten-containers = true;
      enable-normalization-opposite-orientation-for-nested-containers = true;
      default-root-container-layout = "tiles";
      default-root-container-orientation = "auto";
      automatically-unhide-macos-hidden-apps = true;

      mode.main.binding = {
        "${mod}-enter" = "exec-and-forget ${pkgs.alacritty}/bin/alacritty";
        "${mod}-shift-n" = ''
          exec-and-forget osascript -e '
                    tell app "Terminal"
                      activate
                      do script "sudo darwin-rebuild switch; read -s -k \\?COMPLETE ; exit"
                    end tell'
        '';
        "${mod}-n" = "exec-and-forget open -na \"Google Chrome\" --args --new-window";
        "${mod}-shift-e" = "exec-and-forget ${pkgs.emacs}/bin/emacsclient -c -e \"(x-focus-frame nil)\"";
        # Switch to the homebrew Albert for now
        "${mod}-o" = "exec-and-forget /Applications/Albert.app/Contents/MacOS/Albert show \"clipboard \"";
        "${mod}-p" = "exec-and-forget /Applications/Albert.app/Contents/MacOS/Albert show \"apps \"";
        # "${mod}-o" = "exec-and-forget ${pkgs.albert}/bin/albert show \"clipboard \"";
        # "${mod}-p" = "exec-and-forget ${pkgs.albert}/bin/albert show \"apps \"";
        "${mod}-h" = "focus left";
        "${mod}-j" = "focus down";
        "${mod}-k" = "focus up";
        "${mod}-l" = "focus right";
        "${mod}-shift-h" = "move left";
        "${mod}-shift-j" = "move down";
        "${mod}-shift-k" = "move up";
        "${mod}-shift-l" = "move right";
        "${mod}-shift-ctrl-h" = "join-with left";
        "${mod}-shift-ctrl-j" = "join-with down";
        "${mod}-shift-ctrl-k" = "join-with up";
        "${mod}-shift-ctrl-l" = "join-with right";
        "${mod}-space" = "layout tiles accordion";
        "${mod}-shift-space" = "layout vertical horizontal";
        "${mod}-f" = ["layout floating tiling"
          ''
          exec-and-forget osascript -e '
            set windowTitle to ""
            tell application "System Events" to tell (first process whose frontmost is true) to set windowTitle to name of window 1
            if (windowTitle is not equal to "Zoom Meeting") then
              error number -128
            end if
            tell application "System Events"
              tell process "zoom.us"
                click menu item "Keep on top" of menu 1 of menu bar item "Meeting" of menu bar 1
              end tell
            end tell'
          '' ];
        "${mod}-shift-f" = "fullscreen";
        "${mod}-shift-ctrl-cmd-l" = "exec-and-forget pmset displaysleepnow";
        "${mod}-backtick" = "workspace 1";
        "${mod}-1" = "workspace 1";
        "${mod}-2" = "workspace 2";
        "${mod}-3" = "workspace 3";
        "${mod}-4" = "workspace 4";
        "${mod}-5" = "workspace 5";
        "${mod}-6" = "workspace 6";
        "${mod}-7" = "workspace 7";
        "${mod}-8" = "workspace 8";
        "${mod}-9" = "workspace 9";
        "${mod}-0" = "workspace 10";
        "${mod}-shift-1" = "move-node-to-workspace 1";
        "${mod}-shift-2" = "move-node-to-workspace 2";
        "${mod}-shift-3" = "move-node-to-workspace 3";
        "${mod}-shift-4" = "move-node-to-workspace 4";
        "${mod}-shift-5" = "move-node-to-workspace 5";
        "${mod}-shift-6" = "move-node-to-workspace 6";
        "${mod}-shift-7" = "move-node-to-workspace 7";
        "${mod}-shift-8" = "move-node-to-workspace 8";
        "${mod}-shift-9" = "move-node-to-workspace 9";
        "${mod}-shift-0" = "move-node-to-workspace 10";
        "${mod}-tab" = "workspace-back-and-forth";
        "${mod}-shift-equal" = "balance-sizes";
        "${mod}-minus" = "workspace prev";
        "${mod}-equal" = "workspace next";
        "${mod}-shift-c" = "close";
        "${mod}-r" = "mode resize";
        "${mod}-esc" = [
          "mode vm"
          "exec-and-forget ssh jwaggoner@localhost -p 2222 'i3-msg -s /run/user/*/i3/ipc-socket*  mode default'"
        ];
        "${mod}-shift-x" = "exec-and-forget ${pkgs.flameshot}/bin/flameshot gui";
      };

      mode.resize.binding = {
        h = "resize width -150";
        j = "resize height +150";
        k = "resize height -150";
        l = "resize width +150";
        enter = "mode main";
        esc = "mode main";
      };

      # on-focus-changed = [
      #   "exec-and-forget ${config.home.homeDirectory}/bin/aerospace-focus"
      # ];

      # Free all the keybindings for the vm
      mode.vm.binding = {
        "${mod}-backtick" = "workspace 1";
        "${mod}-minus" = "workspace prev";
        "${mod}-equal" = "workspace next";
        "${mod}-esc" = [
          "mode main"
          "exec-and-forget ssh jwaggoner@localhost -p 2222 'i3-msg -s /run/user/*/i3/ipc-socket*  mode window'"
        ];
      };

      # on-window-detected = [{
      #     "if".app-id = "com.vmware.fusion";
      #     check-further-callbacks = false;
      #     run = ["move-node-to-workspace 2"];
      #   }
      # ];

      on-window-detected = [
        {
          "if".app-id = "org.flameshot";
          run = [ "layout floating" ];
        }
      ];

      gaps = {
        inner.horizontal = 20;
        inner.vertical = 20;
        outer.left = 10;
        outer.bottom = 10;
        outer.top = 5;
        outer.right = 10;
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
        "10" = "main";
      };
    };
  };

  home.file."bin/aerospace-focus" = {
    target = "bin/aerospace-focus";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      focused () {
        echo $(${pkgs-aerospace}/bin/aerospace list-windows --focused --format "%{app-bundle-id}")
      }
      if [[ "$(focused)" == "com.vmware.fusion" ]]; then
        sendkeys --targeted --no-activate --initial-delay 0 --application-name "VMWare Fusion" -c '<c:g:command>'
      #else
      #    ${pkgs-aerospace}/bin/aerospace mode main
      fi
    '';
  };

  services.jankyborders = {
    enable = true;
    settings = {
      hidpi = true;
      active_color = "0xffd33682"; # match I3 color
      width = 10.0;
    };
  };

  launchd.agents.swipe-aerospace = {
    enable = true;
    config = {
      ProgramArguments = [
        "${pkgs.swipe-aerospace}/Applications/SwipeAeroSpace.app/Contents/MacOS/SwipeAeroSpace"
      ];
      ProcessType = "Interactive";
      KeepAlive = {
        SuccessfulExit = true;
      };
      RunAtLoad = true;
    };
  };

  targets.darwin.defaults = {
    "club.mediosz.SwipeAeroSpace" = {
      "skip-empty" = 1;
      fingers = "Three";
      threshold = 0.3;
    };
  };

  launchd.agents.albert = {
    enable = true;
    config = {
      ProgramArguments = [
        # "${pkgs.albert}/Applications/Albert.app/Contents/MacOS/Albert"
        "/Applications/Albert.app/Contents/MacOS/Albert"
      ];
      ProcessType = "Interactive";
      KeepAlive = {
        SuccessfulExit = true;
      };
      RunAtLoad = true;
    };
  };

  launchd.agents.flameshot = {
    enable = true;
    config = {
      ProgramArguments = [ "${pkgs.flameshot}/bin/flameshot" ];
      ProcessType = "Interactive";
      KeepAlive = {
        SuccessfulExit = true;
      };
      RunAtLoad = true;
    };
  };

  xdg.configFile."flameshot/flameshot.ini".text = ''
    [General]
    disabledTrayIcon=true
    useJpgForClipboard=true
    savePath=${config.home.homeDirectory}/Downloads

    [Shortcuts]
    SCREENSHOT_HISTORY=
    TAKE_SCREENSHOT=
  '';

  ## This is not yet available in 25.05
  #programs.sketchybar = {
  #  enable = true;
  #};

}
