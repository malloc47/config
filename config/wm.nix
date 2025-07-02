{ config, pkgs, ... }:
let
  mod = "Mod4";
in
with pkgs.lib;
{
  imports = [ ../modules/settings.nix ];

  home.packages = with pkgs; [xclip];

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;
      colors.focused = {
        border = "#d33682";
        background = "#4c7899";
        text = "#ffffff";
        indicator = "#d33682";
        childBorder = "#d33682";
      };
      bars = [
        {
          id = "bar-0";
          position = "top";
          hiddenState = "hide";
          statusCommand = "${pkgs.i3status}/bin/i3status";
          fonts = {
            names = [ config.settings.fontName ];
            size = config.settings.fontSize;
          };
          colors = {
            separator = "#586e75";
            background = "#fdf6e3";
            statusline = "#657b83";
            focusedWorkspace = {
              background = "#586e75";
              border = "#93a1a1";
              text = "#002b36";
            };
            activeWorkspace = {
              background = "#fdf6e3";
              border = "#6c71c4";
              text = "#fdf6e3";
            };
            inactiveWorkspace = {
              background = "#eee8d5";
              border = "#b58900";
              text = "#657b83";
            };
            urgentWorkspace  = {
              background = "#d33682";
              border = "#d33682";
              text = "#fdf6e3";
            };
          };
          extraConfig = ''
            modifier none
          '';
        }
      ];
      keybindings = (
        {
          "${mod}+p" = "exec ${pkgs.rofi}/bin/rofi -show run";
          "${mod}+o" = "exec ${pkgs.clipcat}/bin/clipcat-menu";
          "${mod}+q" = "reload";
          "${mod}+Control+q" = "restart";
          "${mod}+Shift+q" = "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";
          "${mod}+Shift+c" = "kill";
          "${mod}+Return" = "exec ${config.settings.terminal}";
          "${mod}+Shift+Return" = "exec ${config.settings.terminal} -e tmux";
          "${mod}+Shift+e" = "exec emacsclient -c";
          "${mod}+g" = "exec ${pkgs.rofi}/bin/rofi -show window";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";
          "${mod}+h" = "focus left";
          "${mod}+u" = "focus parent";
          "${mod}+Shift+U" = "focus child";
          "${mod}+Shift+J" = "move down";
          "${mod}+Shift+K" = "move up";
          "${mod}+Shift+L" = "move right";
          "${mod}+Shift+H" = "move left";
          "${mod}+c" = "layout tabbed";
          "${mod}+x" = "split v";
          "${mod}+z" = "split h";
          "${mod}+space" = "layout toggle splitv splith tabbed";
          "${mod}+y" = "bar mode toggle";
          "${mod}+Shift+N" = "exec \"xterm -e 'sudo nixos-rebuild switch; read -s -k \\?COMPLETE'\"";
          "${mod}+Shift+r" = "nop";
          "${mod}+v" = "nop";
          "${mod}+e" = "nop";
          "${mod}+s" = "nop";
          "${mod}+Shift+>" = "move workspace to output right";
          "${mod}+Shift+<" = "move workspace to output left";
          "${mod}+Shift+Control+f" = "sticky toggle";
          # Defaults
          "${mod}+f" = "focus mode_toggle";
          "${mod}+Shift+F" = "floating toggle";
          # "${mod}+f" = "fullscreen toggle";
          "${mod}+a" = "focus parent";
          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 10";
          "${mod}+Shift+1" = "move container to workspace number 1";
          "${mod}+Shift+2" = "move container to workspace number 2";
          "${mod}+Shift+3" = "move container to workspace number 3";
          "${mod}+Shift+4" = "move container to workspace number 4";
          "${mod}+Shift+5" = "move container to workspace number 5";
          "${mod}+Shift+6" = "move container to workspace number 6";
          "${mod}+Shift+7" = "move container to workspace number 7";
          "${mod}+Shift+8" = "move container to workspace number 8";
          "${mod}+Shift+9" = "move container to workspace number 9";
          "${mod}+Shift+0" = "move container to workspace number 10";
          "${mod}+Tab" = "workspace back_and_forth";
          "${mod}+r" = "mode resize";
        }
        // optionalAttrs (!config.settings.vm)
          {
            "${mod}+equal" = "workspace next";
            "${mod}+minus" = "workspace prev";
            "${mod}+grave" = "workspace 1";
            "${mod}+Shift+Control+L" = "exec i3lock -c 000000";
            "XF86AudioRaiseVolume" = "exec --no-startup-id amixer sset Master 5%+ unmute";
            "XF86AudioLowerVolume" = "exec --no-startup-id amixer sset Master 5%- unmute";
            "XF86AudioMute" = "exec --no-startup-id amixer sset Master toggle";
          });
      modes.resize = {
        "h" = "resize shrink width 10 px or 10 ppt";
        "j" = "resize grow height 10 px or 10 ppt";
        "k" = "resize shrink height 10 px or 10 ppt";
        "l" = "resize grow width 10 px or 10 ppt";
        "Escape" = "mode default";
        "Return" = "mode default";
      };
      window = {
        titlebar = false;
        border = 2;
      };
    };
    # inexplicably xserver wrapper doesn't set the background image
    extraConfig = ''
      focus_wrapping no
      exec_always "if [ -e $HOME/.background-image ]; then feh --bg-scale $HOME/.background-image ; fi"
      exec i3-msg workspace 1
      for_window [class="(.*join\?action\=join.*|.*zoom.*)"] floating enable
      for_window [class="(.*join\?action\=join.*|.*zoom.*)" title="Zoom - Licensed Account"] floating disable
      for_window [class="(.*join\?action\=join.*|.*zoom.*)" title="Zoom Workplace - Licensed account"] floating disable
      for_window [class="(.*join\?action\=join.*|.*zoom.*)" title="Meeting"] floating disable
    '';
  };

  programs.i3status = {
    enable = true;
    enableDefault = false;
    general = {
      colors = true;
      color_good = "#859900";
      color_degraded = "#B58900";
      color_bad = "#DC322F";
      interval = 5;
    };
    modules = {
      "disk /" = {
        position = 20;
        settings = {
          format = "/ %avail";
        };
      };
      "load" = {
        position = 30;
        settings = {
          format = "Load: %1min";
        };
      };
      "tztime local" = mkDefault {
        position = 40;
        settings = {
          format = "🗽 %Y-%m-%d %l:%M %P";
          min_width = "WWWWWWWWWWWWWWWWWWWWWWWW";
        };
      };
    };
  };


  xresources.properties = {
    "xterm*faceName" = "${config.settings.fontName}";
    "xterm*faceSize" = "${head (splitString "." (toString config.settings.fontSize))}";
  };

  xsession.initExtra =
    if (config.settings.xkbFile != "none" ) then
      let
        xkbFile = ../xkb + "/${config.settings.xkbFile}.xkb";
        compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${xkbFile} $out
       '';
      in
        "${pkgs.xorg.xkbcomp}/bin/xkbcomp ${compiledLayout} $DISPLAY"
    else
      "";

  programs.rofi = {
    enable = true;
    font = config.settings.fontName + " " +
           (head (splitString "." (toString config.settings.fontSize)));
    terminal = "${pkgs.alacritty}/bin/alacritty";
    extraConfig = {
      # Makes rofi default to using the monitor DPI
      dpi = 0;
    };
  };

  # TODO: customize this based on DPI?
  programs.chromium.commandLineArgs = [
    "--high-dpi-support=1"
    "--force-device-scale-factor=2"
  ];

  services.screen-locker = {
    enable = !config.settings.vm;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
    inactiveInterval = 5; # minutes
  };

  services.clipcat.enable = true;
}
