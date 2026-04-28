{
  config,
  pkgs,
  pkgs-unstable,
  ...
}:
let
  mod = "Alt";
  modShift = "Alt + Shift";
  modShiftCtrl = "Alt + Shift + Ctrl";
in
with pkgs.lib;
{
  imports = [
    ../modules/settings.nix
  ];

  home.packages = with pkgs; [
    # autoraise — not needed, rift has built-in focus-follows-mouse
    flameshot
    rift
    # swipe-aerospace — aerospace-specific, not needed with rift
  ];

  # Rift config (TOML) managed by Nix
  xdg.configFile."rift/config.toml".text = ''
    [settings]
    animate = false
    animation_duration = 0.3
    animation_fps = 100.0
    default_disable = false

    focus_follows_mouse = true
    mouse_follows_focus = true
    mouse_hides_on_focus = false

    auto_focus_blacklist = ["com.apple.Spotlight", "com.raycast.macos"]

    run_on_start = []
    hot_reload = true

    [settings.layout]
    mode = "traditional"

    [settings.layout.master_stack]
    master_ratio = 0.6
    master_count = 1
    master_side = "left"
    new_window_placement = "master"

    [settings.layout.scrolling]
    column_width_ratio = 0.7
    min_column_width_ratio = 0.3
    max_column_width_ratio = 0.9
    alignment = "center"
    focus_navigation_style = "niri"

    [settings.layout.stack]
    stack_offset = 40.0
    default_orientation = "perpendicular"

    [settings.layout.gaps.outer]
    top = 5
    left = 10
    bottom = 10
    right = 10

    [settings.layout.gaps.inner]
    horizontal = 20
    vertical = 20

    [settings.ui.menu_bar]
    enabled = false
    show_empty = false
    mode = "all"
    active_label = "index"
    display_style = "layout"

    [settings.ui.stack_line]
    enabled = false

    [settings.ui.mission_control]
    enabled = false

    [settings.gestures]
    enabled = true
    invert_horizontal_swipe = false
    swipe_vertical_tolerance = 0.4
    skip_empty = true
    fingers = 3
    distance_pct = 0.08
    haptics_enabled = true
    haptic_pattern = "level_change"

    [settings.window_snapping]
    drag_swap_fraction = 0.3

    [virtual_workspaces]
    enabled = true
    default_workspace_count = 10
    auto_assign_windows = true
    preserve_focus_per_workspace = true
    workspace_auto_back_and_forth = false
    reapply_app_rules_on_title_change = false
    default_workspace = 0
    workspace_rules = []
    workspace_names = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

    app_rules = [
      { app_id = "org.flameshot", floating = true },
    ]

    [keys]
    # Launch apps
    "${mod} + Return" = { exec = ["/bin/bash", "-c", "open -a /Applications/Ghostty.app"] }
    "${modShift} + N" = { exec = ["/bin/bash", "-c", "osascript -e 'tell app \"Terminal\" to activate do script \"sudo darwin-rebuild switch; read -s -k ?COMPLETE ; exit\"'"] }
    "${mod} + N" = { exec = ["/bin/bash", "-c", "open -na 'Google Chrome' --args --new-window"] }
    "${modShift} + E" = { exec = ["${pkgs.emacs}/bin/emacsclient", "-c", "-e", "(x-focus-frame nil)"] }
    "${mod} + O" = { exec = ["/bin/bash", "-c", "/Applications/Albert.app/Contents/MacOS/Albert show 'clipboard '"] }
    "${mod} + P" = { exec = ["/bin/bash", "-c", "/Applications/Albert.app/Contents/MacOS/Albert show 'apps '"] }

    # Focus directions
    "${mod} + H" = { move_focus = "left" }
    "${mod} + J" = { move_focus = "down" }
    "${mod} + K" = { move_focus = "up" }
    "${mod} + L" = { move_focus = "right" }

    # Move window
    "${modShift} + H" = { move_node = "left" }
    "${modShift} + J" = { move_node = "down" }
    "${modShift} + K" = { move_node = "up" }
    "${modShift} + L" = { move_node = "right" }

    # Join with (container manipulation)
    "${modShiftCtrl} + H" = { join_window = "left" }
    "${modShiftCtrl} + J" = { join_window = "down" }
    "${modShiftCtrl} + K" = { join_window = "up" }
    "${modShiftCtrl} + L" = { join_window = "right" }

    # Layout
    "${mod} + Space" = "toggle_stack"
    "${modShift} + Space" = "toggle_orientation"

    # Floating / fullscreen
    "${mod} + F" = "toggle_window_floating"
    "${modShift} + F" = "toggle_fullscreen"

    # Display sleep
    "${modShift} + Ctrl + Meta + L" = { exec = ["/bin/bash", "-c", "pmset displaysleepnow"] }

    # Workspaces (rift is 0-indexed)
    "${mod} + 1" = { switch_to_workspace = 0 }
    "${mod} + 2" = { switch_to_workspace = 1 }
    "${mod} + 3" = { switch_to_workspace = 2 }
    "${mod} + 4" = { switch_to_workspace = 3 }
    "${mod} + 5" = { switch_to_workspace = 4 }
    "${mod} + 6" = { switch_to_workspace = 5 }
    "${mod} + 7" = { switch_to_workspace = 6 }
    "${mod} + 8" = { switch_to_workspace = 7 }
    "${mod} + 9" = { switch_to_workspace = 8 }
    "${mod} + 0" = { switch_to_workspace = 9 }

    # Move window to workspace
    "${modShift} + 1" = { move_window_to_workspace = 0 }
    "${modShift} + 2" = { move_window_to_workspace = 1 }
    "${modShift} + 3" = { move_window_to_workspace = 2 }
    "${modShift} + 4" = { move_window_to_workspace = 3 }
    "${modShift} + 5" = { move_window_to_workspace = 4 }
    "${modShift} + 6" = { move_window_to_workspace = 5 }
    "${modShift} + 7" = { move_window_to_workspace = 6 }
    "${modShift} + 8" = { move_window_to_workspace = 7 }
    "${modShift} + 9" = { move_window_to_workspace = 8 }
    "${modShift} + 0" = { move_window_to_workspace = 9 }

    # Workspace navigation
    "${mod} + Tab" = "switch_to_last_workspace"
    "${mod} + Minus" = "prev_workspace"
    "${mod} + Equal" = "next_workspace"

    # Resize
    "${modShift} + Equal" = "resize_window_grow"
    "${modShift} + Minus" = "resize_window_shrink"

    # Close focused window (Cmd+W via System Events)
    "${modShift} + C" = { exec = ["/bin/bash", "-c", "osascript -e 'tell application \"System Events\" to keystroke \"w\" using command down'"] }

    # Flameshot
    "${modShift} + X" = { exec = ["${pkgs.flameshot}/bin/flameshot", "gui"] }

    # Rift management
    "${mod} + Z" = "toggle_space_activated"
    "${mod} + Ctrl + Q" = "save_and_exit"
  '';

  launchd.agents.rift = {
    enable = true;
    config = {
      ProgramArguments = [
        "${pkgs.rift}/bin/rift"
      ];
      ProcessType = "Interactive";
      KeepAlive = {
        SuccessfulExit = true;
      };
      RunAtLoad = true;
    };
  };

  # Aerospace — disabled in favor of rift
  # programs.aerospace = { ... };

  # AutoRaise — disabled, rift has built-in focus-follows-mouse
  # launchd.agents.autoraise = { ... };

  # SwipeAeroSpace — aerospace-specific, disabled
  # launchd.agents.swipe-aerospace = { ... };

  services.jankyborders = {
    enable = true;
    settings = {
      hidpi = true;
      active_color = "0xffd33682"; # match I3 color
      width = 10.0;
    };
  };

  targets.darwin.defaults = {
    # SwipeAeroSpace defaults removed
  };

  home.file."Library/Preferences/albert/config" = {
    target = "Library/Preferences/albert/config";
    source = ./. + "/../config/albert.conf";
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

  xdg.configFile."karabiner/karabiner.json".source = ./karabiner.json;

  ## This is not yet available in 25.05
  #programs.sketchybar = {
  #  enable = true;
  #};

}
