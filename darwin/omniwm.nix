{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:
let
  # settings.toml below targets the 0.7.6 schema, which OmniWM validates
  # strictly: one unknown hotkey id or missing key and it ignores the whole
  # file. Bump the version and the settings together. (nixpkgs-unstable is
  # still on 0.7.4.)
  omniwm = pkgs-unstable.omniwm.overrideAttrs {
    version = "0.7.6";
    src = pkgs.fetchurl {
      url = "https://github.com/OmniNull/OmniWM/releases/download/v0.7.6/OmniWM-v0.7.6.zip";
      hash = "sha256-7IxhRqbyGnnhxArHDWESF2K5PPU80wjmPrROis4g/Nw=";
    };
  };
  omniwmctl = "${omniwm}/bin/omniwmctl";

  color = red: green: blue: alpha: {
    inherit
      red
      green
      blue
      alpha
      ;
  };
  rgb =
    r: g: b:
    color (r / 255.0) (g / 255.0) (b / 255.0) 1.0;

  # Keys for workspaces 1-10, as in AeroSpace
  digits = map toString (lib.range 1 9) ++ [ "0" ];

  # OmniWM hotkeys by action id. Anything left out is unassigned, including
  # the OmniWM defaults that collide with the AeroSpace bindings (Option+Return,
  # Option+Shift+F, Option+Minus/Equal, Option+Shift+Minus/Equal). Launchers,
  # alt-f and the resize mode live in skhd below.
  hotkeys =
    lib.listToAttrs (
      lib.concatLists (
        lib.imap0 (i: key: [
          (lib.nameValuePair "switchWorkspace.${toString i}" "Option+${key}")
          (lib.nameValuePair "moveToWorkspace.${toString i}" "Option+Shift+${key}")
        ]) digits
      )
    )
    // {
      "focus.left" = "Option+H";
      "focus.down" = "Option+J";
      "focus.up" = "Option+K";
      "focus.right" = "Option+L";

      # AeroSpace `move` swaps the window with its neighbor; in OmniWM that's
      # move-column, which moves the whole tile or group.
      "moveColumn.left" = "Option+Shift+H";
      "moveColumn.down" = "Option+Shift+J";
      "moveColumn.up" = "Option+Shift+K";
      "moveColumn.right" = "Option+Shift+L";

      # AeroSpace `join-with`: OmniWM's move joins the neighbor into a tab
      # group (or extracts from one) in Dwindle, consumes/expels in Niri.
      "move.left" = "Control+Option+Shift+H";
      "move.down" = "Control+Option+Shift+J";
      "move.up" = "Control+Option+Shift+K";
      "move.right" = "Control+Option+Shift+L";

      # `layout tiles accordion` (Niri only) / `layout vertical horizontal`
      # (Dwindle only)
      "toggleColumnTabbed" = "Option+Space";
      "toggleSplit" = "Option+Shift+Space";
      "toggleWorkspaceLayout" = "Control+Option+Shift+Space";

      "toggleFullscreen" = "Option+Shift+F";
      "balanceSizes" = "Option+Shift+Equal";
      "closeFocusedWindow" = "Option+Shift+C";
      "workspaceBackAndForth" = "Option+Tab";
      "switchWorkspace.previous" = "Option+Minus";
      "switchWorkspace.next" = "Option+Equal";

      # OmniWM defaults with no AeroSpace counterpart. focusPrevious trades
      # places with workspaceBackAndForth.
      "focusPrevious" = "Control+Option+Tab";
      "moveWindowToWorkspaceUp" = "Control+Option+Shift+Up Arrow";
      "moveWindowToWorkspaceDown" = "Control+Option+Shift+Down Arrow";
      "moveColumnToWorkspaceUp" = "Control+Option+Shift+Page Up";
      "moveColumnToWorkspaceDown" = "Control+Option+Shift+Page Down";
      "focusMonitorNext" = "Control+Command+Tab";
      "focusMonitorLast" = "Control+Command+Grave";
      "focusColumnFirst" = "Option+Home";
      "focusColumnLast" = "Option+End";
      "moveColumnToFirst" = "Control+Option+Home";
      "moveColumnToLast" = "Control+Option+End";
      "cycleSizeForward" = "Option+Period";
      "cycleSizeBackward" = "Option+Comma";
      "expandContainerToAvailablePrimarySpan" = "Control+Option+F";
      "resetWindowSecondarySpan" = "Control+Option+R";
      "raiseAllFloatingWindows" = "Option+Shift+R";
      "openCommandPalette" = "Control+Option+Space";
      "openMenuAnywhere" = "Control+Option+M";
      "toggleQuakeTerminal" = "Option+Grave";
      "toggleOverview" = "Option+Shift+O";
    }
    // lib.listToAttrs (
      lib.imap0 (i: key: lib.nameValuePair "focusColumn.${toString i}" "Control+Option+${key}") (
        lib.take 9 digits
      )
    );

  # Workspace ids must stay stable across edits. 1-9 reuse OmniWM's built-in
  # ids; like AeroSpace's workspace-to-monitor-force-assignment, all ten live
  # on the main monitor.
  workspaceIds = [
    "AD36F001-C57E-41A5-AC1D-DF5249D007F0"
    "454CECD4-5E9D-4ED1-95D7-979D48817F5F"
    "BEB842B5-E894-4791-9FD1-397C3CDD3538"
    "248AA883-2261-4D45-943C-79C0E46A232B"
    "8B8C45D6-CE9E-41D9-BD50-BE4989D5E3DE"
    "5953F2BF-A378-4266-91B2-287174C4FA4D"
    "A7D5E104-6985-4516-8ED5-07F144F2A33D"
    "0E18F6B0-345A-4078-8B21-33641F07A861"
    "8D4D711C-2B2F-4FEC-86BF-FA952037738A"
    "8A817103-1948-4155-B4B2-CC8FCF74B5D3"
  ];

  minSize = id: bundleId: minWidth: minHeight: {
    inherit
      id
      bundleId
      minWidth
      minHeight
      ;
  };

  # Complete settings.toml: OmniWM rejects the file if any required key is
  # missing. See https://omniwm.app/config/settings-reference/ (Double-typed
  # keys must be written as floats).
  settings = {
    schemaVersion = 4;

    general = {
      hotkeysEnabled = true;
      systemHyperTrigger = "None";
      hyperKeyModifiers = "Control+Option+Shift+Command";
      defaultLayoutType = "dwindle";
      preventSleepEnabled = false;
      # Upgrades come from bumping the package above
      updateChecksEnabled = false;
      # For omniwmctl, which the skhd bindings use
      ipcEnabled = true;
      animationsEnabled = true;
    };

    # Replaces AutoRaise (-delay 0 -focusDelay 1): focus on hover, no raise
    focus = {
      followsMouse = true;
      raiseOnMouseFocus = false;
      lockModifier = "off";
      moveMouseToFocusedWindow = false;
      followsWindowToMonitor = false;
      crossesMonitorAtEdge = false;
      moveCrossesMonitorAtEdge = false;
    };

    mouseWarp = {
      margin = 1;
      enabled = true;
      constrainToArrangement = false;
    };

    routing = {
      mode = "macOS";
      arrangements = [ ];
    };

    gaps = {
      size = 20.0;
      # A lone window and fullscreen keep the outer gaps, as in AeroSpace
      fullscreenUsesOuterGaps = true;
      outer = {
        left = 10.0;
        right = 10.0;
        top = 5.0;
        bottom = 10.0;
      };
    };

    niri = {
      visibleContainerCount = 2;
      infiniteLoop = false;
      centerFocusedColumn = "never";
      alwaysCenterSingleColumn = false;
      singleWindowFit = "fill";
    };

    dwindle = {
      smartSplit = false;
      defaultSplitRatio = 1.0;
      splitWidthMultiplier = 1.0;
      singleWindowFit = "fill";
      useGlobalGaps = true;
      moveToRootStable = true;
    };

    # Replaces JankyBorders, same color (matches i3)
    borders = {
      enabled = true;
      width = 10.0;
      color = rgb 211 54 130;
    };

    overview = {
      zoom = 1.0;
      backdrop = color 0.05 0.05 0.08 0.0;
      windowBorders = {
        normal = color 0.3 0.3 0.35 0.5;
        hovered = color 0.4 0.6 1.0 1.0;
        selected = color 0.3 0.8 0.4 1.0;
      };
    };

    workspaceBar = {
      enabled = true;
      showLabels = true;
      showFloatingWindows = false;
      windowLevel = "popup";
      position = "overlappingMenuBar";
      notchMode = "moveBelowMenuBar";
      notchActiveZoneWidth = 180.0;
      systemStatsButton = false;
      deduplicateAppIcons = false;
      # Also makes workspace next/prev and swipes skip empty workspaces, like
      # AeroSpace and SwipeAeroSpace's skip-empty
      hideEmptyWorkspaces = true;
      excludedBundleIDs = [ ];
      iconOverrides = { };
      # On a notched display the bar drops below the menu bar; don't let it
      # cover the tops of windows
      reserveLayoutSpace = true;
      revealModifier = "off";
      revealHoldMilliseconds = 200.0;
      hideInNativeFullscreen = false;
      height = 24.0;
      backgroundOpacity = 0.1;
      xOffset = 0.0;
      yOffset = 0.0;
    };

    # Replaces SwipeAeroSpace: three-finger horizontal swipe between
    # workspaces. Niri column scrolling moves to four fingers to stay clear.
    gestures = {
      scrollEnabled = true;
      scrollSensitivity = 5.0;
      scrollModifierKey = "optionShift";
      mouseMoveModifierKey = "option";
      mouseResizeModifierKey = "option";
      fingerCount = 4;
      invertDirection = true;
      trackpadScrollStyle = "snap";
      workspaceSwipeEnabled = true;
      workspaceSwipeFingerCount = 3;
      workspaceSwipeAxis = "horizontal";
    };

    statusBar = {
      showWorkspaceName = false;
      showAppNames = false;
      useWorkspaceId = false;
    };

    hiddenBar = {
      enabled = true;
      hiddenBundleIDs = [ ];
      rehideIntervalSeconds = 5.0;
    };

    clipboard = {
      historyEnabled = false;
      maxItems = 200;
      maxItemBytes = 8388608;
      maxTotalBytes = 67108864;
    };

    quakeTerminal = {
      enabled = true;
      position = "center";
      widthPercent = 50.0;
      heightPercent = 50.0;
      animationDuration = 0.2;
      autoHide = false;
      backgroundEffect = "standardBlur";
    };

    scratchpads.labels = { };

    appearance.mode = "dark";

    hotkeys = lib.mapAttrsToList (id: binding: { inherit id binding; }) hotkeys;

    workspaces = lib.imap1 (n: id: {
      inherit id;
      name = toString n;
      layoutType = "default";
      monitorAssignment.type = "main";
    }) workspaceIds;

    appRules = [
      {
        id = "776FF5ED-9519-491C-8270-F143720E45BC";
        bundleId = "org.flameshot";
        layout = "float";
      }
      # OmniWM's built-in minimum sizes, which this file would otherwise drop
      (minSize "6A31F08A-4051-4354-B439-42F4C71894A3" "com.openai.codex" 800.0 600.0)
      (minSize "4BA546DA-2875-4BEF-B13F-1539E833B1A0" "com.eltima.cmd1.pro.mas" 950.0 550.0)
      (minSize "486CEFA6-69AA-4A3C-AF27-BCD38F4F138B" "com.google.Chrome" 500.0 375.0)
      (minSize "979F05F4-FFA2-4EDD-B23F-08A9944C759F" "dev.zed.Zed" 360.0 240.0)
      (minSize "81426D13-C1A5-475E-AFBC-00BBA05042D0" "com.apple.Safari" 574.0 220.0)
      (minSize "1CF39647-F30D-4E76-9686-79B551F1B094" "app.zen-browser.zen" 500.0 495.0)
      (minSize "005C00D3-F665-47F8-BDAE-D80790E9E46B" "org.mozilla.firefox" 500.0 120.0)
      (minSize "C21156B1-0224-4998-97E3-8F4FA65B9F3B" "company.thebrowser.dia" 500.0 420.0)
      (minSize "2DE9390B-0DB4-4D0C-9ABA-06F76F1D4EA9" "com.spotify.client" 800.0 600.0)
      (minSize "AF752D95-8497-4844-BE20-4C93E73BAEF2" "com.hnc.Discord" 800.0 500.0)
      (minSize "7876C9EF-437E-4D4F-9C27-B1B02F4AABCE" "com.mitchellh.ghostty" 90.0 48.0)
      (minSize "8ECAB78B-BCDD-4245-BC25-1609A49B1C86" "com.microsoft.Outlook" 930.0 650.0)
      (minSize "552FB77D-BF0E-4737-90A6-B5BC6986C579" "com.apple.MobileSMS" 660.0 320.0)
    ];

    monitorBarOverrides = [ ];
    monitorOrientationOverrides = [ ];
    monitorNiriOverrides = [ ];
    monitorDwindleOverrides = [ ];
    monitorGapOverrides = [ ];
  };

  settingsFile = (pkgs.formats.toml { }).generate "omniwm-settings.toml" settings;

  # AeroSpace's alt-f also pinned Zoom's meeting window on top
  zoomKeepOnTop = pkgs.writeText "zoom-keep-on-top.applescript" ''
    set windowTitle to ""
    tell application "System Events" to tell (first process whose frontmost is true) to set windowTitle to name of window 1
    if (windowTitle is not equal to "Zoom Meeting") then
      error number -128
    end if
    tell application "System Events"
      tell process "zoom.us"
        click menu item "Keep on top" of menu 1 of menu bar item "Meeting" of menu bar 1
      end tell
    end tell
  '';

  # AeroSpace's resize mode. The resize command is Dwindle-only and is
  # rejected on a Niri workspace, so fall back to the matching span change.
  resize = dwindle: niri: "${omniwmctl} command resize ${dwindle} || ${omniwmctl} command ${niri}";
in
{
  imports = [
    ../modules/settings.nix
  ];

  config = lib.mkIf (config.settings.darwinWindowManager == "omniwm") {
    home.packages = [ omniwm ];

    launchd.agents.omniwm = {
      enable = true;
      config = {
        ProgramArguments = [ "${omniwm}/Applications/OmniWM.app/Contents/MacOS/OmniWM" ];
        ProcessType = "Interactive";
        # Restart after a crash, but stay down when quit from the menu bar
        KeepAlive = {
          SuccessfulExit = false;
        };
        RunAtLoad = true;
      };
    };

    # OmniWM writes Settings-window changes back to settings.toml, so it gets
    # a writable copy rather than a store symlink. The copy is only replaced
    # when the generated file changes; whatever it held is kept as
    # settings.toml.backup. The deployed version stays linked under
    # ~/.local/state/omniwm-nix for diffing.
    home.activation.omniwmSettings =
      lib.hm.dag.entryBetween [ "setupLaunchAgents" ] [ "writeBoundary" ]
        ''
          target="${config.xdg.configHome}/omniwm/settings.toml"
          deployed="${config.xdg.stateHome}/omniwm-nix/settings.toml"
          if [[ ! -f "$target" || "$(readlink "$deployed")" != "${settingsFile}" ]]; then
            run mkdir -p "$(dirname "$target")" "$(dirname "$deployed")"
            if [[ -f "$target" ]] && ! cmp -s "$target" "${settingsFile}"; then
              run cp "$target" "$target.backup"
            fi
            run install -m 0644 "${settingsFile}" "$target"
            run ln -sfn "${settingsFile}" "$deployed"
          fi
        '';

    # Bindings OmniWM hotkeys can't express: launching commands, chaining
    # commands (alt-f), and modes (alt-r). AeroSpace's alt-esc VM passthrough
    # mode has no equivalent, since OmniWM hotkeys can't be paused.
    services.skhd = {
      enable = true;
      config = ''
        # Ghostty is installed via Homebrew cask (darwin/homebrew.nix), not nix
        alt - return : /Applications/Ghostty.app/Contents/MacOS/ghostty
        alt + shift - n : osascript -e 'tell app "Terminal"' -e 'activate' -e 'do script "sudo darwin-rebuild switch; read -s -k \\?COMPLETE ; exit"' -e 'end tell'
        alt - n : open -na "Google Chrome" --args --new-window
        alt + shift - e : ${pkgs.emacs}/bin/emacsclient -c -e "(x-focus-frame nil)"
        alt - o : /Applications/Albert.app/Contents/MacOS/Albert show "clipboard "
        alt - p : /Applications/Albert.app/Contents/MacOS/Albert show "apps "
        alt + shift - x : ${pkgs.flameshot}/bin/flameshot gui
        hyper - l : pmset displaysleepnow

        alt - f : ${omniwmctl} command toggle-focused-window-floating; osascript ${zoomKeepOnTop}

        :: resize
        alt - r ; resize
        resize < h : ${resize "horizontal shrink" "set-container-primary-span -150"}
        resize < j : ${resize "vertical grow" "set-window-secondary-span +150"}
        resize < k : ${resize "vertical shrink" "set-window-secondary-span -150"}
        resize < l : ${resize "horizontal grow" "set-container-primary-span +150"}
        resize < return ; default
        resize < escape ; default
      '';
    };
  };
}
