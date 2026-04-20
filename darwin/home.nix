{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../config/home-osconfig-bridge.nix
    ../config/home.nix
    ../config/home-dev.nix
    ../config/terminal.nix
    ./wm.nix
    ./xwm.nix
    ./audio.nix
  ];

  home.packages = with pkgs; [
    gnupg
    nixos-anywhere
    nixos-rebuild
    rsync
  ];

  programs.vim = {
    enable = true;
    defaultEditor = false;
    packageConfigurable = pkgs.vim;
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

  # Build a minimal AppleScript .app that wraps emacsclient, then register
  # it as the default plain-text handler.  This lets Ghostty's
  # write_scrollback_file:open open scrollback in a new Emacs GUI frame
  # (via the running daemon) instead of launching TextEdit.
  home.activation.emacs-text-handler = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    EMACSCLIENT="${config.programs.emacs.finalPackage}/bin/emacsclient"
    APP_DIR="$HOME/Applications/EmacsClient.app"
    PLIST="$APP_DIR/Contents/Info.plist"
    LSREGISTER="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister"

    rm -rf "$APP_DIR"
    /usr/bin/osacompile -o "$APP_DIR" -e "
      on open theFiles
        repeat with aFile in theFiles
          do shell script \"$EMACSCLIENT -n -c \" & quoted form of POSIX path of aFile
        end repeat
      end open"

    # Stable bundle ID, text-file handler declaration, no Dock icon
    /usr/libexec/PlistBuddy \
      -c "Set :CFBundleIdentifier org.gnu.EmacsClient" \
      -c "Add :CFBundleDocumentTypes array" \
      -c "Add :CFBundleDocumentTypes:0 dict" \
      -c "Add :CFBundleDocumentTypes:0:CFBundleTypeRole string Editor" \
      -c "Add :CFBundleDocumentTypes:0:LSItemContentTypes array" \
      -c "Add :CFBundleDocumentTypes:0:LSItemContentTypes:0 string public.plain-text" \
      -c "Add :LSUIElement bool true" \
      "$PLIST"

    "$LSREGISTER" -f "$APP_DIR"
    ${pkgs.duti}/bin/duti -s org.gnu.EmacsClient public.plain-text all
  '';

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
