{ config, lib, pkgs, modulesPath, ... }:
let
  dpi = 96;
  mod = "Mod4";
  display = ":0";
  lxcExec = cmd: "exec lxc exec nixos -- /run/current-system/sw/bin/zsh -c \"tail -f /dev/null | machinectl shell --uid=${config.settings.username} .host /run/current-system/sw/bin/zsh -i -c '${cmd}'\"";
  xkbFile = ../xkb + "/${config.settings.xkbFile}.xkb";
  compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${xkbFile} $out
  '';
in
{
  imports = [
    ../nixos/configuration.nix
    ../modules/settings.nix
    ../hardware/lxc.nix
  ];

  settings = {
    vm = true;
    username = "jwaggoner";
    email = "jwaggoner@drwholdings.com";
    fontSize = 14.0;
    profile = "drw";
    xkbFile = "ubuntu-thinkpad";
  };

  # For VPN reasons, this container shares the host network namespace
  # so its resolv.conf needs to match the host.
  #
  # I wanted to bind-mount /etc/resolv.conf from the host to the guest
  # but on Ubuntu the systemd-resolved service makes /etc/resolv.conf
  # a symlink (which is fine on its own) that the VPN subsequently
  # blows away with a flat file. So the bind mount follows the symlink
  # when off the VPN and does not "see" the VPN-provided file when the
  # VPN is later launched, undermining the purpose of using the mount
  # at all.
  networking.hostName = "drw";
  networking.domain = "us.drwholdings.com";
  networking.search = ["us.drwholdings.com" "drwholdings.com" "lan"];
  networking.firewall.enable = false;
  # Hardcoding public DNS first so that it doesn't matter whether the
  # host is on the VPN or not.
  networking.nameservers = ["192.168.1.1" "10.64.16.15" "10.64.16.16"];
  services.openssh.enable = false;

  # These are bind-mounted from the host. These are not checked in to
  # this repo for safety reasons, so these are only references to
  # files that are natively shipped on the laptop.
  security.pki.certificateFiles = [
    /etc/drw-security-certs/firewall-ca.crt
    /etc/drw-security-certs/inspect-ca.crt
    /etc/drw-security-certs/security-root-ca.crt
    /etc/drw-certs/drw-root-062525B6.pem
    /etc/drw-certs/drw-root-062C627A.pem
    /etc/drw-certs/drw-sub-ca-000000F1.pem
    /etc/drw-certs/drw-sub-ca-0000020C.pem
    /etc/drw-certs/drw-sub-ca-00000223.pem
    /etc/drw-certs/iss-ca.pem
    /etc/drw-certs/iss-root-ca.pem
    /etc/drw-certs/ssdns-ca.pem
    /etc/drw-certs/stem-ca.pem
  ];

  users.users.${config.settings.username}.uid = lib.mkForce 34098;

  services.xserver.dpi = dpi;

  home-manager.users.${config.settings.username} = {
    home.pointerCursor.size = 64;
    xresources.properties."Xft.dpi" = dpi;
    # startx does not use .xsession but all the contents are valid for
    # an .xinitrc
    xsession.scriptPath = ".xinitrc";

    home.packages = with pkgs; [
      google-chrome
    ];

    # Use the host's xsession
    home.sessionVariables = {
      DISPLAY = display;
    };

    # Not using a display manager, even startx, so have to do this manually
    systemd.user.services.xrdb = {
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        StandardOutput = "journal";
        Environment = "DISPLAY=${display}";
        ExecStart = "${pkgs.xorg.xrdb}/bin/xrdb -merge %h/.Xresources";
      };
      Install.WantedBy = ["default.target"];
      Install.RequiredBy = ["emacs.service"];
    };

    xsession.windowManager.i3.config.bars = lib.mkForce [
      {
        id = "bar-0";
        position = "top";
        hiddenState = "hide";
        # These are the only two commands that differ
        statusCommand = "i3status";
        command = "i3bar";
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
            bindsym button4 nop
            bindsym button5 nop
          '';
      }
    ];

    # This is where my setup descends into madness.
    #
    # This LXC container is hosted on an Ubuntu laptop since Ubuntu is
    # officially supported by the laptop vendor. However, rather than
    # run a separate X session from this container or run X in a
    # "window" on the host (e.g., VNC or similar), I'm running a
    # single i3 WM on the host with this i3 config symlinked from
    # within the guest back to the host. This approach allows me to
    # version the i3 config together with everything else in the guest
    # even though this portion of the config applies mainly to the
    # host.
    #
    # As a consequence of this approach, the i3 keybindings that
    # normally launch things within the same machine are now reaching
    # across the host->guest boundary and launching applications
    # through `lxc exec`. This works because the shared network
    # exposes the Xorg socket to the guest and I have a DISPLAY=:0
    # environment variable defined so that X-based applications can
    # launch within the host-controlled X session. This still isn't
    # trivial, however, because LXC's usual ways of logging in do not
    # give you a full-blown session (with, say, dbus running, XDG_*
    # defined, systemctl --user working, etc.) which is why everything
    # launches within a `machinectl shell` that is forced to have
    # stdin to simulate being interactive.
    #
    # While clunky, the end result is that I have a single i3 instance
    # that can seamlessly launch applications on the host or
    # applications in the guest, allowing me to retain all the
    # benefits of work/vendor application/OS stability but still keep
    # my own OS and associated tooling.
    xsession.windowManager.i3.config.keybindings = let
      pactl = cmd: "exec --no-startup-id pactl ${cmd} && $refresh_i3status";
    in {
      "${mod}+p"               = lib.mkForce(lxcExec("dmenu_path | dmenu -fn \'${config.settings.fontName}-${toString (config.settings.fontSize + 2)}\' -nb \\#fdf6e3 -nf \\#657b83 -sb \\#eee8d5 -sf \\#cb4b16 | zsh"));
      "${mod}+Shift+P"         = lib.mkForce "exec dmenu_run -fn '${config.settings.fontName}-${toString (config.settings.fontSize + 2)}' -nb \\#fdf6e3 -nf \\#657b83 -sb \\#eee8d5 -sf \\#cb4b16";
      "${mod}+o"               = lib.mkForce "exec clipmenu -fn '${config.settings.fontName}-${toString (config.settings.fontSize + 2)}' -nb \\#fdf6e3 -nf \\#657b83 -sb \\#eee8d5 -sf \\#cb4b16";
      "${mod}+g"               = "exec rofi -show window";
      "${mod}+Return"          = lib.mkForce(lxcExec(config.settings.terminal));
      "${mod}+Shift+Return"    = lib.mkForce "exec gnome-terminal";
      "${mod}+Shift+e"         = lib.mkForce(lxcExec("TERM=alacritty emacsclient -c"));
      "${mod}+a"               = lib.mkForce "exec zoom-mute-toggle";
      "${mod}+Shift+Control+L" = "exec i3lock -c 000000";
      "XF86AudioRaiseVolume"   = pactl("set-sink-volume @DEFAULT_SINK@ +5%");
      "XF86AudioLowerVolume"   = pactl("set-sink-volume @DEFAULT_SINK@ -5%");
      "XF86AudioMute"          = pactl("set-sink-mute @DEFAULT_SINK@ toggle");
      "XF86AudioMicMute"       = pactl("set-source-mute @DEFAULT_SOURCE@ toggle");
      "XF86MonBrightnessUp"    = "exec brightnessctl set 5%+";
      "XF86MonBrightnessDown"  = "exec brightnessctl set 5%-";
      "XF86AudioPlay"          = "exec playerctl play-pause";
      "XF86AudioPrev"          = "exec playerctl previous";
      "XF86AudioNext"          = "exec playerctl next";
      "XF86Favorites"          = "floating toggle, sticky toggle, resize set 30 ppt 40 ppt, move position 70 ppt 3 ppt"; # for zoom
      "KP_Multiply"            = "floating toggle, sticky toggle, resize set 30 ppt 40 ppt, move position 70 ppt 3 ppt"; # for zoom
      "XF86Display"            = "exec swap-display";
      "${mod}+Print"           = "exec flameshot gui";
      "${mod}+Shift+Print"     = "exec flameshot gui";
      "XF86Calculator"         = "exec flameshot gui"; # external keyboard
      "${mod}+Shift+N"         = lib.mkForce ''
        exec "lxc exec nixos -- /run/current-system/sw/bin/zsh -c 'tail -f /dev/null | machinectl shell --uid=${config.settings.username} .host /run/current-system/sw/bin/zsh -i -c \\"xterm -e \\\\"sudo nixos-rebuild switch; read -s -k \\?COMPLETE\\\\"\\"'"
      '';
    };

    # There is no ~/.xinitrc on Ubuntu and the NixOS one links to
    # binaries in /nix/store that won't work on Ubuntu. So I mirror
    # stuff I would normally put in there as i3 exec commands.
    xsession.windowManager.i3.extraConfig =
      # Somehow ibus is getting autostarted on Ubuntu and overriding
      # this, so I can to manually go into ibus-setup -> settings ->
      # advanced -> use system keyboard layout so it would leave my xkb
      # settings alone.
      ''
        exec_always xkbcomp ${compiledLayout} ${display}
        ${lxcExec("xrdb -merge $HOME/.Xresources")}
        exec --no-startup-id nm-applet
        exec --no-startup-id blueman-applet
        exec --no-startup-id pasystray
        exec --no-startup-id systemctl --user start clipmenud.service
      '';

    programs.i3status.modules = {
      "volume master" = {
        position = 1;
        settings = {
          format = "🎵%volume";
          format_muted = "🔇0%%";
        };
      };
      "path_exists VPN" = {
        position = 2;
        settings = {
          format = "%title";
          format_down = "%title: down";
          path = "/sys/class/net/cscotun0";
        };
      };
      "wireless _first_" = {
        position = 3;
        settings = {
          format_up = "📶%ip";
          format_down = "📶down";
        };
      };
      "battery all" = {
        position = 4;
        settings = {
          format = "%status%percentage %remaining";
	        format_down = "No battery";
          status_chr = "⚡";
          status_bat = "🔋";
          last_full_capacity = true;
          integer_battery_capacity = true;
        };
      };
      "read_file power_profile" = {
        position = 5;
        settings = {
          format = "%content";
          path = "/sys/firmware/acpi/platform_profile";
        };
      };
    };

    # For now, I only use one monitor at a time, so allow swapping between them
    home.file."swap-display" = {
      target = "bin/swap-display";
      executable = true;
      text = ''
      #!/usr/bin/env bash
      set -e

      INTERNAL="eDP-1"
      EXTERNAL=$(xrandr | grep " connected " | grep -v $INTERNAL | awk '{print $1}' | head -1)
      if [ -z $EXTERNAL ] ; then
          xrandr --output $INTERNAL --auto --primary
          >&2 echo "External display not found"
          exit 1
      fi
      INTERNAL_TOGGLE=$(xrandr --listactivemonitors | grep -q $INTERNAL && echo "--off" || echo "--auto --primary")
      EXTERNAL_TOGGLE=$(xrandr --listactivemonitors | grep -q $EXTERNAL && echo "--off" || echo "--auto --primary")

      if [ "$INTERNAL_TOGGLE" == "$EXTERNAL_TOGGLE" ] ; then
          >&2 echo "Both monitors are already on or off"
          exit 1
      fi

      xrandr --output $INTERNAL $INTERNAL_TOGGLE
      xrandr --output $EXTERNAL $EXTERNAL_TOGGLE

      [ -e $HOME/.background-image ] && feh --bg-scale $HOME/.background-image
      xkbcomp ${compiledLayout} ${display}
    '';
    };

    home.file."zoom-mute-toggle" = {
      target = "bin/zoom-mute-toggle";
      executable = true;
      text = ''
        #!/usr/bin/env bash
        set -e
        CURRENT=$(xdotool getwindowfocus)
        xdotool search --name '^Zoom Meeting$' \
            windowactivate --sync \
            keyup Super \
            keyup a \
            key --clearmodifiers alt+a \
            windowactivate --sync ''${CURRENT}
    '';
    };

  };
}
