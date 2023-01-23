{ config, lib, pkgs, modulesPath, ... }:
let
  dpi = 96;
  mod = "Mod4";
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

    # Use the host's xsession
    home.sessionVariables = {
      DISPLAY = ":0";
    };

    # Not using a display manager, even startx, so have to do this manually
    systemd.user.services.xrdb = {
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        StandardOutput = "journal";
        Environment = "DISPLAY=:0";
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
    xsession.windowManager.i3.config.keybindings."${mod}+p" = lib.mkForce "exec lxc exec nixos -- /run/current-system/sw/bin/zsh -c \"tail -f /dev/null | machinectl shell --uid=jwaggoner .host /run/current-system/sw/bin/zsh -i -c 'dmenu_path | dmenu | zsh'\"";
    xsession.windowManager.i3.config.keybindings."${mod}+Shift+P" = lib.mkForce  "exec dmenu_run";
    xsession.windowManager.i3.config.keybindings."${mod}+o" = lib.mkForce "exec lxc exec nixos -- /run/current-system/sw/bin/zsh -c \"tail -f /dev/null | machinectl shell --uid=jwaggoner .host /run/current-system/sw/bin/zsh -i -c clipmenu\"";
    xsession.windowManager.i3.config.keybindings."${mod}+Return" = lib.mkForce "exec lxc exec nixos -- /run/current-system/sw/bin/zsh -c \"tail -f /dev/null | machinectl shell --uid=jwaggoner .host /run/current-system/sw/bin/zsh -i -c ${config.settings.terminal}\"";
    xsession.windowManager.i3.config.keybindings."${mod}+Shift+Return" = lib.mkForce "exec gnome-terminal";
    xsession.windowManager.i3.config.keybindings."${mod}+Shift+e" = lib.mkForce "exec lxc exec nixos -- /run/current-system/sw/bin/zsh -c \"tail -f /dev/null | machinectl shell --uid=jwaggoner .host /run/current-system/sw/bin/zsh -i -c 'TERM=alacritty emacsclient -c'\"";
    xsession.windowManager.i3.config.keybindings."${mod}+Shift+N" = lib.mkForce ''
              exec "lxc exec nixos -- /run/current-system/sw/bin/zsh -c 'tail -f /dev/null | machinectl shell --uid=jwaggoner .host /run/current-system/sw/bin/zsh -i -c \\"xterm -e \\\\"sudo nixos-rebuild switch; read -s -k \\?COMPLETE\\\\"\\"'"
            '';
    xsession.windowManager.i3.config.keybindings."${mod}+Shift+Control+L" = "exec i3lock";
    xsession.windowManager.i3.config.keybindings."XF86AudioRaiseVolume" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status";
    xsession.windowManager.i3.config.keybindings."XF86AudioLowerVolume" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status";
    xsession.windowManager.i3.config.keybindings."XF86AudioMute" = "exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status";
    xsession.windowManager.i3.config.keybindings."XF86AudioMicMute" = "exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status";
    xsession.windowManager.i3.config.keybindings."XF86MonBrightnessUp" = "exec brightnessctl set 5%+";
    xsession.windowManager.i3.config.keybindings."XF86MonBrightnessDown" = "exec brightnessctl set 5%-";
    xsession.windowManager.i3.config.keybindings."${mod}+Print" = "exec flameshot gui";
    xsession.windowManager.i3.config.keybindings."${mod}+Shift+Print" = "exec flameshot gui";


    xsession.windowManager.i3.extraConfig = let
      xkbFile = ../xkb + "/${config.settings.xkbFile}.xkb";
      compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${xkbFile} $out
       '';
    in
      ''
        exec xkbcomp ${compiledLayout} $DISPLAY
        exec lxc exec nixos -- /run/current-system/sw/bin/zsh -c \"tail -f /dev/null | machinectl shell --uid=jwaggoner .host /run/current-system/sw/bin/zsh -i -c 'xrdb -merge $HOME/.Xresources'\"
        exec --no-startup-id nm-applet
        exec --no-startup-id blueman-applet
        exec --no-startup-id pasystray
      '';

    programs.i3status.modules = {
      "volume master" = {
        position = 1;
        settings = {
          format = "♪ %volume";
          format_muted = "♪ 0%%";
        };
      };
      "wireless _first_" = {
        position = 2;
        settings = {
          format_up = "W: %essid %quality %ip";
          format_down = "W: down";
        };
      };
      "battery all" = {
        position = 4;
        settings = {
          format = "%status %percentage %remaining";
	        format_down = "⚡";
        };
      };
    };
  };
}
