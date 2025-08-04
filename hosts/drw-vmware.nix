{ config, lib, pkgs, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/virtualisation/vmware-guest.nix")
    ../modules/settings.nix
  ];

  settings = {
    vm = true;
    username = "jwaggoner";
    email = "jwaggoner@drwholdings.com";
    fontName = "Roboto Mono";
    fontSize = 9.0;
    profile = "drw";
    dpi = 254;
  };

  # For VPN reasons, this container shares the host network namespace
  # so its resolv.conf needs to match the host.
  networking.domain = "us.drwholdings.com";
  networking.search = ["us.drwholdings.com" "drwholdings.com" "lan"];
  networking.firewall.enable = false;
  # Hardcoding public DNS first so that it doesn't matter whether the
  # host is on the VPN or not.
  networking.nameservers = ["172.16.204.2" "10.64.16.15" "10.64.16.16"];

  security.pki.certificateFiles = [/etc/ssl/ca-bundle.pem];

  home-manager.users.${config.settings.username} = {
    home.packages = with pkgs; [
      argo
      dbeaver-bin
      gh
      kubectl
      maven
      remmina
      saml2aws
    ];

    xsession.windowManager.i3.config.keybindings = let
      mod = "Mod4";
    in
    {
      "${mod}+Shift+N" = lib.mkForce
        "exec \"xterm -e 'nixos-rebuild switch --use-remote-sudo --impure; read -s -k \\?COMPLETE'\"";

      "${mod}+v" = lib.mkForce "exec xrandr --output Virtual-1 --auto";

      "${mod}+Shift+Escape" = "mode window";
    };

    xsession.windowManager.i3.config.modes.window = let
      mod = "Mod4";
    in
    {
      "${mod}+Control+j" = "focus down";
      "${mod}+Control+k" = "focus up";
      "${mod}+Control+l" = "focus right";
      "${mod}+Control+h" = "focus left";

      # These currently conflict with the host bindings for merging windows
      "${mod}+Control+Shift+J" = "move down";
      "${mod}+Control++Shift+K" = "move up";
      "${mod}+Control++Shift+L" = "move right";
      "${mod}+Control++Shift+H" = "move left";

      "${mod}+Control+1" = "workspace number 1";
      "${mod}+Control+2" = "workspace number 2";
      "${mod}+Control+3" = "workspace number 3";
      "${mod}+Control+4" = "workspace number 4";
      "${mod}+Control+5" = "workspace number 5";
      "${mod}+Control+6" = "workspace number 6";
      "${mod}+Control+7" = "workspace number 7";
      "${mod}+Control+8" = "workspace number 8";
      "${mod}+Control+9" = "workspace number 9";
      "${mod}+Control+0" = "workspace number 10";
      "${mod}+Control+Shift+1" = "move container to workspace number 1";
      "${mod}+Control+Shift+2" = "move container to workspace number 2";
      "${mod}+Control+Shift+3" = "move container to workspace number 3";
      "${mod}+Control+Shift+4" = "move container to workspace number 4";
      "${mod}+Control+Shift+5" = "move container to workspace number 5";
      "${mod}+Control+Shift+6" = "move container to workspace number 6";
      "${mod}+Control+Shift+7" = "move container to workspace number 7";
      "${mod}+Control+Shift+8" = "move container to workspace number 8";
      "${mod}+Control+Shift+9" = "move container to workspace number 9";
      "${mod}+Control+Shift+0" = "move container to workspace number 10";

      "${mod}+v" = "exec xrandr --output Virtual-1 --auto";

      "${mod}+Shift+Escape" = "mode default";

    };

    # Hack around https://github.com/NixOS/nixpkgs/issues/396378
    systemd.user.services."xrandr-preferred" = {
      Service = {
        ExecStart = "${pkgs.writeShellScript "xrandr-preferred.sh" ''
          set -xeuf -o pipefail
          if ! ${pkgs.xorg.xrandr}/bin/xrandr | grep -q '*+'; then
            ${pkgs.xorg.xrandr}/bin/xrandr --output Virtual-1 --preferred
          fi
          ''}";
        Type = "oneshot";
      };
      Unit = {
        StartLimitIntervalSec = "50ms";
        StartLimitBurst = 1;
      };
    };

    systemd.user.timers."xrandr-poll-for-changes" = {
      Install.WantedBy = [ "timers.target" ];
      Timer = {
        Unit = "xrandr-preferred.service";

        AccuracySec = "100ms";
        OnActiveSec = "30s";
        OnUnitActiveSec = "200ms";
      };
    };

  };
}
