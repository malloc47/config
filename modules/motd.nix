{
  config,
  pkgs,
  lib,
  ...
}:

with lib;

let
  cfg = config.motd;

  # 2-row block font using ▄▀█ half-block characters
  font = {
    A = {
      r1 = "▄▀█";
      r2 = "█▀█";
    };
    B = {
      r1 = "█▀▄";
      r2 = "█▄█";
    };
    C = {
      r1 = "▄▀▀";
      r2 = "▀▄▄";
    };
    D = {
      r1 = "█▀▄";
      r2 = "█▄▀";
    };
    E = {
      r1 = "█▀▀";
      r2 = "██▄";
    };
    F = {
      r1 = "█▀▀";
      r2 = "█▀ ";
    };
    G = {
      r1 = "▄▀▀";
      r2 = "▀▄█";
    };
    H = {
      r1 = "█ █";
      r2 = "█▀█";
    };
    I = {
      r1 = "█";
      r2 = "█";
    };
    J = {
      r1 = "  █";
      r2 = "▀▄▀";
    };
    K = {
      r1 = "█▀▄";
      r2 = "█▀▄";
    };
    L = {
      r1 = "█  ";
      r2 = "█▄▄";
    };
    M = {
      r1 = "█▄ ▄█";
      r2 = "█ ▀ █";
    };
    N = {
      r1 = "█▀█";
      r2 = "█ █";
    };
    O = {
      r1 = "█▀█";
      r2 = "█▄█";
    };
    P = {
      r1 = "█▀█";
      r2 = "█▀ ";
    };
    Q = {
      r1 = "█▀█";
      r2 = "█▄▄";
    };
    R = {
      r1 = "█▀█";
      r2 = "█▀▄";
    };
    S = {
      r1 = "▄▀▀";
      r2 = "▀▀█";
    };
    T = {
      r1 = "▀█▀";
      r2 = " █ ";
    };
    U = {
      r1 = "█ █";
      r2 = "█▄█";
    };
    V = {
      r1 = "█ █";
      r2 = "▀▄▀";
    };
    W = {
      r1 = "█ ▄ █";
      r2 = "▀█ █▀";
    };
    X = {
      r1 = "▀▄▀";
      r2 = "▄▀▄";
    };
    Y = {
      r1 = "█ █";
      r2 = " █ ";
    };
    Z = {
      r1 = "▀▀█";
      r2 = "█▄▄";
    };
  };

  chars = stringToCharacters (toUpper config.networking.hostName);
  nameLine1 = concatStringsSep " " (map (c: font.${c}.r1) chars);
  nameLine2 = concatStringsSep " " (map (c: font.${c}.r2) chars);

  specsStr = concatStringsSep " · " cfg.specs;
  tagsStr = concatStringsSep " · " cfg.tags;

  motd = pkgs.writeShellScript "motd" ''
    # Dynamic stats
    uptime_secs=$(awk '{print int($1)}' /proc/uptime)
    uptime_days=$((uptime_secs / 86400))
    uptime_hours=$(( (uptime_secs % 86400) / 3600 ))
    if [ "$uptime_days" -gt 0 ]; then
      uptime_short="''${uptime_days}d ''${uptime_hours}h"
    else
      uptime_mins=$(( (uptime_secs % 3600) / 60 ))
      uptime_short="''${uptime_hours}h ''${uptime_mins}m"
    fi
    load=$(awk '{print $1}' /proc/loadavg)
    disk=$(df -h / | awk 'NR==2 {print $5}')
    ip=$(hostname -I | awk '{print $1}')

    # Content lines
    line1="${nameLine1}   ${cfg.hardware}"
    line2="${nameLine2}   ${specsStr}"
    line3="${tagsStr}"
    line4="''${ip} · up ''${uptime_short} · load ''${load} · ''${disk}"

    # Find max display width across all lines
    max=0
    for line in "$line1" "$line2" "$line3" "$line4"; do
      w=$(echo -n "$line" | wc -m)
      [ "$w" -gt "$max" ] && max=$w
    done
    width=$((max + 2))

    # Print a content line, padded to $width display chars
    print_line() {
      local display_len
      display_len=$(echo -n "$1" | wc -m)
      local pad=$((width - display_len))
      printf " │  %s%''${pad}s│\n" "$1" ""
    }

    # Border
    border=""
    for i in $(seq 1 $((width + 2))); do border="''${border}─"; done

    echo " ┌''${border}┐"
    print_line "$line1"
    print_line "$line2"
    print_line "$line3"
    print_line "$line4"
    echo " └''${border}┘"
  '';
in
{
  options.motd = {
    enable = mkEnableOption "dynamic SSH login banner";
    hardware = mkOption {
      type = types.str;
      description = "Hardware description";
      example = "gmktec g10 · mini pc";
    };
    specs = mkOption {
      type = types.listOf types.str;
      description = "Hardware specs (joined with · separator)";
      example = [
        "8 cores"
        "12GB"
        "512GB"
      ];
    };
    tags = mkOption {
      type = types.listOf types.str;
      description = "Role/service tags (joined with · separator)";
      example = [
        "dns"
        "proxy"
        "auth"
        "dashboard"
      ];
    };
  };

  config = mkIf cfg.enable {
    users.motd = "";
    services.openssh.settings.PrintLastLog = false;
    environment.loginShellInit = "${motd}";
  };
}
