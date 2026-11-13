# pi with a Claude Code-like look (../pi): tool rows that collapse to a line under a ⏺ bullet, a
# status-line footer, the ❯ prompt, the ✻ spinner, and the terminal-ansi theme. The theme names
# ANSI palette slots instead of colors, so the terminal draws it from whichever solarized palette
# is active: it follows the light/dark switch even over mosh, which answers none of pi's color
# queries. Past prompts are shaded with xterm's color 255, the light gray Claude Code puts under
# them, which no theme's palette changes.
#
# Installs pi and the package into ~/.pi/agent, and merges the display settings into pi's
# settings.json at activation (pi writes the file itself from /settings, so it can't be a
# read-only link). The package, theme, and settings are also options for majordomo's desk
# sessions, which don't read ~/.pi:
#
#   programs.majordomo.pi.ui = {
#     packages = [ config.programs.pi-personal.package ];
#     inherit (config.programs.pi-personal) theme settings;
#   };
{ inputs, ... }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.pi-personal;
  system = pkgs.stdenv.hostPlatform.system;
  settingsFile = pkgs.writeText "pi-personal-settings.json" (builtins.toJSON cfg.settings);
in
{
  options.programs.pi-personal = {
    enable = lib.mkEnableOption "pi with a Claude Code-like look";

    installPi = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install pi from llm-agents.";
    };

    package = lib.mkOption {
      type = lib.types.path;
      readOnly = true;
      default = builtins.path {
        path = ../pi;
        name = "pi-personal";
        # Tests and the type check aren't part of what pi loads.
        filter = path: _type: !(lib.hasSuffix ".test.ts" path || baseNameOf path == "check.nix");
      };
      description = "The pi package: its extensions and themes.";
    };

    theme = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "terminal-ansi";
      description = "The package's theme.";
    };

    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = {
        inherit (cfg) theme;
        # The package draws its own header; this hides pi's list of loaded resources.
        quietStartup = "header";
        # Thinking collapses to "✻ Thinking…"; ctrl+t shows it.
        hideThinkingBlock = true;
      };
      description = "Display settings merged into ~/.pi/agent/settings.json; other keys there are left alone.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.optional cfg.installPi inputs.llm-agents.packages.${system}.pi;

    # pi discovers a package directory under extensions/ by its manifest; themes load from themes/,
    # which pi also watches for changes.
    home.file.".pi/agent/extensions/pi-personal".source = cfg.package;
    home.file.".pi/agent/themes/${cfg.theme}.json".source = "${cfg.package}/themes/${cfg.theme}.json";

    home.activation.piPersonalSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      settings="$HOME/.pi/agent/settings.json"
      run mkdir -p "$HOME/.pi/agent"
      if [[ ! -s $settings ]]; then
        run cp ${settingsFile} "$settings"
        run chmod u+w "$settings"
      elif merged=$(${lib.getExe pkgs.jq} -s '.[0] * .[1]' "$settings" ${settingsFile}); then
        printf '%s\n' "$merged" | run tee "$settings" >/dev/null
      else
        warnEcho "pi-personal: $settings isn't valid JSON; leaving it alone"
      fi
    '';
  };
}
