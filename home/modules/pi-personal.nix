# pi with a Claude Code-like look (../pi): tool rows that collapse to a line under a ⏺ bullet, a
# status-line footer, the ❯ prompt, the ✻ spinner, and the terminal-ansi theme. The theme names
# ANSI palette slots instead of colors, so the terminal draws it from whichever solarized palette
# is active: it follows the light/dark switch even over mosh, which answers none of pi's color
# queries. Past prompts are shaded with xterm's color 255, the light gray Claude Code puts under
# them, which no theme's palette changes.
#
# Drafts work as in Claude Code: ctrl+s stashes the editor's text and brings it back, on a stack
# that alt+s (or /drafts) browses (programs.pi-personal.drafts). pi's own ctrl+s actions move to
# ctrl+alt+s, which keeps pi from warning about the clash, and ctrl+p and ctrl+n move the cursor up
# and down a line, as in Emacs, with model cycling on alt+p (programs.pi-personal.keybindings).
#
# Installs pi and the package into ~/.pi/agent, and merges the display settings and keybindings
# into pi's settings.json and keybindings.json at activation (pi writes settings.json itself from
# /settings, and both are yours to edit, so neither can be a read-only link). The package, theme,
# and settings are also options for majordomo's desk sessions, which don't read ~/.pi:
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
  keybindingsFile = pkgs.writeText "pi-personal-keybindings.json" (builtins.toJSON cfg.keybindings);
  # The package as pi loads it, with config.json from the options; tests and the type check stay out.
  source = builtins.path {
    path = ../pi;
    name = "pi-personal-source";
    filter =
      path: _type:
      !(
        lib.hasSuffix ".test.ts" path || baseNameOf path == "check.nix" || baseNameOf path == "config.json"
      );
  };
  packageConfig = pkgs.writeText "pi-personal-config.json" (
    builtins.toJSON { inherit (cfg) drafts; }
  );
  # Merges managed keys into a JSON file pi or you also write, leaving the rest alone.
  mergeJson = file: managed: ''
    file="$HOME/.pi/agent/${file}"
    run mkdir -p "$HOME/.pi/agent"
    if [[ ! -s $file ]]; then
      run cp ${managed} "$file"
      run chmod u+w "$file"
    elif merged=$(${lib.getExe pkgs.jq} -s '.[0] * .[1]' "$file" ${managed}); then
      printf '%s\n' "$merged" | run tee "$file" >/dev/null
    else
      warnEcho "pi-personal: $file isn't valid JSON; leaving it alone"
    fi
  '';
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
      default = pkgs.runCommand "pi-personal" { } ''
        cp -R ${source} "$out"
        chmod u+w "$out"
        cp ${packageConfig} "$out/config.json"
      '';
      description = "The pi package: its extensions, themes, and configuration.";
    };

    drafts = {
      keys = {
        stash = lib.mkOption {
          type = lib.types.str;
          default = "ctrl+s";
          description = "Stash the editor's text, or bring back the newest stashed draft into an empty editor, as Claude Code's ctrl+s does.";
        };
        browse = lib.mkOption {
          type = lib.types.str;
          default = "alt+s";
          description = "Open the stashed drafts, to restore or delete an older one (also /drafts).";
        };
      };
      limit = lib.mkOption {
        type = lib.types.ints.positive;
        default = 50;
        description = "Drafts kept per working directory; a stash past it drops the oldest.";
      };
      restoreAfterSubmit = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "After you send a prompt, bring back a draft you stashed earlier in the same pi session, as Claude Code does.";
      };
    };

    keybindings = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.str (lib.types.listOf lib.types.str));
      default = {
        # pi's ctrl+s actions, out of the drafts' way.
        "app.thinking.save" = "ctrl+alt+s";
        "app.models.save" = "ctrl+alt+s";
        "app.session.toggleSort" = "ctrl+alt+s";
        # Emacs's line motion; model cycling leaves ctrl+p for alt+p, which crosses mosh as ESC p.
        "tui.editor.cursorUp" = [
          "up"
          "ctrl+p"
        ];
        "tui.editor.cursorDown" = [
          "down"
          "ctrl+n"
        ];
        "app.model.cycleForward" = "alt+p";
        "app.model.cycleBackward" = "alt+shift+p";
      };
      description = "pi keybindings merged into ~/.pi/agent/keybindings.json; other keys there are left alone.";
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

    home.activation.piPersonalSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] (
      mergeJson "settings.json" settingsFile + mergeJson "keybindings.json" keybindingsFile
    );
  };
}
