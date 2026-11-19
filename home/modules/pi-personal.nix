# pi with a Claude Code-like look (../pi): tool rows that collapse to a line under a ⏺ bullet, a
# status-line footer, the ❯ prompt, the ✻ spinner, and the terminal-ansi theme. The theme names
# ANSI palette slots instead of colors, so the terminal draws it from whichever solarized palette
# is active: it follows the light/dark switch even over mosh, which answers none of pi's color
# queries. Past prompts are shaded with xterm's color 255, the light gray Claude Code puts under
# them, which no theme's palette changes.
#
# Drafts work as in Claude Code: ctrl+s stashes the editor's text and brings it back, on a stack
# that alt+s (or /drafts) browses (programs.pi-personal.drafts). The prompt takes Claude Code's
# keys too (programs.pi-personal.editor): ctrl+l clears it, a backslash before Enter makes a
# newline, and ctrl+r searches the prompts you've sent. pi's own keys move to match
# (programs.pi-personal.keybindings): the model picker to alt+p, as in Claude Code, model cycling
# off, ctrl+p and ctrl+n to Emacs line motion, and pi's ctrl+s, ctrl+l, and ctrl+r actions in its
# pickers to ctrl+alt+, which keeps pi from warning about clashes.
#
# A run of tool calls draws as one line, "⏺ Tools: Slack(read channel), Outlook(list mail
# messages)", whose items open their own rows when clicked (programs.pi-personal.toolGroups: every
# MCP tool by default, with names for MCP servers).
#
# Installs pi and the package into ~/.pi/agent, and merges the display settings and keybindings
# into pi's settings.json and keybindings.json at activation (pi writes settings.json itself from
# /settings, and both are yours to edit, so neither can be a read-only link). A model provider is
# set up the same way: `packages` are linked into ~/.pi/agent/extensions/, `models` is merged into
# models.json, and `keyFile` has pi export its key as `keyEnv`. The package, theme, and settings
# are also options for majordomo's desk sessions, which don't read ~/.pi:
#
#   programs.majordomo.pi.ui = {
#     packages = [ config.programs.pi-personal.package ];
#     inherit (config.programs.pi-personal) theme settings;
#   };
#
# For a consumer that should differ from plain pi, a variant builds the package again with some of
# the configuration changed (programs.pi-personal.variants.<name>), and exposes the same attributes:
#
#   programs.pi-personal.variants.majordomo.toolGroups.include = [ "*" ];
#   programs.majordomo.pi.ui = {
#     packages = [ config.programs.pi-personal.variants.majordomo.package ];
#     inherit (config.programs.pi-personal.variants.majordomo) theme settings keybindings;
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
  # `model` is "provider/id", where the id may itself contain slashes.
  modelParts = lib.splitString "/" cfg.model;
  settingsFile = pkgs.writeText "pi-personal-settings.json" (
    builtins.toJSON (
      cfg.settings
      // lib.optionalAttrs (cfg.model != null) {
        defaultProvider = lib.head modelParts;
        defaultModel = lib.concatStringsSep "/" (lib.tail modelParts);
      }
    )
  );
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
  # The package with the config.json of a set of options: the base's, or a variant's.
  mkPackage =
    name: options:
    let
      packageConfig = pkgs.writeText "${name}-config.json" (
        builtins.toJSON { inherit (options) drafts editor toolGroups; }
      );
    in
    pkgs.runCommand name { } ''
      cp -R ${source} "$out"
      chmod u+w "$out"
      cp ${packageConfig} "$out/config.json"
    '';
  # The base's values as defaults for a variant, pushed down to each leaf: what the variant sets
  # replaces only those leaves, so an attribute set such as toolGroups.labels merges key by key.
  # (mkDefault on a whole attribute set would let one key the variant sets drop all the others.)
  leafDefaults =
    value: if builtins.isAttrs value then lib.mapAttrs (_: leafDefaults) value else lib.mkDefault value;
  keybindingsType = lib.types.attrsOf (
    lib.types.either lib.types.str (lib.types.listOf lib.types.str)
  );
  # The options that go into the package's config.json, declared once for the base and its variants.
  packageOptions = {
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

    editor = {
      keys = {
        clear = lib.mkOption {
          type = lib.types.str;
          default = "ctrl+l";
          description = "Clear the prompt, as Claude Code's ctrl+l does.";
        };
        history = lib.mkOption {
          type = lib.types.str;
          default = "ctrl+r";
          description = "Search the prompts you've sent, as Claude Code's ctrl+r does (also /history).";
        };
      };
      backslashNewline = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "A backslash right before the cursor makes Enter insert a newline, as in Claude Code.";
      };
      historyLimit = lib.mkOption {
        type = lib.types.ints.positive;
        default = 1000;
        description = "Past prompts the history search offers, newest first.";
      };
    };

    toolGroups = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Draw a run of tool calls as one line, "⏺ Tools: Slack(read channel), Outlook(list mail
          messages)", in place of a row per call. Clicking an item opens its row; clicking "Tools:"
          opens or closes them all; ctrl+o shows every row as usual.
        '';
      };
      include = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "mcp__*" ];
        example = [
          "mcp__*"
          "todo"
        ];
        description = "Globs over pi's tool names (`*` matches any run of characters) whose calls are grouped: every MCP tool by default, and none of pi's built-in tools.";
      };
      exclude = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        example = [ "mcp__github__*" ];
        description = "Globs over tool names never grouped; they win over `include`.";
      };
      labels = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
        example = {
          ms365 = "Outlook";
        };
        description = ''
          Names to show for MCP servers, by the server's name in pi's mcp__<server>__<tool> tool
          names (characters other than letters, digits, and _ become _). A server without one shows
          its name, capitalized.
        '';
      };
    };
  };
  modelsFile = (pkgs.formats.json { }).generate "pi-personal-models.json" cfg.models;
  piBase = inputs.llm-agents.packages.${system}.pi;
  # pi reads a model provider's key from the environment; export it from the file at launch, so the
  # secret never lands in the store or in every shell's environment.
  piPackage =
    if cfg.keyFile == null then
      piBase
    else
      pkgs.symlinkJoin {
        name = "${piBase.name}-with-key";
        paths = [ piBase ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/pi" --run ${lib.escapeShellArg ''
            if [ -r ${lib.escapeShellArg cfg.keyFile} ]; then
              export ${cfg.keyEnv}="$(cat ${lib.escapeShellArg cfg.keyFile})"
            fi
          ''}
        '';
        meta.mainProgram = "pi";
      };
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
  options.programs.pi-personal = packageOptions // {
    enable = lib.mkEnableOption "pi with a Claude Code-like look";

    installPi = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install pi from llm-agents.";
    };

    package = lib.mkOption {
      type = lib.types.path;
      readOnly = true;
      default = mkPackage "pi-personal" cfg;
      description = "The pi package: its extensions, themes, and configuration.";
    };

    packages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Extra pi packages, such as an organization's model provider. Each is linked into
        ~/.pi/agent/extensions/, where pi finds it by its package.json manifest.
      '';
    };

    models = lib.mkOption {
      type = (pkgs.formats.json { }).type;
      default = { };
      description = "pi's models.json (providers, and any models they list), merged into ~/.pi/agent/models.json; other keys there are left alone.";
    };

    model = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "my-gateway/claude-opus-5-5";
      description = ''
        The model pi starts on, as provider/id, written as defaultProvider and defaultModel in
        settings.json. Without it pi picks one itself, which with a gateway provider is the first
        model alphabetically.
      '';
    };

    keyEnv = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "The environment variable pi's model provider reads its API key from. Set it together with `keyFile`.";
    };

    keyFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Runtime path of a file holding the API key (an agenix secret, say). pi is wrapped to export it as `keyEnv`.";
    };

    keybindings = lib.mkOption {
      type = keybindingsType;
      default = {
        # pi's ctrl+s actions, out of the drafts' way, and its ctrl+l and ctrl+r ones in pickers out of
        # the prompt keys'.
        "app.thinking.save" = "ctrl+alt+s";
        "app.models.save" = "ctrl+alt+s";
        "app.session.toggleSort" = "ctrl+alt+s";
        "app.tree.filter.labeledOnly" = "ctrl+alt+l";
        "app.session.rename" = "ctrl+alt+r";
        # The model picker on Claude Code's alt+p (it crosses mosh as ESC p), and no model cycling,
        # which Claude Code doesn't have; /model and the picker still switch.
        "app.model.select" = "alt+p";
        "app.model.cycleForward" = [ ];
        "app.model.cycleBackward" = [ ];
        # Emacs's line motion, as in Claude Code's prompt.
        "tui.editor.cursorUp" = [
          "up"
          "ctrl+p"
        ];
        "tui.editor.cursorDown" = [
          "down"
          "ctrl+n"
        ];
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

    variants = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, config, ... }:
          {
            options = packageOptions // {
              settings = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                description = "Display settings for this variant's consumer; the base's `settings`, key by key, except those set here.";
              };
              keybindings = lib.mkOption {
                type = keybindingsType;
                description = "Keybindings for this variant's consumer; the base's `keybindings`, key by key, except those set here.";
              };
              theme = lib.mkOption {
                type = lib.types.str;
                readOnly = true;
                default = cfg.theme;
                description = "The package's theme, as the base's.";
              };
              package = lib.mkOption {
                type = lib.types.path;
                readOnly = true;
                description = "The pi package built with this variant's configuration.";
              };
            };
            # Everything not set in the variant is the base's: lists such as toolGroups.include
            # replace the base's, and attribute sets such as toolGroups.labels and settings merge
            # with it key by key.
            config = {
              drafts = leafDefaults cfg.drafts;
              editor = leafDefaults cfg.editor;
              toolGroups = leafDefaults cfg.toolGroups;
              # Settings and keybindings merge by their top-level keys; a key's value is replaced whole.
              settings = lib.mapAttrs (_: lib.mkDefault) cfg.settings;
              keybindings = lib.mapAttrs (_: lib.mkDefault) cfg.keybindings;
              package = mkPackage "pi-personal-${name}" config;
            };
          }
        )
      );
      default = { };
      example = lib.literalExpression ''
        {
          majordomo.toolGroups = {
            include = [ "*" ];
            exclude = [ "todo" ];
          };
        }
      '';
      description = ''
        Other builds of the package for other consumers, such as majordomo's desk sessions: each
        variant takes the base's configuration (drafts, editor, toolGroups, settings, keybindings)
        and changes what it sets, and exposes its own `package`, `settings`, `keybindings`, and
        `theme`. Only the base is installed into ~/.pi; a variant is for its consumer to load, for
        instance `programs.majordomo.pi.ui.packages = [ config.programs.pi-personal.variants.majordomo.package ]`.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = (cfg.keyFile == null) == (cfg.keyEnv == null);
        message = "programs.pi-personal: set keyEnv and keyFile together.";
      }
      {
        assertion = cfg.model == null || lib.length modelParts > 1;
        message = "programs.pi-personal.model must be provider/id.";
      }
    ];

    home.packages = lib.optional cfg.installPi piPackage;

    # pi discovers a package directory under extensions/ by its manifest; themes load from themes/,
    # which pi also watches for changes.
    home.file = {
      ".pi/agent/extensions/pi-personal".source = cfg.package;
      ".pi/agent/themes/${cfg.theme}.json".source = "${cfg.package}/themes/${cfg.theme}.json";
    }
    // lib.listToAttrs (
      map (p: lib.nameValuePair ".pi/agent/extensions/${lib.getName p}" { source = p; }) cfg.packages
    );

    home.activation.piPersonalSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] (
      mergeJson "settings.json" settingsFile
      + mergeJson "keybindings.json" keybindingsFile
      + lib.optionalString (cfg.models != { }) (mergeJson "models.json" modelsFile)
    );
  };
}
