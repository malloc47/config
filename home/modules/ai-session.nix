{ inputs, ... }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.ai-session;
  system = pkgs.stdenv.hostPlatform.system;
  agentPkgs = inputs.llm-agents.packages.${system};
in
{
  imports = [ ../../modules/settings.nix ];

  options.programs.ai-session = {
    enable = lib.mkEnableOption "AI session orchestration (zellij + agent-deck)";

    webServer = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable Zellij web server (requires Zellij >= 0.44.0).";
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 8082;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zellij = {
      enable = true;
      settings = {
        theme = "stylix-soft";
        ui.pane_frames.rounded_corners = true;
        show_startup_tips = false;
      }
      // lib.optionalAttrs cfg.webServer.enable {
        web_server = true;
        web_server_ip = "127.0.0.1";
        web_server_port = cfg.webServer.port;
        web_sharing = "on";
        enforce_https_for_localhost = false;
      };
    };

    # Muted variant: uses content tones (base04/05) for UI chrome instead of
    # saturated accents. RGB triples come from stylix so the theme tracks the
    # active base16 scheme. Auto-discovered by zellij from the themes dir.
    # Literal `0` is zellij's "terminal default" sentinel — not a color.
    xdg.configFile."zellij/themes/stylix-soft.kdl".text =
      let
        c = config.lib.stylix.colors;
        rgb = name: "${c."${name}-rgb-r"} ${c."${name}-rgb-g"} ${c."${name}-rgb-b"}";
        base00 = rgb "base00";
        base01 = rgb "base01";
        base02 = rgb "base02";
        base04 = rgb "base04";
        base05 = rgb "base05";
        base06 = rgb "base06";
        base08 = rgb "base08";
        base09 = rgb "base09";
        base0A = rgb "base0A";
        base0B = rgb "base0B";
        base0C = rgb "base0C";
        base0D = rgb "base0D";
        base0E = rgb "base0E";
        base0F = rgb "base0F";
      in
      ''
        themes {
            stylix-soft {
                text_unselected {
                    base ${base05}
                    background ${base01}
                    emphasis_0 ${base09}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                text_selected {
                    base ${base06}
                    background ${base00}
                    emphasis_0 ${base09}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                ribbon_selected {
                    base ${base01}
                    background ${base05}
                    emphasis_0 ${base08}
                    emphasis_1 ${base09}
                    emphasis_2 ${base0F}
                    emphasis_3 ${base0D}
                }
                ribbon_unselected {
                    base ${base04}
                    background ${base01}
                    emphasis_0 ${base08}
                    emphasis_1 ${base02}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                table_title {
                    base ${base0D}
                    background 0
                    emphasis_0 ${base09}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                table_cell_selected {
                    base ${base06}
                    background ${base00}
                    emphasis_0 ${base09}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                table_cell_unselected {
                    base ${base05}
                    background ${base01}
                    emphasis_0 ${base09}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                list_selected {
                    base ${base06}
                    background ${base00}
                    emphasis_0 ${base09}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                list_unselected {
                    base ${base05}
                    background ${base01}
                    emphasis_0 ${base09}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0D}
                    emphasis_3 ${base0F}
                }
                frame_selected {
                    base ${base0D}
                    background 0
                    emphasis_0 ${base0C}
                    emphasis_1 ${base0E}
                    emphasis_2 ${base0F}
                    emphasis_3 0
                }
                frame_highlight {
                    base ${base0C}
                    background 0
                    emphasis_0 ${base0D}
                    emphasis_1 ${base0C}
                    emphasis_2 ${base0C}
                    emphasis_3 ${base0C}
                }
                exit_code_success {
                    base ${base0B}
                    background 0
                    emphasis_0 ${base0C}
                    emphasis_1 ${base05}
                    emphasis_2 ${base0F}
                    emphasis_3 ${base0D}
                }
                exit_code_error {
                    base ${base08}
                    background 0
                    emphasis_0 ${base0A}
                    emphasis_1 0
                    emphasis_2 0
                    emphasis_3 0
                }
                multiplayer_user_colors {
                    player_1 ${base0F}
                    player_2 ${base0D}
                    player_3 0
                    player_4 ${base0A}
                    player_5 ${base0C}
                    player_6 0
                    player_7 ${base0E}
                    player_8 0
                    player_9 0
                    player_10 0
                }
            }
        }
      '';

    home.file.".agent-deck/config.toml".source =
      (pkgs.formats.toml { }).generate "agent-deck-config.toml"
        {
          default_tool = "claude";
          theme = "light";
          tmux = {
            window_style_override = "default";
          };
        };

    programs.codex = {
      enable = true;
      package = agentPkgs.codex;
    };

    programs.claude-code = {
      enable = true;
      package = agentPkgs.claude-code;
    };

    home.packages = [
      (agentPkgs.agent-deck.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          ./patches/agent-deck-no-extkeys.patch
          ./patches/agent-deck-no-clobber-symlink.patch
        ];
      }))
      agentPkgs.opencode
      pkgs.claude-code-acp
    ];
  };
}
