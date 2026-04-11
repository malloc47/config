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
  imports = [ ../modules/settings.nix ];

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
        theme = "solarized-light-soft";
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

    # Muted solarized-light variant: uses content tones for UI chrome instead
    # of saturated accent colors. Auto-discovered by zellij from themes dir.
    xdg.configFile."zellij/themes/solarized-light-soft.kdl".text = ''
      themes {
          solarized-light-soft {
              text_unselected {
                  base 88 110 117
                  background 238 232 213
                  emphasis_0 203 75 22
                  emphasis_1 42 161 152
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              text_selected {
                  base 7 54 66
                  background 253 246 227
                  emphasis_0 203 75 22
                  emphasis_1 42 161 152
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              ribbon_selected {
                  base 238 232 213
                  background 88 110 117
                  emphasis_0 220 50 47
                  emphasis_1 203 75 22
                  emphasis_2 211 54 130
                  emphasis_3 38 139 210
              }
              ribbon_unselected {
                  base 101 123 131
                  background 238 232 213
                  emphasis_0 220 50 47
                  emphasis_1 147 161 161
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              table_title {
                  base 38 139 210
                  background 0
                  emphasis_0 203 75 22
                  emphasis_1 42 161 152
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              table_cell_selected {
                  base 7 54 66
                  background 253 246 227
                  emphasis_0 203 75 22
                  emphasis_1 42 161 152
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              table_cell_unselected {
                  base 88 110 117
                  background 238 232 213
                  emphasis_0 203 75 22
                  emphasis_1 42 161 152
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              list_selected {
                  base 7 54 66
                  background 253 246 227
                  emphasis_0 203 75 22
                  emphasis_1 42 161 152
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              list_unselected {
                  base 88 110 117
                  background 238 232 213
                  emphasis_0 203 75 22
                  emphasis_1 42 161 152
                  emphasis_2 38 139 210
                  emphasis_3 211 54 130
              }
              frame_selected {
                  base 38 139 210
                  background 0
                  emphasis_0 42 161 152
                  emphasis_1 108 113 196
                  emphasis_2 211 54 130
                  emphasis_3 0
              }
              frame_highlight {
                  base 42 161 152
                  background 0
                  emphasis_0 38 139 210
                  emphasis_1 42 161 152
                  emphasis_2 42 161 152
                  emphasis_3 42 161 152
              }
              exit_code_success {
                  base 133 153 0
                  background 0
                  emphasis_0 42 161 152
                  emphasis_1 88 110 117
                  emphasis_2 211 54 130
                  emphasis_3 38 139 210
              }
              exit_code_error {
                  base 220 50 47
                  background 0
                  emphasis_0 181 137 0
                  emphasis_1 0
                  emphasis_2 0
                  emphasis_3 0
              }
              multiplayer_user_colors {
                  player_1 211 54 130
                  player_2 38 139 210
                  player_3 0
                  player_4 181 137 0
                  player_5 42 161 152
                  player_6 0
                  player_7 108 113 196
                  player_8 0
                  player_9 0
                  player_10 0
              }
          }
      }
    '';

    home.packages = [
      agentPkgs.agent-deck
      agentPkgs.claude-code
      agentPkgs.opencode
    ];
  };
}
