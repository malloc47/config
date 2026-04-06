# Usage from a home-manager config:
#   imports = [ inputs.ai-dev.homeManagerModules.default ];
#   programs.ai-dev.enable = true;
#   programs.ai-dev.installXdgZellijConfig = false;  # default -- use wrapper
{ self, ... }:
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.ai-dev;
  system = pkgs.stdenv.hostPlatform.system;
  aiPkgs = self.packages.${system};
in
{
  options.programs.ai-dev = {
    enable = lib.mkEnableOption "AI coding tools (claude, opencode, agent-deck, zellij-ai)";

    installXdgZellijConfig = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        If true, install the Zellij config to ~/.config/zellij/config.kdl so that
        plain `zellij` picks it up automatically (and the config becomes editable
        in place). If false (default), users invoke `zellij-ai` which bakes the
        config into the store path -- fully reproducible but read-only.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      aiPkgs.claude-code
      aiPkgs.opencode
      aiPkgs.agent-deck
      aiPkgs.zellij-ai
      pkgs.zellij
    ];

    xdg.configFile."zellij/config.kdl" = lib.mkIf cfg.installXdgZellijConfig {
      source = self + "/zellij-config.kdl";
    };
  };
}
