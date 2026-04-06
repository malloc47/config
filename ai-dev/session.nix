# Home-manager module: AI session orchestration (agent-deck, zellij-ai)
# Independent of sandboxing — launches whatever claude/opencode is on $PATH.
#
#   imports = [ inputs.ai-dev.homeManagerModules.session ];
#   programs.ai-session.enable = true;
{ self, ... }:
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.ai-session;
  system = pkgs.stdenv.hostPlatform.system;
  aiPkgs = self.packages.${system};
in
{
  options.programs.ai-session = {
    enable = lib.mkEnableOption "AI session orchestration (agent-deck, zellij-ai)";

    installXdgZellijConfig = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        If true, install the Zellij config to ~/.config/zellij/config.kdl so that
        plain `zellij` picks it up automatically. If false (default), users invoke
        `zellij-ai` which bakes the config into the store path.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      aiPkgs.agent-deck
      aiPkgs.zellij-ai
      pkgs.zellij
    ];

    xdg.configFile."zellij/config.kdl" = lib.mkIf cfg.installXdgZellijConfig {
      source = self + "/zellij-config.kdl";
    };
  };
}
