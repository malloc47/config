# Home-manager module: AI coding tools (claude-code, opencode)
# These are the tools that per-project sandboxes wrap.
#
#   imports = [ inputs.ai-dev.homeManagerModules.sandbox ];
#   programs.ai-sandbox.enable = true;
{ self, ... }:
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.ai-sandbox;
  system = pkgs.stdenv.hostPlatform.system;
  aiPkgs = self.packages.${system};
in
{
  options.programs.ai-sandbox = {
    enable = lib.mkEnableOption "AI coding tools (claude-code, opencode) for per-project sandboxing";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      aiPkgs.claude-code
      aiPkgs.opencode
    ];
  };
}
