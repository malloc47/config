{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.programs.gh-agenix;

  gh-wrapped = pkgs.symlinkJoin {
    name = "gh-with-agenix";
    paths = [ pkgs.gh ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/gh \
        --run 'if [ -z "''${GH_TOKEN:-}" ] && [ -r "${cfg.tokenFile}" ]; then export GH_TOKEN="$(cat "${cfg.tokenFile}")"; fi'
    '';
  };
in
{
  options.programs.gh-agenix = {
    enable = lib.mkEnableOption "gh wrapped with an agenix-provided GH_TOKEN";

    tokenFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to the file containing GH_TOKEN at runtime (e.g. config.age.secrets.<name>.path).";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ (lib.hiPrio gh-wrapped) ];

    # Intentionally empty: any host listed here makes gh's startup
    # migration try to resolve a token via libsecret, which fails on
    # hosts without a running secret service. Env vars cover auth;
    # --hostname covers host targeting.
    xdg.configFile."gh/hosts.yml".text = "";
  };
}
