{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.programs.gh-agenix;

  # Match the upstream-original wrapProgram formatting byte-for-byte:
  # `wrapProgram $out/bin/gh \` followed by 2-space-indented `--run '...'`
  # lines joined with backslash-continuation.
  injectEnv =
    envVar: tokenFile:
    "  --run 'if [ -z \"\${${envVar}:-}\" ] && [ -r \"${tokenFile}\" ]; then export ${envVar}=\"$(cat \"${tokenFile}\")\"; fi'";

  runLines = [
    (injectEnv "GH_TOKEN" cfg.tokenFile)
  ]
  ++ lib.optional (cfg.enterpriseTokenFile != null) (
    injectEnv "GH_ENTERPRISE_TOKEN" cfg.enterpriseTokenFile
  )
  ++ lib.optional (
    cfg.defaultHost != null
  ) "  --run 'if [ -z \"\${GH_HOST:-}\" ]; then export GH_HOST=\"${cfg.defaultHost}\"; fi'";

  gh-wrapped = pkgs.symlinkJoin {
    name = "gh-with-agenix";
    paths = [ pkgs.gh ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = "wrapProgram $out/bin/gh \\\n" + lib.concatStringsSep " \\\n" runLines + "\n";
  };
in
{
  options.programs.gh-agenix = {
    enable = lib.mkEnableOption "gh wrapped with an agenix-provided GH_TOKEN";

    tokenFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to the file containing GH_TOKEN at runtime (e.g. config.age.secrets.<name>.path).";
    };

    enterpriseTokenFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Optional path to the file containing GH_ENTERPRISE_TOKEN at runtime, for accessing a GitHub Enterprise host alongside github.com.";
    };

    defaultHost = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "github.example.com";
      description = "Optional hostname exported as GH_HOST (unless already set), making it gh's default target. Required for gh to pick up GH_ENTERPRISE_TOKEN, since hosts.yml is left empty.";
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
