{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.ntfy-git-sync;

  syncScript = pkgs.writeShellApplication {
    name = "git-sync-subscriber";
    runtimeInputs = [
      pkgs.curl
      pkgs.git
      pkgs.openssh
    ];
    text = ''
      TOKEN_FILE="$1"
      NTFY_URL="$2"
      shift 2
      REPOS=("$@")

      sync_repos() {
        for repo in "''${REPOS[@]}"; do
          if [ -d "$repo/.git" ]; then
            git -C "$repo" pull --ff-only || git -C "$repo" fetch || true
          fi
        done
      }

      sync_repos

      while true; do
        curl -sf -N \
          -u "git-sync:$(cat "$TOKEN_FILE")" \
          "$NTFY_URL/json" | \
        while read -r _; do
          sync_repos
        done || true
        sleep 10
        sync_repos
      done
    '';
  };

  wrapperScript = pkgs.writeShellScript "git-sync-wrapper" (
    "exec ${syncScript}/bin/git-sync-subscriber "
    + lib.escapeShellArg cfg.tokenFile
    + " "
    + lib.escapeShellArg cfg.ntfyUrl
    + " "
    + lib.concatMapStringsSep " " lib.escapeShellArg cfg.repos
  );
in
{
  options.services.ntfy-git-sync = {
    enable = lib.mkEnableOption "ntfy-based git repo sync";

    repos = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Absolute paths to git repositories to keep in sync.";
    };

    ntfyUrl = lib.mkOption {
      type = lib.types.str;
      default = "https://ntfy.malloc47.com/git-sync";
      description = "ntfy topic URL to subscribe to.";
    };

    tokenFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to file containing the ntfy password for the git-sync user.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.repos != [ ];
        message = "services.ntfy-git-sync.repos must contain at least one repository path.";
      }
    ];

    systemd.user.services.ntfy-git-sync = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "ntfy-based git repo sync";
        After = [ "network-online.target" ];
      };
      Service = {
        ExecStart = toString wrapperScript;
        Restart = "always";
        RestartSec = 30;
      };
      Install.WantedBy = [ "default.target" ];
    };

    launchd.agents.git-sync = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        ProgramArguments = [
          "${syncScript}/bin/git-sync-subscriber"
          cfg.tokenFile
          cfg.ntfyUrl
        ]
        ++ cfg.repos;
        KeepAlive = true;
        RunAtLoad = true;
        StandardOutPath = "/tmp/git-sync.stdout.log";
        StandardErrorPath = "/tmp/git-sync.stderr.log";
      };
    };
  };
}
