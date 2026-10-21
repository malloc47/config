# sqlit (terminal SQL client) with declarative control over where it keeps
# saved connection passwords.
#
# By default sqlit probes the system keyring at startup, retrying against
# D-Bus. On headless hosts with no Secret Service that probe stalls for a
# while and ends in a "Keyring unavailable" warning. The `credentials` option
# lets a host opt out of that probe.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.sqlit;

  # With no usable keyring, tell sqlit not to probe for one at all. Plaintext
  # storage needs this too: sqlit still probes when saving a connection that
  # has a password, even after plaintext storage has been selected.
  skipProbe = cfg.credentials != "keyring";

  package =
    if skipProbe then
      pkgs.symlinkJoin {
        name = "${cfg.package.name}-wrapped";
        paths = [ cfg.package ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/sqlit" --set SQLIT_SKIP_KEYRING_PROBE 1
        '';
        meta.mainProgram = "sqlit";
      }
    else
      cfg.package;

  settingsFile = "${config.xdg.configHome}/sqlit/settings.json";
  jq = lib.getExe pkgs.jq;
in
{
  options.programs.sqlit = {
    enable = lib.mkEnableOption "sqlit, a terminal SQL client";

    package = lib.mkPackageOption pkgs "sqlit-tui" { };

    credentials = lib.mkOption {
      type = lib.types.enum [
        "keyring"
        "plaintext"
        "none"
      ];
      default = "keyring";
      description = ''
        Where sqlit stores saved connection passwords.

        - `keyring`: sqlit's default. Probe the system keyring (macOS
          Keychain, Secret Service) at startup.
        - `plaintext`: store passwords in a file in the sqlit config
          directory and never touch the keyring. Passwords persist but sit on
          disk unencrypted. Sets `allow_plaintext_credentials` in
          `settings.json` on activation, leaving the rest of that file alone.
        - `none`: skip the keyring probe and keep passwords in memory only, so
          sqlit prompts for them each session. Nothing secret is written to
          disk.

        Switching back to `keyring` does not clear `allow_plaintext_credentials`
        from an existing `settings.json`; use `:credentials keyring` in sqlit
        for that.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ package ];

    # settings.json also holds sqlit's own mutable state (theme, expanded
    # tree nodes, ...), so it can't be a read-only store symlink. Merge just
    # the one key instead.
    home.activation.sqlitCredentials = lib.mkIf (cfg.credentials == "plaintext") (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        settings="${settingsFile}"
        if [ -z "''${DRY_RUN:-}" ]; then
          mkdir -p "$(dirname "$settings")"
          if [ ! -s "$settings" ]; then
            echo '{"allow_plaintext_credentials": true}' > "$settings"
          elif ! ${jq} -e '.allow_plaintext_credentials == true' "$settings" >/dev/null 2>&1; then
            tmp="$(mktemp)"
            if ${jq} '.allow_plaintext_credentials = true' "$settings" > "$tmp"; then
              cat "$tmp" > "$settings"
            else
              echo "sqlit: could not parse $settings; not enabling plaintext credentials" >&2
            fi
            rm -f "$tmp"
          fi
        fi
      ''
    );
  };
}
