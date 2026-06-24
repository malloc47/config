{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.drift-check;
  diff = "${pkgs.diffutils}/bin/diff";
  jq = "${pkgs.jq}/bin/jq";

  # Produce the shell that manages one registered file:
  #
  #   * If the file does not exist, seed it from the nix source as a plain
  #     writable copy (NOT a read-only store symlink), so the owning tool can
  #     rewrite it at runtime.
  #   * If it already exists, leave it untouched and only report drift from
  #     the current nix source. The user's runtime edits are preserved; the
  #     switch never fails over a divergence.
  #
  # For format = "json", both sides are normalized through `jq -S` (sorted
  # keys, canonical whitespace) before diffing, so cosmetic reformatting by
  # the tool that owns the file (e.g. compact toJSON vs. pretty-printed
  # rewrites) is not reported as drift. Comparison/diff failures are
  # swallowed so a switch is never blocked.
  manageFile =
    f:
    let
      escPath = lib.escapeShellArg f.path;
      escSource = lib.escapeShellArg "${f.source}";
      warn = ''
        printf '\033[33mwarning\033[0m: %s has drifted from nix-managed source (left as-is; delete it to reseed):\n' \
          ${escPath} >&2
      '';
      jsonCheck = ''
        _dc_want="$(${jq} -S . ${escSource} 2>/dev/null)"
        _dc_have="$(${jq} -S . ${escPath} 2>/dev/null)"
        if [ -z "$_dc_have" ]; then
          # Unparseable on-disk JSON is itself drift worth surfacing.
          ${warn}
          ${diff} --color=always ${escSource} ${escPath} >&2 || true
        elif [ "$_dc_want" != "$_dc_have" ]; then
          ${warn}
          ${diff} --color=always \
            <(printf '%s\n' "$_dc_want") \
            <(printf '%s\n' "$_dc_have") >&2 || true
        fi
        unset _dc_want _dc_have
      '';
      rawCheck = ''
        if ! ${diff} -q ${escSource} ${escPath} > /dev/null 2>&1; then
          ${warn}
          ${diff} --color=always ${escSource} ${escPath} >&2 || true
        fi
      '';
      check = if f.format == "json" then jsonCheck else rawCheck;
    in
    ''
      if [ ! -e ${escPath} ]; then
        $DRY_RUN_CMD mkdir -p "$(dirname ${escPath})"
        $DRY_RUN_CMD cp -f ${escSource} ${escPath}
        $DRY_RUN_CMD chmod u+w ${escPath}
      else
        ${check}
      fi
    '';
in
{
  options.programs.drift-check = {
    files = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            path = lib.mkOption {
              type = lib.types.str;
              description = "Absolute runtime path of the managed file (use config.home.homeDirectory, not $HOME).";
            };
            source = lib.mkOption {
              type = lib.types.path;
              description = "Nix store path of the canonical file content (e.g. pkgs.writeText result).";
            };
            format = lib.mkOption {
              type = lib.types.enum [
                "raw"
                "json"
              ];
              default = "raw";
              description = ''
                How to compare the file. "raw" does a byte-for-byte diff.
                "json" normalizes both sides through `jq -S` first, so key
                ordering and whitespace differences are ignored and only
                semantic JSON drift is reported.
              '';
            };
          };
        }
      );
      default = [ ];
      description = ''
        Mutable config files seeded from nix and monitored for drift.

        For each registered file, during `home-manager switch`:

          * If the file is missing, it is seeded from `source` as a plain
            writable copy (not a read-only store symlink), so the owning tool
            can rewrite it at runtime.
          * If it already exists, it is left untouched; if its contents differ
            from the current nix source, a warning and diff are printed to
            stderr. Runtime edits are preserved and the switch never fails.

        Use this for files a tool must write back to (settings saves, trusted
        repo lists, etc.) where a read-only home.file symlink would break it.
        Delete the on-disk file to pick up a changed nix source.
      '';
    };
  };

  config = lib.mkIf (cfg.files != [ ]) {
    home.activation.driftCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] (
      lib.concatStrings (map manageFile cfg.files)
    );
  };
}
