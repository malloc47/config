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

  # Produce the shell that compares one registered file against its source.
  # For format = "json", both sides are normalized through `jq -S` (sorted
  # keys, canonical whitespace) before diffing, so cosmetic reformatting by
  # the tool that owns the file (e.g. compact toJSON vs. pretty-printed
  # rewrites) is not reported as drift. Comparison/diff failures are
  # swallowed so a switch is never blocked.
  checkFile =
    f:
    let
      escPath = lib.escapeShellArg f.path;
      escSource = lib.escapeShellArg "${f.source}";
      warn = ''
        printf '\033[33mwarning\033[0m: %s has drifted from nix-managed source (will be reset on next switch):\n' \
          ${escPath} >&2
      '';
      jsonCheck = ''
        if [ -e ${escPath} ]; then
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
        fi
      '';
      rawCheck = ''
        if [ -e ${escPath} ]; then
          if ! ${diff} -q ${escSource} ${escPath} > /dev/null 2>&1; then
            ${warn}
            ${diff} --color=always ${escSource} ${escPath} >&2 || true
          fi
        fi
      '';
    in
    if f.format == "json" then jsonCheck else rawCheck;
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
            before = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Activation step names this drift check must run before (typically the cp step that resets the file).";
            };
          };
        }
      );
      default = [ ];
      description = ''
        Files to monitor for drift against their nix-generated source.

        Each registered file is checked during `home-manager switch`; if the
        on-disk copy differs from the nix-store source, a warning and unified
        diff are printed to stderr.  The deploy is never blocked.

        Modules that manage mutable config files via home.activation should
        register their files here so users can see runtime changes before they
        are silently overwritten on the next switch.
      '';
    };
  };

  config = lib.mkIf (cfg.files != [ ]) {
    home.activation.driftCheck = lib.hm.dag.entryBetween (lib.unique (
      lib.concatMap (f: f.before) cfg.files
    )) [ "writeBoundary" ] (lib.concatStrings (map checkFile cfg.files));
  };
}
