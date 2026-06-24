{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.drift-check;
  diff = "${pkgs.diffutils}/bin/diff";
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
    home.activation.driftCheck =
      lib.hm.dag.entryBetween (lib.unique (lib.concatMap (f: f.before) cfg.files)) [ "writeBoundary" ]
        (
          lib.concatStrings (
            map (f: ''
              if [ -e ${lib.escapeShellArg f.path} ]; then
                if ! ${diff} -q ${f.source} ${lib.escapeShellArg f.path} > /dev/null 2>&1; then
                  printf '\033[33mwarning\033[0m: %s has drifted from nix-managed source (will be reset on next switch):\n' \
                    ${lib.escapeShellArg f.path} >&2
                  ${diff} --color=always ${f.source} ${lib.escapeShellArg f.path} >&2 || true
                fi
              fi
            '') cfg.files
          )
        );
  };
}
