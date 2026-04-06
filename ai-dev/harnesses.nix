# Harness definitions: per-tool knowledge for sandbox configuration.
# Each harness knows its package, binary name, state paths, and how to
# inject CLI flags (e.g. --add-dir, --dangerously-skip-permissions).
{
  pkgs,
  agents,
  defaults,
}:
{
  "claude-code" = {
    pkg = agents.claude-code;
    binName = "claude";
    outName = "claude";
    stateDirs = defaults.claudeStateDirs;
    stateFiles = defaults.claudeStateFiles;
    domains = [ ];
    packages = [ ];

    # Returns the pkg, possibly wrapped with extra CLI flags before sandboxing.
    mkWrappedPkg =
      {
        extraStateDirs ? [ ],
        unrestricted ? false,
      }:
      let
        addDirFlags = map (d: "--add-dir ${d}") extraStateDirs;
        permFlags = if unrestricted then [ "--dangerously-skip-permissions" ] else [ ];
        allFlags = addDirFlags ++ permFlags;
      in
      if allFlags == [ ] then
        agents.claude-code
      else
        pkgs.writeShellApplication {
          name = "claude";
          runtimeInputs = [ agents.claude-code ];
          text = ''exec claude ${builtins.concatStringsSep " " allFlags} "$@"'';
        };
  };

  "opencode" = {
    pkg = agents.opencode;
    binName = "opencode";
    outName = "opencode";
    stateDirs = defaults.opencodeStateDirs;
    stateFiles = [ ];
    domains = [ ];
    packages = [ ];

    mkWrappedPkg =
      {
        extraStateDirs ? [ ],
        unrestricted ? false,
      }:
      agents.opencode;
  };
}
