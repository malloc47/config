# Composition engine: merges harnesses and profiles into sandboxed derivations.
{
  pkgs,
  agents,
  mkSandboxUpstream,
  defaults,
  harnessDefinitions,
  profileDefinitions,
}:
let
  # Fold a list of profiles into a single aggregate config.
  mergeProfiles =
    profiles:
    builtins.foldl'
      (acc: p: {
        packages = acc.packages ++ (p.packages or [ ]);
        domains = acc.domains ++ (p.domains or [ ]);
        stateDirs = acc.stateDirs ++ (p.stateDirs or [ ]);
        stateFiles = acc.stateFiles ++ (p.stateFiles or [ ]);
      })
      {
        packages = [ ];
        domains = [ ];
        stateDirs = [ ];
        stateFiles = [ ];
      }
      profiles;

  # Resolve a harness spec (string name or raw derivation) to a definition.
  resolveHarness =
    h:
    if builtins.isString h then
      harnessDefinitions.${h}
        or (throw "Unknown harness: ${h}. Known: ${builtins.concatStringsSep ", " (builtins.attrNames harnessDefinitions)}")
    else
      # Bare derivation: generic wrapper with no special state/flags
      {
        pkg = h;
        binName = h.pname or h.name;
        outName = h.pname or h.name;
        stateDirs = [ ];
        stateFiles = [ ];
        domains = [ ];
        packages = [ ];
        mkWrappedPkg =
          {
            extraStateDirs ? [ ],
            unrestricted ? false,
          }:
          h;
      };

  # Build a single sandboxed harness derivation.
  mkSandboxedHarness =
    harnessSpec:
    {
      profiles ? [ ],
      extraDomains ? [ ],
      extraPackages ? [ ],
      extraStateDirs ? [ ],
      extraStateFiles ? [ ],
      extraEnv ? { },
      restrictNetwork ? true,
      unrestrictedHarness ? false,
    }:
    let
      harness = resolveHarness harnessSpec;
      merged = mergeProfiles profiles;

      allStateDirs = harness.stateDirs ++ merged.stateDirs ++ extraStateDirs;
      allStateFiles = (harness.stateFiles or [ ]) ++ merged.stateFiles ++ extraStateFiles;
      allDomains = defaults.allowedDomains ++ harness.domains ++ merged.domains ++ extraDomains;
      allPackages = defaults.sandboxPackages ++ harness.packages ++ merged.packages ++ extraPackages;

      wrappedPkg = harness.mkWrappedPkg {
        inherit extraStateDirs;
        unrestricted = unrestrictedHarness;
      };
    in
    if pkgs.stdenv.isLinux then
      mkSandboxUpstream {
        pkg = wrappedPkg;
        inherit (harness) binName outName;
        allowedPackages = allPackages;
        stateDirs = allStateDirs;
        stateFiles = allStateFiles;
        inherit restrictNetwork;
        allowedDomains = allDomains;
        extraEnv = extraEnv;
      }
    else
      # Darwin: bubblewrap unavailable, return unwrapped package
      wrappedPkg;

  # High-level: produce a devShell from a list of harnesses.
  mkProjectShell =
    {
      harnesses ? [
        "claude-code"
        "opencode"
      ],
      profiles ? [ ],
      extraDomains ? [ ],
      extraPackages ? [ ],
      extraStateDirs ? [ ],
      extraStateFiles ? [ ],
      extraEnv ? { },
      extraShellPackages ? [ ],
      restrictNetwork ? true,
      unrestrictedHarness ? false,
    }:
    let
      sandboxedBinaries = map (
        h:
        mkSandboxedHarness h {
          inherit
            profiles
            extraDomains
            extraPackages
            extraStateDirs
            extraStateFiles
            extraEnv
            restrictNetwork
            unrestrictedHarness
            ;
        }
      ) harnesses;
    in
    pkgs.mkShell {
      packages = sandboxedBinaries ++ extraShellPackages;
    };

in
{
  inherit
    mkProjectShell
    mkSandboxedHarness
    mergeProfiles
    resolveHarness
    ;
  inherit (defaults) sandboxPackages allowedDomains;
  profiles = profileDefinitions;
  # Re-export for power users
  mkSandbox = mkSandboxUpstream;
}
