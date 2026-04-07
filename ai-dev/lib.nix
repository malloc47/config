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
        allowWrite = acc.allowWrite ++ (p.allowWrite or [ ]);
        allowRead = acc.allowRead ++ (p.allowRead or [ ]);
      })
      {
        packages = [ ];
        domains = [ ];
        allowWrite = [ ];
        allowRead = [ ];
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
        allowWrite = [ ];
        domains = [ ];
        packages = [ ];
        mkWrappedPkg =
          {
            extraWritePaths ? [ ],
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
      extraAllowWrite ? [ ],
      extraAllowRead ? [ ],
      env ? { },
      # true = unrestricted network; false = restrict to domain allowlist.
      unrestrictedNetwork ? false,
      unrestrictedHarness ? false,
    }:
    let
      harness = resolveHarness harnessSpec;
      merged = mergeProfiles profiles;

      allAllowWrite = harness.allowWrite ++ merged.allowWrite ++ extraAllowWrite;
      allAllowRead = merged.allowRead ++ extraAllowRead;
      allDomains = defaults.allowedDomains ++ harness.domains ++ merged.domains ++ extraDomains;
      allPackages = defaults.sandboxPackages ++ harness.packages ++ merged.packages ++ extraPackages;

      wrappedPkg = harness.mkWrappedPkg {
        extraWritePaths = extraAllowWrite;
        unrestricted = unrestrictedHarness;
      };
    in
    if pkgs.stdenv.isLinux then
      mkSandboxUpstream {
        pkg = wrappedPkg;
        inherit (harness) binName outName;
        packages = allPackages;
        allowWrite = allAllowWrite;
        allowRead = allAllowRead;
        allowNet = if unrestrictedNetwork then true else allDomains;
        inherit env;
      }
    else
      # Darwin: Seatbelt blocks macOS Keychain access, which claude-code
      # and other tools need for OAuth credential storage.  Until zerobox
      # gains a --allow-keychain or Mach-service allowlist, skip sandboxing.
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
      extraAllowWrite ? [ ],
      extraAllowRead ? [ ],
      env ? { },
      extraShellPackages ? [ ],
      unrestrictedNetwork ? false,
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
            extraAllowWrite
            extraAllowRead
            env
            unrestrictedNetwork
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
  mkSandbox = mkSandboxUpstream;
}
