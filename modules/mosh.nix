{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.mosh-server;
  # See config/home.nix for context.  Builds mosh from the head of
  # mobile-shell/mosh#1104 since the diff doesn't apply cleanly to the
  # 1.4.0 release tarball.
  patchedMosh = pkgs.mosh.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "mgulick";
      repo = "mosh";
      rev = "e8337eff281f1cbecf6292dac64598ede7277928";
      hash = "sha256-bWLAkZoH7ralcUxwt8KSSKSEh466NGTaaruFM7x99aE=";
    };
    # nixpkgs cherry-picks upstream commit eee1a8cf into 1.4.0; that
    # commit is already in master / the PR branch, so drop the
    # cherry-pick patch to avoid "reversed patch detected" failures.
    patches = builtins.filter (p: !(lib.hasInfix "eee1a8cf" (toString p))) (old.patches or [ ]);
  });
in
{
  options.services.mosh-server = {
    enable = lib.mkEnableOption "mosh (installs binaries, opens UDP 60000-61000)";

    interface = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "tailscale0";
      description = ''
        If set, restrict mosh UDP ports to this interface instead of opening
        them globally. Useful for public VPSes where mosh should only be
        reachable over a private network (e.g. Tailscale).
      '';
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        programs.mosh.enable = true;
        programs.mosh.package = patchedMosh;
      }

      (lib.mkIf (cfg.interface == null) {
        programs.mosh.openFirewall = true;
      })

      (lib.mkIf (cfg.interface != null) {
        programs.mosh.openFirewall = false;
        networking.firewall.interfaces.${cfg.interface}.allowedUDPPortRanges = [
          {
            from = 60000;
            to = 61000;
          }
        ];
      })
    ]
  );
}
