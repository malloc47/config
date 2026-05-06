{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.mosh-server;
  patchedMosh = pkgs.mosh.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ../config/patches/mosh-osc52-selection-types.patch
    ];
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
