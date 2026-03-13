# Actual Budget — self-hosted personal finance tracker.
#
# Replaces Monarch Money / YNAB. All data stays local.
# Bank sync (US: SimpleFIN bridge, EU: GoCardless) is optional and configured
# from within the Actual UI — the server itself has no cloud dependency.
#
# Access: https://actual.yourdomain.com
# On first visit, create a password and import any existing budget files.

{ ... }:

{
  virtualisation.oci-containers.containers.actual-budget = {
    image = "docker.io/actualbudget/actual-server:latest";
    ports = [ "127.0.0.1:5006:5006" ];
    volumes = [ "/data/actual:/data" ];
    environment = {
      # Actual listens on 5006 by default
      ACTUAL_PORT = "5006";
    };
  };
}
