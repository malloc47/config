{ lib, ... }:

{
  # modules/networking.nix (shared across all hosts) enables NetworkManager,
  # which is inappropriate for a headless server. Override it here.
  networking.networkmanager.enable = lib.mkForce false;

  # Re-enable the firewall (modules/networking.nix sets enable = false for
  # desktop hosts that manage their own iptables via NetworkManager).
  networking.firewall.enable = true;

  # aida is the reverse-proxy endpoint for all internal services.
  # All service containers bind to 127.0.0.1 and Caddy listens on 443.
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # Syncthing relay and discovery ports (intra-LAN sync, not exposed to WAN
  # since the router blocks inbound TCP from outside)
  networking.firewall.allowedTCPPortRanges = [
    { from = 22000; to = 22001; }
  ];
  networking.firewall.allowedUDPPortRanges = [
    { from = 22000; to = 22001; }
  ];

  # Static LAN IP — avoids DHCP races at boot and gives Caddy a stable address
  # that the router's split-DNS record points to.
  #
  # Interface name: enp1s0 is the typical PCIe NIC name on the G10.
  # Verify with `ip link show` on first boot; adjust if different (e.g. eth0).
  networking.interfaces.enp1s0.ipv4.addresses = [
    {
      address = "192.168.1.10";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "192.168.1.1";

  # Use the router as DNS resolver so split-DNS (*.yourdomain.com -> 192.168.1.10)
  # resolves correctly on the box itself (e.g. for Caddy's ACME DNS challenge
  # verifying its own cert, or containers reaching other containers by hostname).
  networking.nameservers = [ "192.168.1.1" ];

  # Disable DHCP on all interfaces — we are fully static
  networking.useDHCP = false;
}
