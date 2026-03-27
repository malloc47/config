{
  config,
  pkgs,
  inputs,
  ...
}:

let
  installerSystem = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      (
        { modulesPath, ... }:
        {
          imports = [ (modulesPath + "/installer/netboot/netboot-minimal.nix") ];
          services.openssh = {
            enable = true;
            openFirewall = true;
            settings.PasswordAuthentication = false;
          };
          users.users.root.openssh.authorizedKeys.keys = [
            (builtins.readFile ../personal/ssh/malloc47/id_ed25519.pub)
          ];
        }
      )
    ];
  };
  netboot = installerSystem.config.system.build;
in
{
  services.pixiecore = {
    enable = true;
    openFirewall = true;
    dhcpNoBind = true;
    mode = "boot";
    kernel = "${netboot.kernel}/bzImage";
    initrd = "${netboot.netbootRamdisk}/initrd";
    cmdLine = "init=${netboot.toplevel}/init loglevel=4";
  };

  networking.firewall.allowedUDPPorts = [ 4011 ];
}
