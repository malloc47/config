{ config, lib, pkgs, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/virtualisation/vmware-guest.nix")
    ../modules/settings.nix
  ];

  settings = {
    vm = true;
    username = "jwaggoner";
    email = "jwaggoner@drwholdings.com";
    fontName = "Roboto Mono";
    fontSize = 9.0;
    profile = "drw";
    dpi = 254;
  };

  # For VPN reasons, this container shares the host network namespace
  # so its resolv.conf needs to match the host.
  networking.domain = "us.drwholdings.com";
  networking.search = ["us.drwholdings.com" "drwholdings.com" "lan"];
  networking.firewall.enable = false;
  # Hardcoding public DNS first so that it doesn't matter whether the
  # host is on the VPN or not.
  networking.nameservers = ["172.16.204.2" "10.64.16.15" "10.64.16.16"];

  security.pki.certificateFiles = [/etc/ssl/ca-bundle.pem];

  home-manager.users.${config.settings.username} = {
    home.packages = with pkgs; [
      argo
      dbeaver-bin
      gh
      kubectl
      maven
      remmina
      saml2aws
    ];

    xsession.windowManager.i3.config.keybindings."Mod4+Shift+N" = lib.mkForce
      "exec \"xterm -e 'nixos-rebuild switch --use-remote-sudo --impure; read -s -k \\?COMPLETE'\"";
  };
}
