{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ../nixos/configuration.nix
    ../modules/settings.nix
    "${modulesPath}/virtualisation/lxc-container.nix"
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontSize = 9.0;
    xkbFile = "vm";
    terminal = "kitty";
  };

  # TODO: pull this out into hardware/lxc.nix
  # https://discourse.nixos.org/t/howto-setup-lxd-on-nixos-with-nixos-guest-using-unmanaged-bridge-network-interface/21591
  boot.isContainer = true;
  environment.noXlibs = false;
  programs.command-not-found.enable = true;
  documentation.enable = true;
  environment.variables.NIX_REMOTE = lib.mkForce "";
  systemd.services."console-getty".enable = false;
  systemd.services."getty@".enable = false;

  networking.hostName = "harpocrates";
  networking.firewall.enable = false;
  networking.nameservers = ["8.8.8.8" "8.8.4.4"];

  services.xserver.autoRepeatDelay = 250;

  services.xserver.dpi = 277;

  hardware.video.hidpi.enable = true;

  # HiDPI fix for alacritty
  environment.variables.WINIT_HIDPI_FACTOR = "3";
  # Chrome scaling fix
  environment.variables.GDK_SCALE = "3.0";
  environment.variables.GDK_DPI_SCALE="0.25";

  nix.settings.trusted-users = [ "@wheel" ];
  nix.extraOptions = ''
    tarball-ttl = 604800
    experimental-features = nix-command flakes
  '';

  # Used for file sharing between host and guest
  services.openssh.enable = true;
  users.users.${config.settings.username} = {
    openssh.authorizedKeys.keys = [
      (builtins.readFile (../personal/ssh + "/${config.settings.profile}/id_rsa.pub"))
    ];
  };

  home-manager.users.${config.settings.username} = {
    home.pointerCursor.size = 64;
    xresources.properties."Xft.dpi" = 277;
  };
}
