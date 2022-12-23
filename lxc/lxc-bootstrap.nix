{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
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

  # https://discourse.nixos.org/t/howto-setup-lxd-on-nixos-with-nixos-guest-using-unmanaged-bridge-network-interface/21591
  boot.isContainer = true;
  environment.noXlibs = false;
  programs.command-not-found.enable = true;
  documentation.enable = true;
  environment.variables.NIX_REMOTE = lib.mkForce "";
  systemd.services."console-getty".enable = false;
  systemd.services."getty@".enable = false;

  # This bare LXC image will ultimately be bootstrapped with a real
  # config, so set the hostname in advance so there's one less change
  networking.hostName = "harpocrates";
  networking.firewall.enable = false;
  networking.nameservers = ["8.8.8.8" "8.8.4.4"];

  nix.settings.trusted-users = [ "@wheel" ];
  nix.extraOptions = ''
    tarball-ttl = 604800
    experimental-features = nix-command flakes
  '';

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    git
  ];

  security.sudo.wheelNeedsPassword = false;

  programs.zsh.enable = true;

  # Used for file sharing between host and guest
  services.openssh.enable = true;
  users.users.${config.settings.username} = {
    openssh.authorizedKeys.keys = [
      (builtins.readFile (../personal/ssh + "/${config.settings.profile}/id_rsa.pub"))
    ];
  };

  users.users.${config.settings.username} = {
    isNormalUser = true;
    createHome = true;
    home = "/home/${config.settings.username}";
    description = "Jarrell Waggoner";
    extraGroups = ["audio" "docker" "networkmanager" "wheel" "lxd"];
    uid = 1000;
    shell = pkgs.zsh;
  };

  system.stateVersion = "21.11";

}
