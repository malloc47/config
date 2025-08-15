{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [
    ../modules/settings.nix
    ../hardware/lxc.nix
  ];

  settings = {
    vm = true;
    username = "malloc47";
    fontSize = 9.0;
    xkbFile = "vm";
    terminal = "kitty";
  };

  # This bare LXC image will ultimately be bootstrapped with a real
  # config, so set the hostname in advance so there's one less change
  networking.hostName = "harpocrates";
  networking.firewall.enable = false;
  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];

  nix.settings.trusted-users = [ "@wheel" ];
  nix.extraOptions = ''
    tarball-ttl = 604800
    experimental-features = nix-command flakes
  '';

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    git
  ];

  # This bootstrap environment does not have any overlays added, but
  # the full host will. There's a circular dependency where my
  # configuration.nix defines this file, but before creating it relies
  # on it for the overlay refrences within the configuration.nix file
  # to resolve.
  environment.etc."overlays-compat" = {
    text = ''
      self: super:
      with super.lib;
      let
        eval = import <nixpkgs/nixos/lib/eval-config.nix>;
        paths = (eval {modules = [(import <nixos-config>)];})
          .config.nixpkgs.overlays;
      in
      foldl' (flip extends) (_: super) paths self
    '';
    target = "nixos/overlays-compat/overlays.nix";
  };

  security.sudo.wheelNeedsPassword = false;

  programs.zsh.enable = true;

  services.openssh.enable = true;
  users.users.${config.settings.username} = {
    isNormalUser = true;
    createHome = true;
    home = "/home/${config.settings.username}";
    description = "Jarrell Waggoner";
    extraGroups = [
      "audio"
      "docker"
      "networkmanager"
      "wheel"
      "lxd"
    ];
    uid = 1000;
    shell = pkgs.zsh;
    # Used for file sharing between host and guest
    openssh.authorizedKeys.keys = [
      (builtins.readFile (../personal/ssh + "/${config.settings.profile}/id_rsa.pub"))
    ];
  };

  system.stateVersion = "21.11";

}
