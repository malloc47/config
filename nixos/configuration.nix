{ config, pkgs, ... }:

{
  nixpkgs.config = import ../pkgs/config.nix;

  nixpkgs.overlays = [
    (import ../pkgs/default.nix)
  ];

  imports = [
    ../modules/settings.nix
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];

  time.timeZone = "America/Chicago";

  environment.systemPackages = with pkgs; [
    feh
    vim
    wget
    xorg.xkill
    docker_compose
  ];

  fonts.fonts = with pkgs; [
    corefonts
    geosanslight
    inconsolata
    libertine
    libre-baskerville
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  programs.ssh.startAgent = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = false;

  services.xserver.enable = true;

  services.xserver.desktopManager = {
    default = "none";
    xterm.enable = false;
  };

  services.xserver.windowManager.i3.enable = true;

  services.xserver.autorun = true;

  security.sudo.wheelNeedsPassword = false;

  programs.zsh.enable = true;

  users.users.${config.settings.username} = {
    isNormalUser = true;
    createHome = true;
    home = "/home/${config.settings.username}";
    description = "Jarrell Waggoner";
    extraGroups = ["audio" "docker" "networkmanager" "wheel"];
    uid = 1000;
    shell = pkgs.zsh;
  };

  home-manager.users.${config.settings.username} = import ../config/home.nix ;

  system.stateVersion = "18.09";

}
