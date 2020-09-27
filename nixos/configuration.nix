{ config, pkgs, options, ... }:

let
  hm-src = {
    url = "https://github.com/rycee/home-manager/archive/e6f96b6aa3e99495f9f6f3488ecf78dd316e5bec.tar.gz" ;
    sha256 = "1xvxqw5cldlhcl7xsbw11n2s3x1h2vmbm1b9b69a641rzj3srg11";
  };
in
{
  imports = [
    ../modules/settings.nix
    "${builtins.fetchTarball hm-src}/nixos"
  ];

  nixpkgs.config = import ../config/nixpkgs.nix;
  nixpkgs.overlays = [(import ../pkgs/default.nix)];

  # Using https://nixos.wiki/wiki/Overlays to let the local nix tools
  # get the same overlays as we define in this file
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

  nix.nixPath =
    options.nix.nixPath.default ++
    [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    feh
    vim
    wget
    xorg.xkill
    docker_compose
    exfat
  ];

  # I rely on this behavior, but it should probably be moved to
  # home.nix at some point
  environment.homeBinInPath = true;

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

  # Required because /run/user/1000 tempfs is too small for docker
  services.logind.extraConfig = ''
    RuntimeDirectorySize=8G
  '';

  services.xserver.enable = true;

  services.xserver.desktopManager.xterm.enable = false;

  services.xserver.displayManager.defaultSession = "none+i3";

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

  system.stateVersion = "20.03";

}
