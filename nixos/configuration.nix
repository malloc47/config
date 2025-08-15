{
  config,
  pkgs,
  lib,
  options,
  ...
}:

let
  hm-src = {
    url = "https://github.com/nix-community/home-manager/archive/d0bbd221482c2713cccb80220f3c9d16a6e20a33.tar.gz";
    sha256 = "1iqfq5lz3102cp3ryqqqs2hr2bdmwn0mdajprh1ls5h5nsfkigs1";
  };
in
{
  imports = [
    ../modules/settings.nix
    "${builtins.fetchTarball hm-src}/nixos"
  ];

  nixpkgs.config = import ../config/nixpkgs.nix;
  nixpkgs.overlays = [ (import ../pkgs/default.nix) ];

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

  nix.nixPath = options.nix.nixPath.default ++ [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];

  nix.settings.trusted-users = [ "@wheel" ];

  nix.extraOptions = ''
    tarball-ttl = 604800
    experimental-features = nix-command flakes
  '';

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    docker-compose
    exfat
    feh
    man-pages
    man-pages-posix
    vim
    wget
    xorg.xkill
  ];

  documentation.enable = true;
  documentation.man.enable = true;
  documentation.dev.enable = true;

  # I rely on this behavior, but it should probably be moved to
  # home.nix at some point
  environment.homeBinInPath = true;

  environment.wordlist.enable = true;

  fonts.packages = with pkgs; [
    corefonts
    geosanslight
    inconsolata-unstable
    libertine
    libre-baskerville
    emacs-all-the-icons-fonts
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Enable sound.
  services.pulseaudio.enable = true;
  services.pipewire.enable = false;
  # This is only needed for containers that want to share the
  # pulse-native socket
  # hardware.pulseaudio.extraClientConf = ''
  #   enable-shm = no
  # '';

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  programs.ssh.startAgent = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = false;

  # 25.05 bug: https://github.com/nixos/nixpkgs/issues/422385
  virtualisation.lxd.enable = !config.settings.vm;
  virtualisation.lxc.lxcfs.enable = !config.settings.vm;

  # Required because /run/user/1000 tempfs is too small for docker
  services.logind.extraConfig = ''
    RuntimeDirectorySize=8G
  '';

  services.xserver.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.windowManager.i3.enable = true;
  services.xserver.autorun = true;
  services.xserver.autoRepeatDelay = 250;
  services.displayManager.defaultSession = "none+i3";

  security.sudo.wheelNeedsPassword = false;

  programs.zsh.enable = true;

  services.journald.extraConfig = ''
    SystemMaxUse=2G
  '';

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
  };

  home-manager.useUserPackages = true;
  home-manager.users.${config.settings.username} = {
    imports = [ ../config/home.nix ];
    home.pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
    };
    home.stateVersion = lib.mkForce "21.11";
  };

  system.stateVersion = "21.11";

}
