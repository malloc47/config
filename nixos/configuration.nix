{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    (import ../pkgs/default.nix)
  ];

  time.timeZone = "America/Chicago";

  environment.systemPackages = with pkgs; [
    vim
    wget
    xorg.xkill
  ];

  fonts.fonts = with pkgs; [
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
  sound.mediaKeys.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  programs.ssh.startAgent = true;

  services.xserver.enable = true;

  services.xserver.windowManager.i3.enable = true;

  services.xserver.autorun = true;

  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "18.09";

}
