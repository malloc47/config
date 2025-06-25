{ config, osConfig, pkgs, ... }:
with pkgs.lib;
{
  imports = [
    ../modules/settings.nix
    ./emacs.nix
    ./git.nix
    ./shell.nix
    ./terminal.nix
    ./wm.nix
  ];

  settings = osConfig.settings;

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs.nix;

  home.packages = with pkgs; [
    anki
    aspell
    aspellDicts.en
    chromium
    clj-kondo
    cljfmt
    clojure
    dmenu
    evince
    feh
    ffmpeg
    firefox
    gcc
    gimp
    gitAndTools.pre-commit
    gnumake
    graphviz
    home-manager
    jq
    killall
    leiningen
    moreutils
    nixfmt-rfc-style
    nixos-generators
    nodePackages.mermaid-cli
    nodejs
    pandoc
    protobuf
    pv
    pyright
    (python3.withPackages (ps: with ps; [virtualenv wheel setuptools numpy pandas]))
    ripgrep
    sbt
    scrot
    sqlite
    term-do
    tree
    unzip
    zip
  ];

  home.sessionPath = ["$HOME/bin"];

  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.file.".inputrc".source = ./.inputrc;
  home.file.".lein" = { source = ./.lein; recursive = true; };
  home.file.".clojure" = { source = ./.clojure; recursive = true; };
  home.file.".sbt" = { source = ./.sbt; recursive = true; };
  xdg.configFile.".user-dirs.dirs".source = ./.user-dirs.dirs;

  home.file."wifi" = mkIf (!config.settings.vm) {
    target = "bin/wifi";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ${config.settings.terminal} -e nmtui
    '';
  };

  home.file."brightness" = mkIf (!config.settings.vm) {
    target = "bin/brightness";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      sudo bash -c "echo $1 > /sys/class/backlight/mba6x_backlight/brightness"
    '';
  };

  home.file."setup-shared" = mkIf (config.settings.vm) {
    target = "bin/setup-shared";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      mkdir $HOME/shared 2>/dev/null
      vmhgfs-fuse .host:/shared $HOME/shared -o subtype=vmhgfs-fuse
    '';
  };

  programs.java.enable = true;

  home.stateVersion = "25.05";
}
