{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
{
  imports = [
    ./emacs.nix
    ./repos.nix
  ];

  home.packages = with pkgs; [
    (aspellWithDicts (
      dicts: with dicts; [
        en
      ]
    ))
    cargo
    clj-kondo
    cljfmt
    clojure
    ffmpeg
    gcc
    gh
    pre-commit
    gnumake
    go-task
    graphviz
    leiningen
    nixfmt-rfc-style
    nixos-generators
    nodePackages.mermaid-cli
    nodejs
    pandoc
    protobuf
    pyright
    (python3.withPackages (
      ps: with ps; [
        virtualenv
        wheel
        setuptools
        numpy
        pandas
      ]
    ))
    sbt
    sqlite
    wordnet
  ];

  programs.java.enable = true;

  home.file.".lein" = {
    source = ./.lein;
    recursive = true;
  };
  home.file.".clojure" = {
    source = ./.clojure;
    recursive = true;
  };
  home.file.".sbt" = {
    source = ./.sbt;
    recursive = true;
  };
}
