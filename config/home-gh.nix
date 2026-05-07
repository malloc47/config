{
  config,
  pkgs,
  lib,
  ...
}:

let
  ghTokenGithub = config.age.secrets.gh-token-github.path;

  gh-wrapped = pkgs.symlinkJoin {
    name = "gh-with-agenix";
    paths = [ pkgs.gh ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/gh \
        --run 'if [ -z "''${GH_TOKEN:-}" ] && [ -r "${ghTokenGithub}" ]; then export GH_TOKEN="$(cat "${ghTokenGithub}")"; fi'
    '';
  };
in
{
  age.secrets.gh-token-github = {
    file = ../secrets/gh-token-github.age;
    path = "${config.home.homeDirectory}/.config/agenix/gh-token-github";
  };

  home.packages = [ (lib.hiPrio gh-wrapped) ];

  # Intentionally empty: any host listed here makes gh's startup
  # migration try to resolve a token via libsecret, which fails on
  # hosts without a running secret service. Env vars cover auth;
  # --hostname covers host targeting.
  xdg.configFile."gh/hosts.yml".text = "";
}
