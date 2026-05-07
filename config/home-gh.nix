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

  xdg.configFile."gh/hosts.yml".text = ''
    github.com:
        git_protocol: ssh
        users:
            malloc47:
        user: malloc47
  '';
}
