# Clones public github repositories as part of activating home-manager
{ config, pkgs, lib, ... }:
let
  # TODO: make this parts of settings.nix?
  repos = [
    "git@github.com:malloc47/cv.git"
    "git@github.com:malloc47/malloc47.github.com.git"
  ];
  path = "src";
in
{
  home.activation = {
    personalRepos = lib.hm.dag.entryAfter ["installPackages"] (lib.concatStrings (
      map
        ( repo: let
          # Convert from git@ to https so we can clone without ssh, and then
          # update remote to point back to git@ later for interactive use
          https-url = ("https://github.com/" +
                       (lib.strings.removePrefix "git@github.com:" repo));
          target-path = lib.removeSuffix ".git" (builtins.baseNameOf repo);
        in
          ''
            if [ ! -d ~/${path}/${target-path} ]; then
              run ${lib.getExe pkgs.git} clone ${https-url} ~/${path}/${target-path}
              run ${lib.getExe pkgs.git} --git-dir ~/${path}/${target-path}/.git remote set-url origin ${repo}
            fi
          '')
        repos
    ));
  };
}
