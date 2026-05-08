# Clones public github repositories as part of activating home-manager
{
  config,
  pkgs,
  lib,
  ...
}:
{
  home.activation = {
    personalRepos = lib.hm.dag.entryAfter [ "installPackages" ] (
      lib.concatStrings (
        map (
          repo:
          let
            # Convert from git@ to https so we can clone without ssh, and then
            # update remote to point back to git@ later for interactive use
            https-url = ("https://github.com/" + (lib.strings.removePrefix "git@github.com:" repo.url));
          in
          ''
            if [ ! -d ${repo.target} ]; then
              run ${lib.getExe pkgs.git} clone ${https-url} ${repo.target}
              run ${lib.getExe pkgs.git} --git-dir ${repo.target}/.git remote set-url origin ${repo.url}
            fi
          ''
        ) config.settings.repositories
      )
    );
  };
}
