# Clones git repositories as part of activating home-manager. Each entry
# is `{ url = "git@host:org/repo"; target = "/abs/path"; }`. The initial
# clone uses HTTPS (no ssh keys required at activation), then rewrites
# origin to the SSH URL for interactive use.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.services.repo-clone;
in
{
  imports = [ ../../modules/settings.nix ];

  options.services.repo-clone = {
    enable = lib.mkEnableOption "git repository cloning at home-manager activation";

    repos = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            url = lib.mkOption {
              type = lib.types.str;
              description = "Git remote URL (typically `git@host:org/repo`).";
            };
            target = lib.mkOption {
              type = lib.types.str;
              description = "Absolute filesystem path to clone into.";
            };
          };
        }
      );
      default = config.settings.repositories;
      defaultText = lib.literalExpression "config.settings.repositories";
      description = "Repositories to clone if their target path does not yet exist.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.activation.personalRepos = lib.hm.dag.entryAfter [ "installPackages" ] (
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
        ) cfg.repos
      )
    );
  };
}
