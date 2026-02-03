{ config, pkgs, ... }:
{
  imports = [ ../modules/settings.nix ];

  programs.git = {
    enable = true;
    settings = {
      aliases = {
        s = "status -s -uno";
        gl = "log --oneline --graph";
      };
      user.name = config.settings.name;
      user.email = config.settings.email;
      branch.autosetuprebase = "never";
      push.default = "simple";
      core.pager = "less -F -X";
      pull.ff = "only";
      init.defaultBranch = "main";
    };
    ignores = [
      ".#*"
      "*.desktop"
      "*.lock"
    ];
    lfs.enable = true;
  };
}
