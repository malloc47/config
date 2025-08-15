{ config, pkgs, ... }:
{
  imports = [ ../modules/settings.nix ];

  programs.git = {
    enable = true;
    userName = config.settings.name;
    userEmail = config.settings.email;
    aliases = {
      s = "status -s -uno";
      gl = "log --oneline --graph";
    };
    ignores = [
      ".#*"
      "*.desktop"
      "*.lock"
    ];
    lfs.enable = true;
    extraConfig = {
      branch.autosetuprebase = "never";
      push.default = "simple";
      core.pager = "less -F -X";
      pull.ff = "only";
      init.defaultBranch = "main";
    };
  };
}
