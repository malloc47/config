{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../modules/settings.nix
    ./git.nix
    ./shell.nix
    ./ghostty-terminfo.nix
  ];

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs.nix;

  home.packages = with pkgs; [
    home-manager
    jq
    killall
    moreutils
    # Build mosh from the head of mobile-shell/mosh#1104, which extends
    # OSC 52 forwarding to handle the empty-selector form tmux emits and
    # preserves the selection-target parameter end-to-end.  The diff
    # against 1.4.0 is too large for a simple .patch overlay, so we swap
    # in the PR branch as source.  Pinned by SHA so the build is stable
    # if the branch is force-pushed.
    (mosh.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "mgulick";
        repo = "mosh";
        rev = "e8337eff281f1cbecf6292dac64598ede7277928";
        hash = "sha256-bWLAkZoH7ralcUxwt8KSSKSEh466NGTaaruFM7x99aE=";
      };
      # nixpkgs cherry-picks upstream commit eee1a8cf into 1.4.0; that
      # commit is already in master / the PR branch, so drop the
      # cherry-pick patch to avoid "reversed patch detected" failures.
      patches = builtins.filter (p: !(lib.hasInfix "eee1a8cf" (toString p))) (old.patches or [ ]);
    }))
    pv
    ripgrep
    tree
    unzip
    zip
  ];

  home.sessionPath = [ "$HOME/bin" ];

  home.sessionVariables = {
    EDITOR = lib.mkForce "emacsclient -t";
  };

  home.file.".hushlogin".text = "";
  home.file.".inputrc".source = ./.inputrc;
  xdg.configFile.".user-dirs.dirs".source = ./.user-dirs.dirs;

  services.ssh-agent.enable = true;

  home.stateVersion = "25.05";
}
