{ config, pkgs, ... }:
let
  inherit (pkgs) stdenv;
in
{
  imports = [ ../modules/settings.nix ];

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    defaultKeymap = "emacs";
    dotDir = ".config/zsh";
    history = {
      expireDuplicatesFirst = true;
      path = ".config/zsh/.zsh_history";
    };
    oh-my-zsh = {
      enable = true;
      plugins = ["lein" "sudo"];
      theme = "lambda";
    };
    shellAliases = {
      "ll" = "ls -al";
    };
    plugins = pkgs.lib.mkIf (!stdenv.isDarwin) [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.1.0";
          sha256 = "0snhch9hfy83d4amkyxx33izvkhbwmindy0zjjk28hih1a9l2jmx";
        };
      }
    ];
    initContent = let
      cdpath = "$HOME/src" +
        pkgs.lib.optionalString (config.settings.profile != "malloc47")
          " $HOME/src/${config.settings.profile}";
    in
    ''
      hg() { history | grep $1 }
      pg() { ps aux | grep $1 }
      bindkey -s "^[x" 'term-do^M'
      term-do() {command term-do "$*" && builtin cd $(cat ~/.term-do.d/pwd)}
      ns() { if [ -f "flake.nix" ] ; then nix develop --command zsh ; else nix-shell ; fi }

      function chpwd() {
        emulate -L zsh
        ls
      }

      cdpath=(${cdpath})

      if [[ -n "$IN_NIX_SHELL" ]]; then
        export PS1="${"\${PS1}%F{red}ns%f"} "
      fi
    '';
    sessionVariables = {
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10";
    };
  };

  programs.bash = {
    enable = true;
    historyFile = "\$HOME/.config/bash/.bash_history";
    shellAliases = {
      ".." = "cd ..";
      "..." = "cd ../../";
      "...." = "cd ../../../";
      "....." = "cd ../../../../";
      "......" = "cd ../../../../../";
      "ll" = "ls -al";
    };
    initExtra = ''
      hg() { history | grep "$1"; }
      pg() { ps aux | grep "$1"; }
      cd() { if [[ -n "$1" ]]; then builtin cd "$1" && ls; else builtin cd && ls; fi }
      term-do() {
        command term-do "$*"
        builtin cd $(cat ~/.term-do.d/pwd)
      }
      ns() { if [ -f "flake.nix" ] ; then nix develop --command zsh ; else nix-shell ; fi }
      export PS1="λ \w \$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/') "
    '';
    sessionVariables = {
      CDPATH = ".:~/src/" +
        pkgs.lib.optionalString (config.settings.profile != "malloc47")
        ":~/src/${config.settings.profile}";
    };
    shellOptions = [
      "autocd" "cdspell" "globstar" # bash >= 4
      "cmdhist" "nocaseglob" "histappend" "extglob"
    ];
  };
}
