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
    dotDir = "${config.xdg.configHome}/zsh";
    history = {
      expireDuplicatesFirst = true;
      path = "${config.xdg.dataHome}/zsh/zsh_history";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [
        "lein"
        "sudo"
      ];
      theme = "";
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
    initContent =
      let
        cdpath =
          "$HOME/src"
          + pkgs.lib.optionalString (
            config.settings.profile != "malloc47"
          ) " $HOME/src/${config.settings.profile}";
      in
      ''
        hg() { history | grep $1 }
        pg() { ps aux | grep $1 }
        bindkey -s "^[x" 'term-do^M'
        term-do() {command term-do "$*" && builtin cd $(cat ~/.term-do.d/pwd)}
        ns() { if [ -f "flake.nix" ] ; then nix develop --command zsh ; else nix-shell ; fi }



        materialize() {
          if [ -f "$1.link" ] ; then
            rm $1
            mv $1.link $1
          else
            mv $1 $1.link
            cp $1.link $1
            chmod +w $1
          fi
        }

        function chpwd() {
          emulate -L zsh
          ls
        }

        cdpath=(${cdpath})

        # Prompt: λ [host:]path [· (branch[✘])] [· nix]
        # Inside a git repo, path is shown relative to repo parent (e.g. config/modules)
        # Outside a git repo, path is shown relative to ~ (e.g. ~/Documents)
        autoload -Uz vcs_info
        zstyle ':vcs_info:git:*' check-for-changes true
        zstyle ':vcs_info:git:*' unstagedstr '%F{red}✘%f'
        zstyle ':vcs_info:git:*' formats '(%F{yellow}%b%f%u)'
        zstyle ':vcs_info:git:*' actionformats '(%F{yellow}%b%f%u %a)'
        zstyle ':vcs_info:*' enable git

        _prompt_pwd() {
          local git_root
          git_root=$(git rev-parse --show-toplevel 2>/dev/null)
          if [[ -n "$git_root" ]]; then
            local repo_name="''${git_root:t}"
            local rel="''${PWD#$git_root}"
            echo "''${repo_name}''${rel}"
          else
            print -P '%~'
          fi
        }

        precmd() {
          local last_exit=$?
          vcs_info
          local lambda_color=""
          if [[ $last_exit -ne 0 ]]; then
            lambda_color="%F{red}"
          fi
          local host_part=""
          if [[ -n "$SSH_CONNECTION" ]]; then
            host_part="%F{cyan}%m%f:"
          fi
          local git_part=""
          if [[ -n "$vcs_info_msg_0_" ]]; then
            git_part=" $vcs_info_msg_0_"
          fi
          local nix_part=""
          if [[ -n "$IN_NIX_SHELL" || "$PATH" == */nix/store/* ]]; then
            nix_part=" %F{242}·%f %F{red}nix%f"
          fi
          PROMPT="''${lambda_color}λ''${lambda_color:+%f} ''${host_part}$(_prompt_pwd)''${git_part}''${nix_part} "
        }

        nixos-deploy() {
          nixos-rebuild switch --flake ~/src/config#$1 --target-host $1 --build-host $1 --fast --use-remote-sudo
        }

        if [[ $TERM == "dumb" ]]; then
            export PROMPT="$ "
        fi
      '';
    sessionVariables = {
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=10";
    };
  };

  programs.bash = {
    enable = true;
    historyFile = "${config.xdg.dataHome}//bash/bash_history";
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

      materialize() {
        if [ -f "$1.link" ] ; then
          rm $1
          mv $1.link $1
        else
          mv $1 $1.link
          cp $1.link $1
          chmod +w $1
        fi
      }

      if [[ $TERM == "dumb" ]]; then
          export PS1="$ "
      else
          HOST_PART=""
          if [[ -n "$SSH_CONNECTION" ]]; then
            HOST_PART="\[\e[36m\]\h\[\e[0m\]:"
          fi
          export PS1="λ ''${HOST_PART}\w \$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/') "
      fi

      nixos-deploy() {
        nixos-rebuild switch --flake ~/src/config#$1 --target-host $1 --build-host $1 --fast --use-remote-sudo
      }
    '';
    sessionVariables = {
      CDPATH =
        ".:~/src/"
        + pkgs.lib.optionalString (
          config.settings.profile != "malloc47"
        ) ":~/src/${config.settings.profile}";
    };
    shellOptions = [
      "autocd"
      "cdspell"
      "globstar" # bash >= 4
      "cmdhist"
      "nocaseglob"
      "histappend"
      "extglob"
    ];
  };
}
