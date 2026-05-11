{
  config,
  lib,
  pkgs,
  pkgs-unstable ? pkgs,
  ...
}:
let
  cfg = config.programs.shell-personal;
  inherit (pkgs) stdenv;
in
{
  imports = [ ../../modules/settings.nix ];

  options.programs.shell-personal = {
    enable = lib.mkEnableOption "personal zsh + tmux + theme-toggle config";
  };

  config = lib.mkIf cfg.enable {
    stylix.targets.tmux.enable = false;

    programs.tmux = {
      enable = true;
      package = pkgs-unstable.tmux;
      terminal = "tmux-256color";
      shortcut = "u";
      escapeTime = 0;
      historyLimit = 50000;
      mouse = true;
      extraConfig = ''
        set -ag terminal-overrides ",xterm*:Tc:smcup@:rmcup@"
        set -ag terminal-overrides ",*256col*:Tc"
        # OSC 52 clipboard passthrough: lets tmux mouse selection reach the
        # outer terminal's clipboard (Ghostty → macOS).  `set-clipboard on`
        # makes tmux emit OSC 52 on its own copy operations; the `clipboard`
        # terminal-feature tells tmux the outer terminal supports OSC 52
        # (tmux-256color from ncurses omits the Ms capability otherwise).
        set -s set-clipboard on
        set -as terminal-features ',*:clipboard'
        # tmux's own set-clipboard emit (copy-mode → Ms) emits with an empty
        # selection flag (`\E]52;;<b64>\a`), which Ghostty silently ignores.
        # Inner-program OSC 52 with explicit `c` flag works fine, so route
        # copy-mode through copy-pipe to emit OSC 52 ourselves with `c`.
        bind-key -T copy-mode-vi MouseDragEnd1Pane send -X copy-pipe-and-cancel '{ printf "\033]52;c;"; base64 -w0; printf "\a"; } > #{pane_tty}'
        bind-key -T copy-mode    MouseDragEnd1Pane send -X copy-pipe-and-cancel '{ printf "\033]52;c;"; base64 -w0; printf "\a"; } > #{pane_tty}'
        bind-key -T copy-mode-vi Enter             send -X copy-pipe-and-cancel '{ printf "\033]52;c;"; base64 -w0; printf "\a"; } > #{pane_tty}'
        bind-key -T copy-mode    Enter             send -X copy-pipe-and-cancel '{ printf "\033]52;c;"; base64 -w0; printf "\a"; } > #{pane_tty}'
        # Disabled: CSI-u encoding breaks multi-line paste in Claude Code
        # (anthropics/claude-code#43169). Re-enable when Claude fixes its
        # bracketed paste tokenizer.
        # set -g extended-keys on
        # set -as terminal-features 'tmux-256color:extkeys'
      '';
    };

    # Toggle between light and dark solarized across all tools.
    # State is tracked in ~/.config/theme-mode ("light" or "dark").
    home.file."toggle-theme" = {
      target = "bin/toggle-theme";
      executable = true;
      text = ''
        #!/usr/bin/env bash
        set -e

        MODE_FILE="$HOME/.config/theme-mode"
        CURRENT=$(cat "$MODE_FILE" 2>/dev/null || echo "light")

        if [ "$CURRENT" = "dark" ]; then
          NEW="light"
        else
          NEW="dark"
        fi

        mkdir -p "$(dirname "$MODE_FILE")"
        echo "$NEW" > "$MODE_FILE"

        echo "Switching to $NEW mode..."

        # 1. macOS: toggle system dark mode (Ghostty follows automatically)
        if [ "$(uname)" = "Darwin" ]; then
          if [ "$NEW" = "dark" ]; then
            osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'
          else
            osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to false'
          fi
        fi

        # 2. Linux: set XDG color-scheme portal (Ghostty follows)
        if [ "$(uname)" = "Linux" ] && command -v dbus-send >/dev/null 2>&1; then
          SCHEME=$( [ "$NEW" = "dark" ] && echo 1 || echo 2 )
          dbus-send --session --type=method_call \
            --dest=org.freedesktop.portal.Desktop \
            /org/freedesktop/portal/desktop \
            org.freedesktop.impl.portal.Settings.SetValue \
            string:"org.freedesktop.appearance" \
            string:"color-scheme" \
            variant:uint32:$SCHEME 2>/dev/null || true
        fi

        # 3. Switch Emacs theme
        emacsclient -e "(jw/apply-theme-mode \"$NEW\")" 2>/dev/null || true

        echo "Done."
      '';
    };

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
          if [ $TERM = "dumb" ] || [ $TERM = "tramp" ] ; then
            unset RPROMPT
            unset RPS1
            PS1="$ "
            unsetopt zle
            unsetopt rcs
            return
          fi

          hg() { history | grep $1 }
          pg() { ps aux | grep $1 }
          ns() {NIX_SHELL_PACKAGES="''${NIX_SHELL_PACKAGES:+$NIX_SHELL_PACKAGES }$*" nix shell $(print ''${*/#/nixpkgs\#})}
          nr() { nix run "nixpkgs#$1" -- ''${@:2} }

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

          # Prompt: λ [host:]path [· (branch ✘)] [· nix]
          # Inside a git repo, path is shown relative to repo parent (e.g. config/modules)
          # Outside a git repo, path is shown relative to ~ (e.g. ~/Documents)
          autoload -Uz vcs_info
          zstyle ':vcs_info:git:*' check-for-changes true
          zstyle ':vcs_info:git:*' unstagedstr '%F{red} ✘%f'
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
              host_part="%F{cyan}${
                if config.settings.displayName != null then config.settings.displayName else "%m"
              }%f:"
            fi
            local git_part=""
            if [[ -n "$vcs_info_msg_0_" ]]; then
              git_part=" $vcs_info_msg_0_"
            fi
            local nix_part=""
            if [[ -n "$NIX_SHELL_PACKAGES" ]]; then
              nix_part=" %F{242}·%f %F{red}[+''${NIX_SHELL_PACKAGES// / +}]%f"
            elif [[ -n "$IN_NIX_SHELL" ]]; then
              nix_part=" %F{242}·%f %F{red}nix%f"
            elif [[ "$PATH" == */nix/store/* ]]; then
              local -aU _nix_pkgs=()
              local _p
              for _p in ''${(s.:.)PATH}; do
                if [[ "$_p" == /nix/store/*/bin ]]; then
                  local _name=''${''${_p%/bin}:t}
                  _name=''${_name#*-}
                  _name=''${_name%%-[0-9]*}
                  _nix_pkgs+=("+$_name")
                fi
              done
              if (( ''${#_nix_pkgs} )); then
                nix_part=" %F{242}·%f %F{red}[''${(j. .)_nix_pkgs}]%f"
              fi
            fi
            PROMPT="''${lambda_color}λ''${lambda_color:+%f} ''${host_part}$(_prompt_pwd)''${git_part}''${nix_part} ▸ "
          }

          nixos-deploy() {
            nixos-rebuild switch --flake ~/src/config#$1 --target-host $1 --build-host $1 --fast --use-remote-sudo
          }
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

        ns() { NIX_SHELL_PACKAGES="''${NIX_SHELL_PACKAGES:+$NIX_SHELL_PACKAGES }$*" nix shell "''${@/#/nixpkgs#}"; }
        nr() { nix run "nixpkgs#$1" -- ''${@:2}; }

        if [[ $TERM == "dumb" ]]; then
            export PS1="$ "
        else
            HOST_PART=""
            if [[ -n "$SSH_CONNECTION" ]]; then
              HOST_PART="\[\e[36m\]${
                if config.settings.displayName != null then config.settings.displayName else "\\h"
              }\[\e[0m\]:"
            fi
            export PS1="λ ''${HOST_PART}\w \$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/') ▸ "
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
  };
}
