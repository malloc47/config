{ config, pkgs, ... }:

let
  mod = "Mod4";
  compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${../xkb/macbook-modified.xkb} $out
  '';
in
{
  nixpkgs.config.allowUnfree = true;

  imports = [
    ../modules/settings.nix
  ];

  home.packages = with pkgs; [
    alacritty
    autocutsel
    evince
    feh
    firefox
    google-chrome
    home-manager
    jq
    python37
  ];

  programs.emacs.enable = true;
  services.emacs.enable = true;

  home.file.".emacs.d" = {
    source = ./.emacs.d;
    recursive = true;
  };

  home.file."fonts.el" = {
    target = ".emacs.d/config/fonts.el";
    text = ''
      (provide 'fonts)
      (set-default-font "Inconsolata-${toString config.settings.fontSize}")
      (setq default-frame-alist '((font . "Inconsolata-${toString config.settings.fontSize}")))
    '';
  };

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
  };

  xsession.enable = true;

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;
      bars = [
        {
          id = "bar-0";
          position = "top";
          fonts = ["Inconsolata ${toString config.settings.fontSize}"];
        }
      ];
      keybindings = pkgs.lib.mkOptionDefault (
      let
        keys = {
          "${mod}+p" = "exec ${pkgs.dmenu}/bin/dmenu_run";
          "${mod}+q" = "reload";
          "${mod}+Control+q" = "restart";
          "${mod}+Shift+q" = "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";
          "${mod}+Shift+c" = "kill";
          "${mod}+Return" = "exec ${config.settings.terminal}";
          "${mod}+Shift+Return" = "exec ${config.settings.terminal} -e tmux";
          "${mod}+Shift+e" = "exec emacsclient -c";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";
          "${mod}+h" = "focus left";
          "${mod}+u" = "focus parent";
          "${mod}+Shift+U" = "focus child";
          "${mod}+Shift+J" = "move down";
          "${mod}+Shift+K" = "move up";
          "${mod}+Shift+L" = "move right";
          "${mod}+Shift+H" = "move left";
          "${mod}+c" = "layout tabbed";
          "${mod}+x" = "split v";
          "${mod}+z" = "split h";
          "${mod}+space" = "layout toggle splitv splith tabbed";
          "${mod}+y" = "bar mode toggle";
          "${mod}+Shift+N" = "exec \"xterm -e 'sudo nixos-rebuild switch; read -s -k \\?COMPLETE'\"";
          "${mod}+Shift+Control+L" = "exec i3lock";
          "${mod}+Shift+r" = "nop";
          "${mod}+v" = "nop";
          "${mod}+e" = "nop";
          "${mod}+s" = "nop";
        };
        systemKeys = {
          "${mod}+equal" = "workspace next";
          "${mod}+minus" = "workspace prev";
          "${mod}+grave" = "workspace 1";
          "XF86AudioRaiseVolume" = "exec --no-startup-id amixer sset Master 5%+ unmute";
          "XF86AudioLowerVolume" = "exec --no-startup-id amixer sset Master 5%- unmute";
          "XF86AudioMute" = "exec --no-startup-id amixer sset Master toggle";
        };
      in
        if config.settings.vm then
          keys
        else
          keys // systemKeys
      );
      modes.resize = {
        "h" = "resize shrink width 10 px or 10 ppt";
        "j" = "resize grow height 10 px or 10 ppt";
        "k" = "resize shrink height 10 px or 10 ppt";
        "l" = "resize grow width 10 px or 10 ppt";
        "Escape" = "mode default";
        "Return" = "mode default";
      };
      window.titlebar = false;
    };
    extraConfig = ''
      focus_wrapping no
    '';
  };

  home.file.".Xresourcesn" = {
    target = ".Xresources";
    text = ''
      Xcursor.size: 32
      xterm*faceName: Inconsolata
      xterm*faceSize: ${toString config.settings.fontSize}
    '';
  };


  #xsession.initExtra = "${pkgs.xorg.xkbcomp}/bin/xkbcomp ${compiledLayout} $DISPLAY";

  services.screen-locker = {
    enable = true;
    lockCmd = "i3lock";
  };

  programs.git = {
    enable = true;
    userName = config.settings.name;
    userEmail = config.settings.email;
    aliases = {
      s = "status -s -uno";
      gl = "log --oneline --graph";
    };
    ignores = [".#*" "*.desktop" "*.lock"];
    extraConfig = {
      branch.autosetuprebase = "never";
      push.default = "simple";
    };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
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
      theme = "";
    };
    shellAliases = {
      "ll" = "ls -al";
    };
    initExtra = ''
      hg() { history | grep $1 }
      pg() { ps aux | grep $1 }

      function chpwd() {
        emulate -L zsh
        ls
      }

      cdpath=($HOME/src)
    '';
    sessionVariables = {
      EDITOR = "vim";
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10";
    };
  };

  programs.bash = {
    enable = true;
    historyFile = "\$HOME/.config/bash/.bash_history";
    shellAliases = {
      "ll" = "ls -al";
      ".." = "cd ..";
      "..." = "cd ../../";
      "...." = "cd ../../../";
      "....." = "cd ../../../../";
      "......" = "cd ../../../../../";
    };
    initExtra = ''
      hg() { history | grep "$1"; }
      pg() { ps aux | grep "$1"; }
      cd() { if [[ -n "$1" ]]; then builtin cd "$1" && ls; else builtin cd && ls; fi }
    '';
    sessionVariables = {
      CDPATH = "CDPATH=.:~/src/";
      EDITOR = "vim";
    };
    shellOptions = [
    "autocd" "cdspell" "dirspell" "globstar" # bash >= 4
    "cmdhist" "nocaseglob" "histappend" "extglob"];
  };

  systemd.user.services.autocutsel = {
    Unit.Description = "AutoCutSel";
    Install = {
      WantedBy = [ "default.target" ];
    };
    Service = {
      Type = "forking";
      Restart = "always";
      RestartSec = 2;
      ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
      ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
    };
  };

  home.file.".inputrc".source = ./.inputrc;
  home.file.".lein" = { source = ./.lein; recursive = true; };
  home.file.".sbt" = { source = ./.sbt; recursive = true; };
  home.file.".alacritty.yml" = { source = ./.alacritty.yml; target = ".config/alacritty/alacritty.yml"; };
  home.file.".i3status.conf" = { source = ./.i3status.conf; target = ".config/i3status/config"; };
  home.file.".user-dirs.dirs" = {source = ./.user-dirs.dirs; target = ".config/user-dirs.dirs";};

  home.file."wifi" = pkgs.lib.mkIf (!config.settings.vm) {
    target = "bin/wifi";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ${config.settings.terminal} -e nmtui
    '';
  };
}
