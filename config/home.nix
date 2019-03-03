{ config, pkgs, ... }:

let
  mod = "Mod4";
  compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${../xkb/macbook-modified.xkb} $out
  '';
in
{
  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    alacritty
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
        }
      ];
      keybindings = pkgs.lib.mkOptionDefault {
        "${mod}+p" = "exec ${pkgs.dmenu}/bin/dmenu_run";
        "${mod}+q" = "reload";
        "${mod}+Control+q" = "restart";
        "${mod}+Shift+q" = "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";
        "${mod}+Shift+c" = "kill";
        "${mod}+Return" = "exec alacritty";
        "${mod}+Shift+Return" = "exec alacritty -e tmux";
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
        "${mod}+Shift+N" = "exec \"xterm -e 'sudo nixos-rebuild switch; echo COMPLETE ; read -n 1 -s'\"";
        "${mod}+Shift+Control+L" = "exec i3lock";
        "${mod}+equal" = "workspace next";
        "${mod}+minus" = "workspace prev";
        "${mod}+grave" = "workspace 1";
        "${mod}+Shift+r" = "nop";
        "${mod}+v" = "nop";
        "${mod}+e" = "nop";
        "${mod}+s" = "nop";
        "XF86AudioRaiseVolume" = "exec --no-startup-id amixer sset Master 5%+ unmute";
        "XF86AudioLowerVolume" = "exec --no-startup-id amixer sset Master 5%- unmute";
        "XF86AudioMute" = "exec --no-startup-id amixer sset Master toggle";
      };
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

  xsession.initExtra = "${pkgs.xorg.xkbcomp}/bin/xkbcomp ${compiledLayout} $DISPLAY";

  services.screen-locker = {
    enable = true;
    lockCmd = "i3lock";
  };

  programs.git = {
    enable = true;
    userName = "Jarrell Waggoner";
    userEmail = "malloc47@gmail.com";
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

  home.file.".inputrc".source = ./.inputrc;
  home.file.".lein" = { source = ./.lein; recursive = true; };
  home.file.".sbt" = { source = ./.sbt; recursive = true; };
  home.file.".alacritty.yml" = { source = ./.alacritty.yml; target = ".config/alacritty/alacritty.yml"; };
  home.file.".i3status.conf" = { source = ./.i3status.conf; target = ".config/i3status/config"; };
  home.file.".user-dirs.dirs" = {source = ./.user-dirs.dirs; target = ".config/user-dirs.dirs";};
}
