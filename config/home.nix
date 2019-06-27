{ config, pkgs, ... }:

let
  mod = "Mod4";
in
with pkgs.lib;
{
  imports = [
    ../modules/settings.nix
  ];

  nixpkgs.config = import ./nixpkgs.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs.nix;
  nixpkgs.overlays = [(import ../pkgs/default.nix)];

  home.packages = with pkgs; [
    alacritty
    autocutsel
    evince
    feh
    ffmpeg
    firefox
    gitAndTools.pre-commit
    google-chrome
    home-manager
    ispell
    jq
    killall
    leiningen
    openjdk8
    python37
    sbt
    unzip
    zip
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
      (set-frame-font "${config.settings.fontName}-${toString config.settings.fontSize}")
      (setq default-frame-alist '((font . "${config.settings.fontName}-${toString config.settings.fontSize}")))
    '';
  };

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    shortcut = "u";
  };

  programs.ssh = {
    enable = true;
    controlPath = "~/.ssh/master-%C";
  };

  home.file."id_rsa" = {
    source = ./. + "/../personal/ssh/${config.settings.profile}/id_rsa";
    target = ".ssh/id_rsa";
  };

  home.file."id_rsa.pub" = {
    source = ./. + "/../personal/ssh/${config.settings.profile}/id_rsa.pub";
    target = ".ssh/id_rsa.pub";
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
          fonts = ["${config.settings.fontName} ${toString config.settings.fontSize}"];
        }
      ];
      keybindings = mkOptionDefault (
        {
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
          "${mod}+Shift+r" = "nop";
          "${mod}+v" = "nop";
          "${mod}+e" = "nop";
          "${mod}+s" = "nop";
        }
        // optionalAttrs (!config.settings.vm)
        {
          "${mod}+equal" = "workspace next";
          "${mod}+minus" = "workspace prev";
          "${mod}+grave" = "workspace 1";
          "${mod}+Shift+Control+L" = "exec i3lock";
          "XF86AudioRaiseVolume" = "exec --no-startup-id amixer sset Master 5%+ unmute";
          "XF86AudioLowerVolume" = "exec --no-startup-id amixer sset Master 5%- unmute";
          "XF86AudioMute" = "exec --no-startup-id amixer sset Master toggle";
        });
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
    # inexplicably xserver wrapper doesn't set the background image
    extraConfig = ''
      focus_wrapping no
      exec_always "if [[ -e $HOME/.background-image ]]; then feh --bg-scale $HOME/.background-image ; fi"
    '';
  };

  home.file.".Xresources" = {
    target = ".Xresources";
    text = ''
      xterm*faceName: ${config.settings.fontName}
      xterm*faceSize: ${toString config.settings.fontSize}
    '';
  };

  xsession.initExtra =
    if (config.settings.xkbFile != "none" ) then
      let
        xkbFile = ../xkb + "/${config.settings.xkbFile}.xkb";
        compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${xkbFile} $out
       '';
      in
        "${pkgs.xorg.xkbcomp}/bin/xkbcomp ${compiledLayout} $DISPLAY"
    else
      "";

  services.screen-locker = {
    enable = !config.settings.vm;
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
      "ns" = "nix-shell --command zsh";
    };
    initExtra = let
      cdpath = "$HOME/src" +
        optionalString (config.settings.profile != "malloc47")
          " $HOME/src/${config.settings.profile}";
    in
    ''
      hg() { history | grep $1 }
      pg() { ps aux | grep $1 }

      function chpwd() {
        emulate -L zsh
        ls
      }

      cdpath=(${cdpath})
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
      ".." = "cd ..";
      "..." = "cd ../../";
      "...." = "cd ../../../";
      "....." = "cd ../../../../";
      "......" = "cd ../../../../../";
      "ll" = "ls -al";
      "ns" = "nix-shell --command zsh";
    };
    initExtra = ''
      hg() { history | grep "$1"; }
      pg() { ps aux | grep "$1"; }
      cd() { if [[ -n "$1" ]]; then builtin cd "$1" && ls; else builtin cd && ls; fi }
    '';
    sessionVariables = {
      CDPATH = ".:~/src/" +
        optionalString (config.settings.profile != "malloc47")
        ":~/src/${config.settings.profile}";
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
  xdg.configFile."alacritty/alacritty.yml".source = ./.alacritty.yml;
  xdg.configFile."i3status/config".source = ./.i3status.conf;
  xdg.configFile.".user-dirs.dirs".source = ./.user-dirs.dirs;

  home.file."wifi" = mkIf (!config.settings.vm) {
    target = "bin/wifi";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ${config.settings.terminal} -e nmtui
    '';
  };
}
