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
    anki
    autocutsel
    clojure
    evince
    feh
    ffmpeg
    firefox
    gimp
    gitAndTools.pre-commit
    google-chrome
    home-manager
    ispell
    jq
    killall
    kitty
    leiningen
    moreutils
    nodejs
    openjdk8
    pandoc
    protobuf
    pv
    (python38.withPackages (ps: with ps; [virtualenv wheel]))
    sbt
    scrot
    unzip
    zip
    zoom-us
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
      (set-frame-font "${config.settings.fontName}-${head (splitString "." (toString config.settings.fontSize))}")
      (setq default-frame-alist '((font . "${config.settings.fontName}-${head (splitString "." (toString config.settings.fontSize))}")))
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
          fonts = {
            names = [ config.settings.fontName ];
            size = config.settings.fontSize;
          };
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
      xterm*faceSize: ${head (splitString "." (toString config.settings.fontSize))}
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
      # TODO: bring this file under nix control
      core.excludesfile = "/home/jwaggoner/.config/git/gitignore";
      pull.ff = "only";
    };
  };

  services.dropbox.enable = true;

  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.inconsolata-unstable;
      name = "${config.settings.fontName}";
    };
    settings = {

      font_size = toString config.settings.fontSize;

      background = "#fdf6e3";
      foreground = "#657b83";
      cursor = "#586e75";

      selection_background = "#475b62";
      selection_foreground = "#eae3cb";

      color0 = "#073642";
      color8 = "#002b36";

      color1 = "#dc322f";
      color9 = "#cb4b16";

      color2 = "#859900";
      color10 = "#586e75";

      color3 = "#b58900";
      color11 = "#657b83";

      color4 = "#268bd2";
      color12 = "#839496";

      color5 = "#d33682";
      color13 = "#6c71c4";

      color6 = "#2aa198";
      color14 = "#93a1a1";

      color7 = "#eee8d5";
      color15 = "#fdf6e3";
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
      theme = "lambda";
    };
    shellAliases = {
      "ll" = "ls -al";
      "ns" = "nix-shell --command zsh";
    };
    plugins = [
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

      if [[ -n "$IN_NIX_SHELL" ]]; then
        export PS1="${"\${PS1}%F{red}ns%f"} "
      fi
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

  home.file."brightness" = mkIf (!config.settings.vm) {
    target = "bin/brightness";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      sudo bash -c 'echo $1 > /sys/class/backlight/mba6x_backlight/brightness'
    '';
  };
}
