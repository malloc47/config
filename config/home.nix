{ config, osConfig, pkgs, ... }:

let
  mod = "Mod4";
in
with pkgs.lib;
{
  imports = [
    ../modules/settings.nix
  ];

  settings = osConfig.settings;

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs.nix;

  home.stateVersion = "25.05";

  home.packages = with pkgs; [
    alacritty
    anki
    aspell
    aspellDicts.en
#    carve
    clojure
    cljfmt
    clj-kondo
    dmenu
    evince
    feh
    ffmpeg
    firefox
    gcc
    gimp
    gitAndTools.pre-commit
    gnumake
    graphviz
    chromium
    home-manager
    jq
    killall
    kitty
    leiningen
    moreutils
    nixos-generators
    nodejs
    nodePackages.mermaid-cli
    pyright
    pandoc
    protobuf
    pv
    (python3.withPackages (ps: with ps; [virtualenv wheel setuptools numpy pandas]))
    ripgrep
    sbt
    scrot
    sqlite
    term-do
    tree
    unzip
    zip
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsNativeComp;
  };
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
      colors.focused = {
        border = "#d33682";
        background = "#4c7899";
        text = "#ffffff";
        indicator = "#d33682";
        childBorder = "#d33682";
      };
      bars = [
        {
          id = "bar-0";
          position = "top";
          hiddenState = "hide";
          statusCommand = "${pkgs.i3status}/bin/i3status";
          fonts = {
            names = [ config.settings.fontName ];
            size = config.settings.fontSize;
          };
          colors = {
            separator = "#586e75";
            background = "#fdf6e3";
            statusline = "#657b83";
            focusedWorkspace = {
              background = "#586e75";
              border = "#93a1a1";
              text = "#002b36";
            };
            activeWorkspace = {
              background = "#fdf6e3";
              border = "#6c71c4";
              text = "#fdf6e3";
            };
            inactiveWorkspace = {
              background = "#eee8d5";
              border = "#b58900";
              text = "#657b83";
            };
            urgentWorkspace  = {
              background = "#d33682";
              border = "#d33682";
              text = "#fdf6e3";
            };
          };
          extraConfig = ''
            modifier none
          '';
        }
      ];
      keybindings = (
        {
          "${mod}+p" = "exec ${pkgs.dmenu}/bin/dmenu_run -fn '${config.settings.fontName}-${toString config.settings.fontSize}' -nb \\#fdf6e3 -nf \\#657b83 -sb \\#eee8d5 -sf \\#cb4b16";
          "${mod}+o" = "exec ${pkgs.clipmenu}/bin/clipmenu -fn '${config.settings.fontName}-${toString config.settings.fontSize}' -nb \\#fdf6e3 -nf \\#657b83 -sb \\#eee8d5 -sf \\#cb4b16";
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
          "${mod}+Shift+>" = "move workspace to output right";
          "${mod}+Shift+<" = "move workspace to output left";
          "${mod}+Shift+Control+f" = "sticky toggle";
          # Defaults
          "${mod}+f" = "focus mode_toggle";
          "${mod}+Shift+F" = "floating toggle";
          # "${mod}+f" = "fullscreen toggle";
          "${mod}+a" = "focus parent";
          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 10";
          "${mod}+Shift+1" = "move container to workspace number 1";
          "${mod}+Shift+2" = "move container to workspace number 2";
          "${mod}+Shift+3" = "move container to workspace number 3";
          "${mod}+Shift+4" = "move container to workspace number 4";
          "${mod}+Shift+5" = "move container to workspace number 5";
          "${mod}+Shift+6" = "move container to workspace number 6";
          "${mod}+Shift+7" = "move container to workspace number 7";
          "${mod}+Shift+8" = "move container to workspace number 8";
          "${mod}+Shift+9" = "move container to workspace number 9";
          "${mod}+Shift+0" = "move container to workspace number 10";
          "${mod}+Tab" = "workspace back_and_forth";
          "${mod}+r" = "mode resize";
        }
        // optionalAttrs (!config.settings.vm)
          {
            "${mod}+equal" = "workspace next";
            "${mod}+minus" = "workspace prev";
            "${mod}+grave" = "workspace 1";
            "${mod}+Shift+Control+L" = "exec i3lock -c 000000";
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
      window = {
        titlebar = false;
        border = 2;
      };
    };
    # inexplicably xserver wrapper doesn't set the background image
    extraConfig = ''
      focus_wrapping no
      exec_always "if [ -e $HOME/.background-image ]; then feh --bg-scale $HOME/.background-image ; fi"
      exec i3-msg workspace 1
      for_window [class="(.*join\?action\=join.*|.*zoom.*)"] floating enable
      for_window [class="(.*join\?action\=join.*|.*zoom.*)" title="Zoom - Licensed Account"] floating disable
      for_window [class="(.*join\?action\=join.*|.*zoom.*)" title="Zoom Workplace - Licensed account"] floating disable
      for_window [class="(.*join\?action\=join.*|.*zoom.*)" title="Meeting"] floating disable
    '';
  };

  programs.i3status = {
    enable = true;
    enableDefault = false;
    general = {
      colors = true;
      color_good = "#859900";
      color_degraded = "#B58900";
      color_bad = "#DC322F";
      interval = 5;
    };
    modules = {
      "disk /" = {
        position = 20;
        settings = {
          format = "/ %avail";
        };
      };
      "load" = {
        position = 30;
        settings = {
          format = "Load: %1min";
        };
      };
      "tztime local" = mkDefault {
        position = 40;
        settings = {
          format = "🗽 %Y-%m-%d %l:%M %P";
        };
      };
    };
  };


  xresources.properties = {
    "xterm*faceName" = "${config.settings.fontName}";
    "xterm*faceSize" = "${head (splitString "." (toString config.settings.fontSize))}";
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
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
    inactiveInterval = 5; # minutes
  };

  services.clipmenu.enable = true;

  programs.git = {
    enable = true;
    userName = config.settings.name;
    userEmail = config.settings.email;
    aliases = {
      s = "status -s -uno";
      gl = "log --oneline --graph";
    };
    ignores = [".#*" "*.desktop" "*.lock"];
    lfs.enable = true;
    extraConfig = {
      branch.autosetuprebase = "never";
      push.default = "simple";
      # TODO: bring this file under nix control
      core.pager = "less -F -X";
      pull.ff = "only";
      init.defaultBranch = "main";
    };
  };

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

  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        padding = {
          x = 2;
          y = 2;
        };
        decorations = "full";
        dynamic_title = true;
        opacity = 1.0;
      };
      scrolling = {
        history = 10000;
        multiplier = 3;
      };
      font = {
        normal = {
          family = config.settings.fontName;
          style = "Regular";
        };
        bold = {
          family = config.settings.fontName;
          style = "Bold";
        };
        italic = {
          family = config.settings.fontName;
          style = "Italic";
        };
        size = config.settings.fontSize;
        offset = {
          x = 0;
          y = 0;
        };
        glyph_offset = {
          x = 0;
          y = 0;
        };
      };
      colors = {
        primary = {
          background = "0xfdf6e3";
          foreground = "0x586e75";
        };
        normal = {
          black = "0xeee8d5";
          red = "0xdc322f";
          green = "0x859900";
          yellow = "0xb58900";
          blue = "0x268bd2";
          magenta = "0xd33682";
          cyan = "0x2aa198";
          white = "0x073642";
        };
        bright = {
          black = "0xfdf6e3";
          red = "0xcb4b16";
          green = "0x93a1a1";
          yellow = "0x839496";
          blue = "0x657b83";
          magenta = "0x6c71c4";
          cyan = "0x586e75";
          white = "0x002b36";
        };
        draw_bold_text_with_bright_colors = false;
      };
      mouse = {
        hide_when_typing = false;
        bindings = [
          {mouse = "Middle"; action = "PasteSelection";}
        ];
      };
      selection = {
        semantic_escape_chars = ",│`|:\"' ()[]{}<>";
        save_to_clipboard = false;
      };
      cursor = {
        style = "Block";
        unfocused_hollow = true;
        thickness = 1.0;
      };
      general.live_config_reload = true;
    };
  };

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
    initContent = let
      cdpath = "$HOME/src" +
        optionalString (config.settings.profile != "malloc47")
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
        optionalString (config.settings.profile != "malloc47")
        ":~/src/${config.settings.profile}";
    };
    shellOptions = [
    "autocd" "cdspell" "globstar" # bash >= 4
    "cmdhist" "nocaseglob" "histappend" "extglob"];
  };

  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.file.".inputrc".source = ./.inputrc;
  home.file.".lein" = { source = ./.lein; recursive = true; };
  home.file.".clojure" = { source = ./.clojure; recursive = true; };
  home.file.".sbt" = { source = ./.sbt; recursive = true; };
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
      sudo bash -c "echo $1 > /sys/class/backlight/mba6x_backlight/brightness"
    '';
  };

  home.file."setup-shared" = mkIf (config.settings.vm) {
    target = "bin/setup-shared";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      mkdir $HOME/shared 2>/dev/null
      vmhgfs-fuse .host:/shared $HOME/shared -o subtype=vmhgfs-fuse
    '';
  };

  programs.java.enable = true;
}
