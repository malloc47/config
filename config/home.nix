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
    firefox
    google-chrome
    home-manager
    jq
    python37
    feh
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
        "${mod}+Shift+J" = "move down";
        "${mod}+Shift+K" = "move up";
        "${mod}+Shift+L" = "move right";
        "${mod}+Shift+H" = "move left";
        "${mod}+c" = "layout tabbed";
        "${mod}+x" = "split h";
        "${mod}+z" = "split v";
        "${mod}+space" = "layout toggle splitv splith tabbed";
        "${mod}+y" = "bar mode toggle";
        "${mod}+Shift+N" = "exec \"xterm -e 'sudo nixos-rebuild switch; echo COMPLETE ; read -n 1 -s'\"";
        "${mod}+Shift+Control+L" = "exec i3lock";
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

  home.file.".inputrc".source = ./.inputrc;
  home.file.".lein" = { source = ./.lein; recursive = true; };
  home.file.".sbt" = { source = ./.sbt; recursive = true; };
  home.file.".alacritty.yml" = { source = ./.alacritty.yml; target = ".config/alacritty/alacritty.yml"; };
  home.file.".i3status.conf" = { source = ./.i3status.conf; target = ".config/i3status/config"; };
}
