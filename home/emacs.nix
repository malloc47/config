{
  config,
  pkgs,
  pkgs-unstable,
  ...
}:
{
  imports = [ ../modules/settings.nix ];

  home.packages = [ ];

  programs.emacs.enable = true;
  services.emacs.enable = true;

  home.file.".emacs.d" = {
    source = ./.emacs.d;
    recursive = true;
  };

  home.file."fonts.el" = with pkgs.lib; {
    target = ".emacs.d/config/fonts.el";
    text = ''
      (provide 'fonts)
      (set-frame-font "${config.settings.fontName}-${head (splitString "." (toString config.settings.fontSize))}")
      (setq default-frame-alist '((font . "${config.settings.fontName}-${head (splitString "." (toString config.settings.fontSize))}")))
    '';
  };

  # Generate both light and dark base16 solarized themes for Emacs.
  # Light colors come from Stylix; dark colors are the well-known
  # solarized-dark base16 palette (base08–base0F are identical,
  # base00–base07 are reversed).
  home.file."stylix-theme.el" = {
    target = ".emacs.d/config/stylix-theme.el";
    text =
      let
        light = config.lib.stylix.colors.withHashtag;
        dark = {
          base00 = "#002b36";
          base01 = "#073642";
          base02 = "#586e75";
          base03 = "#657b83";
          base04 = "#839496";
          base05 = "#93a1a1";
          base06 = "#eee8d5";
          base07 = "#fdf6e3";
          base08 = "#dc322f";
          base09 = "#cb4b16";
          base0A = "#b58900";
          base0B = "#859900";
          base0C = "#2aa198";
          base0D = "#268bd2";
          base0E = "#6c71c4";
          base0F = "#d33682";
        };
        mkThemeColors = name: c: ''
          (defvar base16-${name}-theme-colors
            '(:base00 "${c.base00}"
              :base01 "${c.base01}"
              :base02 "${c.base02}"
              :base03 "${c.base03}"
              :base04 "${c.base04}"
              :base05 "${c.base05}"
              :base06 "${c.base06}"
              :base07 "${c.base07}"
              :base08 "${c.base08}"
              :base09 "${c.base09}"
              :base0A "${c.base0A}"
              :base0B "${c.base0B}"
              :base0C "${c.base0C}"
              :base0D "${c.base0D}"
              :base0E "${c.base0E}"
              :base0F "${c.base0F}"))

          (deftheme base16-${name})
          (base16-theme-define 'base16-${name} base16-${name}-theme-colors)
          (provide-theme 'base16-${name})
        '';
      in
      ''
        (provide 'stylix-theme)
        (require 'base16-theme)

        ${mkThemeColors "solarized-light" light}
        ${mkThemeColors "solarized-dark" dark}
      '';
  };
}
