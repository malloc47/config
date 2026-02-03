{ config, pkgs, pkgs-unstable, ... }:
{
  imports = [ ../modules/settings.nix ];

  home.packages = with pkgs; [ ispell ];

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
      (setq nano-font-size ${head (splitString "." (toString config.settings.fontSize))})
      (setq nano-font-family-monospaced "${config.settings.fontName}")
    '';
  };
}
