{ config, pkgs, pkgs-unstable, ... }:
let
  # Using unstable due to https://github.com/NixOS/nixpkgs/issues/395169
  emacs-custom = pkgs-unstable.emacs30.override {
    withNativeCompilation = true;
  };
in
with pkgs.lib;
{
  imports = [ ../modules/settings.nix ];

  home.packages = with pkgs; [ ispell ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsNativeComp; # emacs-custom;
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
      (setq nano-font-size ${head (splitString "." (toString config.settings.fontSize))})
      (setq nano-font-family-monospaced "${config.settings.fontName}")
    '';
  };
}
