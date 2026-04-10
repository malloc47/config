{ config, pkgs, ... }:
{
  imports = [ ../modules/settings.nix ];

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/solarized-light.yaml";
    polarity = "light";
    autoEnable = true;

    # Headless hosts have no wallpaper; disable the mandatory image
    image = pkgs.runCommand "blank.png" { nativeBuildInputs = [ pkgs.imagemagick ]; } ''
      magick -size 1x1 xc:#fdf6e3 $out
    '';

    # Use the per-host font from settings so Stylix and terminal.nix agree
    fonts.monospace = {
      name = config.settings.fontName;
      package = pkgs.dejavu_fonts; # fallback package; actual font installed separately
    };
    fonts.sizes.terminal = builtins.floor config.settings.fontSize;

    targets = {
      # Emacs uses its own solarized-theme package; let it manage itself
      emacs.enable = false;
    };
  };
}
