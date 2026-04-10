{ pkgs, ... }:
{
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/solarized-light.yaml";
    polarity = "light";

    # Headless hosts have no wallpaper; disable the mandatory image
    image = pkgs.runCommand "blank.png" { nativeBuildInputs = [ pkgs.imagemagick ]; } ''
      magick -size 1x1 xc:#fdf6e3 $out
    '';

    targets = {
      # Emacs uses its own solarized-theme package; let it manage itself
      emacs.enable = false;
    };
  };
}
