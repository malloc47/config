{ lib, fetchzip }:

fetchzip {
  name = "geosanslight";

  url = "https://dl.dafont.com/dl/?f=geo_sans_light";
  extension = "zip";
  stripRoot = false;

  postFetch = ''
    mkdir -p $out/share/fonts/truetype
    mv $out/*.ttf $out/share/fonts/truetype
  '';

  sha256 = "1amv94zalz6n0x2jcmydvixsy3a49hjz948sxpcs7knv62r7ajk7";

  meta = {
    description = "Geo Sans Light font";
    homepage = "https://www.dafont.com/geo-sans-light.font";
    platforms = lib.platforms.unix;
  };
}
