{stdenv, fetchzip}:

fetchzip {
  name = "geosanslight";

  url =  https://dl.dafont.com/dl/?f=geo_sans_light;

  postFetch = ''
    unzip $downloadedFile
    mkdir -p $out/share/fonts/truetype
    cp *.ttf $out/share/fonts/truetype
  '';

  sha256 = "1amv94zalz6n0x2jcmydvixsy3a49hjz948sxpcs7knv62r7ajk7";

  meta = {
    description = "Geo Sans Light font";
    homepage = https://www.dafont.com/geo-sans-light.font;
    platforms = stdenv.lib.platforms.unix;
  };
}
