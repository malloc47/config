{ lib
, stdenv
, fetchzip
, autoPatchelfHook
, zlib
, gcc-unwrapped}:

stdenv.mkDerivation rec {
  pname = "carve";
  version = "0.1.0";

  src = fetchzip {
    url = "https://github.com/borkdude/${pname}/releases/download/v${version}/${pname}-${version}-linux-amd64.zip";
    sha256 = "0gwlg6ph0lig15a215i7iywq3q8yq8d7wfv3b3dxxhaa14f8v3j0";
  };

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = [
    gcc-unwrapped
    zlib
  ];

  installPhase = ''
    mkdir -p $out/bin/
    cp carve $out/bin/
  '';

  meta = with lib; {
    description = "Carve out the essentials of your Clojure app";
    homepage = "https://github.com/borkdude/carve";
    license = licenses.epl10;
    platforms = platforms.linux;
  };
}
