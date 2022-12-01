{ lib
, stdenv
, fetchFromGitHub
, writeText
, gcc
, graalvm17-ce
, leiningen}:

let
  pname = "cljfmt";
  version = "0.8.0";

  # Workaround for https://github.com/weavejester/cljfmt/issues/236
  patch = writeText "project.patch" ''
diff --git a/cljfmt/project.clj b/cljfmt/project.clj
index b881c1e..c1c6d6d 100644
--- a/cljfmt/project.clj
+++ b/cljfmt/project.clj
@@ -11,7 +11,8 @@
                  [rewrite-clj "1.0.605-alpha"]]
   :plugins [[lein-cljsbuild "1.1.7"]
             [io.taylorwood/lein-native-image "0.3.1"]]
-  :hooks [leiningen.cljsbuild]
+  :local-repo "./m2"
+  :offline true
   :cljsbuild {:builds
               {"dev" {:source-paths ["src" "test"]
                       :compiler {:main cljfmt.test-runner
  '';

  src = fetchFromGitHub {
    owner = "weavejester";
    repo = pname;
    rev = version;
    sha256 = "1fyjfjzfg6yv2ijghqddasn987m2bi9zd1dfxdxwiixxaczidv1x";
  } + "/cljfmt";

  deps = stdenv.mkDerivation {
    name = "${pname}-deps";
    inherit src;

    nativeBuildInputs = [ leiningen ];

    buildPhase = ''
      patch < ${patch}
      LEIN_NO_USER_PROFILES=true LEIN_HOME=. lein deps
    '';

    installPhase = ''
      mkdir -p $out/deps/
      cp -r m2/* $out/deps
      # There are timestamps in the comment headers of these files that
      # break consistent hashing
      find $out/deps/ -name \*.repositories | xargs -I {} sed -i '/^#/d' {}
    '';

    dontFixup = true;
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-JRJ8vgvKwfCgkgELYYHETP/4ge0dyww6Okod3kcFKxQ=";
  };

in

stdenv.mkDerivation rec {

  inherit pname version src;

  nativeBuildInputs = [
    gcc
    graalvm17-ce
    leiningen
  ];

  buildPhase = ''
    patch < ${patch}
    mkdir m2
    cp -r ${deps}/deps/* m2/
    LEIN_NO_USER_PROFILES=true LEIN_HOME=. lein native-image
  '';

  installPhase = ''
    mkdir -p $out/bin/
    cp target/cljfmt $out/bin/
  '';

  meta = {
    description = "Native image build of cljfmt";
    homepage = "https://github.com/weavejester/cljfmt";
    license = lib.licenses.epl10;
    platforms = lib.platforms.linux;
  };
}
