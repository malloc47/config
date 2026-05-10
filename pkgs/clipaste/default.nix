{
  lib,
  apple-sdk_15 ? null,
  fetchFromGitHub,
  rustPlatform,
  stdenv,
}:

rustPlatform.buildRustPackage rec {
  pname = "clipaste";
  version = "2.2.0";

  src = fetchFromGitHub {
    owner = "hqhq1025";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-Jgc2SwZ/ZZxgRJmraGx3zCTF9dkIPhhUX+O0fUFV9i8=";
  };

  cargoHash = "sha256-mUvW3FXNZj3OeXuaZgMZpRnmQ52E7TcLiypemt1T3p4=";

  buildInputs = lib.optionals stdenv.isDarwin [ apple-sdk_15 ];

  meta = {
    description = "Fix macOS and Windows screenshot paste in terminal AI tools";
    homepage = "https://github.com/hqhq1025/clipaste";
    license = lib.licenses.mit;
    mainProgram = "clipaste";
    platforms = lib.platforms.darwin;
  };
}
