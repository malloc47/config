{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "adguardian";
  version = "1.6.0-unstable-2026-05-10";

  src = fetchFromGitHub {
    owner = "Lissy93";
    repo = "AdGuardian-Term";
    rev = "63656d8092a4217fe99144c014c6685f17d71276";
    hash = "sha256-eOJ93qAAlBNZW27ZAEP6AkLa7SkDQVxuajkgjBfYZYM=";
  };

  cargoHash = "sha256-yPDysaslL/7N60eZ/hqZl5ZXIsof/pvlgHYfW1mIWtI=";

  meta = {
    description = "Terminal-based, real-time traffic monitoring and statistics for your AdGuard Home instance";
    mainProgram = "adguardian";
    homepage = "https://github.com/Lissy93/AdGuardian-Term";
    license = lib.licenses.mit;
  };
}
