# Emacs client for tramp-rpc, built from the same release as tramp-rpc-server
# so the two always agree on the protocol revision. Call it from an Emacs
# package scope (e.g. programs.emacs.extraPackages) via epkgs.callPackage.
{
  lib,
  elpaBuild,
  melpaBuild,
  fetchurl,
  fetchFromGitHub,
  runCommand,
  tramp-rpc-server,
}:

let
  # nixpkgs 26.05 ships tramp 2.8.1.3; tramp-rpc requires >= 2.8.1.4.
  tramp = elpaBuild rec {
    pname = "tramp";
    version = "2.8.2.4";
    # The tarball's test/resources/foo.iso/ gets rewritten by Emacs's built-in
    # tramp-archive file handler during package.el's unpack check, which then
    # rejects the package. Drop the tests; nothing here runs them.
    src =
      runCommand "tramp-${version}.tar"
        {
          tarball = fetchurl {
            url = "https://elpa.gnu.org/packages/tramp-${version}.tar";
            hash = "sha256-et/Ysa/WkNM0++75CVQjrfrGenjh/xly52cXVeK3u9o=";
          };
        }
        ''
          tar --delete --wildcards -f - 'tramp-${version}/test*' < "$tarball" > "$out"
        '';
  };

  # nixpkgs' msgpack is a 2020 snapshot that predates msgpack-read's keyword
  # API. Same pin as upstream's flake.
  msgpack = melpaBuild {
    pname = "msgpack";
    version = "0.1.1";
    src = fetchFromGitHub {
      owner = "xuchunyang";
      repo = "msgpack.el";
      rev = "5353a7b2da854c843cbec4536996242001f63471";
      hash = "sha256-XMNaHjNx3E/HbqNyqSfotIq5YpUMydUX6d9et2VE+SI=";
    };
    files = ''("msgpack.el")'';
  };
in
melpaBuild {
  pname = "tramp-rpc";
  inherit (tramp-rpc-server) version src;
  files = ''("lisp/*.el")'';
  packageRequires = [
    tramp
    msgpack
  ];

  meta = {
    description = "TRAMP backend that talks MessagePack-RPC to tramp-rpc-server";
    inherit (tramp-rpc-server.meta) homepage license;
  };
}
