# Copied from https://github.com/NixOS/nixpkgs/blob/nixos-25.05/pkgs/by-name/al/albert/package.nix
# TODO: This doesn't work yet, lots of linking problems
{
  lib,
  stdenv,
  fetchFromGitHub,
  fixDarwinDylibNames,
  qt6,
  qt6Packages,
  cmake,
  libqalculate,
  muparser,
  libarchive,
  python3Packages,
  nix-update-script,
  pkg-config,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "albert";
  version = "0.32.1";

  src = fetchFromGitHub {
    owner = "albertlauncher";
    repo = "albert";
    tag = "v${finalAttrs.version}";
    hash = "sha256-v2SMY0KGFwwybsiMu1W1wBWdyoDEFF3hWd4LeaT8Nts=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    qt6.wrapQtAppsHook
    fixDarwinDylibNames
  ];

  cmakeFlags = [
    "-DMAKE_OSX_DEPLOYMENT_TARGET=12"
    "-DCMAKE_BUILD_TYPE=RelWithDebInfo"
    "-DCMAKE_OSX_ARCHITECTURES=arm64"
    "-DBUILD_PLUGIN_DEBUG=OFF"
    "-DBUILD_PLUGIN_DOCS=OFF"
    "-DCMAKE_INSTALL_LIBDIR=lib"
  ];


  buildInputs = [
    libqalculate
    libarchive
    muparser
    qt6.qtbase
    qt6.qtscxml
    qt6.qtsvg
    qt6.qtdeclarative
    qt6.qt5compat
    qt6.qttools
    qt6Packages.qtkeychain
  ]
  ++ (with python3Packages; [
    python
    pybind11
  ]);

  patches = [
    # ./no-link.patch
    ./bundle-fixes.patch
  ];

  postPatch = ''
    find -type f -name CMakeLists.txt -exec sed -i {} -e '/INSTALL_RPATH/d' \;

    substituteInPlace src/app/qtpluginprovider.cpp \
      --replace-fail "QStringList install_paths;" "QStringList install_paths;${"\n"}install_paths << QFileInfo(\"$out/lib\").canonicalFilePath();"
  '';

  passthru = {
    updateScript = nix-update-script { };
  };

  # preConfigure = ''
  #   substituteInPlace cmake/bundle-macos.cmake.in \
  #     --replace-fail "set(BUNDLE_PATH \"\''${CMAKE_INSTALL_PREFIX}/\''${PROJECT_NAME}.app\"" "set(BUNDLE_PATH \"$out/Applications/\''${PROJECT_NAME}.app\""
  # '';

  cmakeBuildDir = "appbuild";

  # installPhase = ''
  #   runHook preInstall

  #   mkdir "$out/bin"
  #   ln -s "$out/Applications/Albert.app/Contents/MacOS/Albert" "$out/bin/albert"

  #   runHook postInstall
  # '';

  postInstall = ''
    mkdir "$out/Applications" "$out/bin"
    mv "$out/Albert.app" "$out/Applications/"
    ln -s "$out/Applications/Albert.app/Contents/MacOS/Albert" "$out/bin/albert"
    install_name_tool \
       -add_rpath @executable_path/../Frameworks \
       "$out/Applications/Albert.app/Contents/MacOS/Albert"
  '';

  meta = {
    description = "Fast and flexible keyboard launcher";
    longDescription = ''
      Albert is a desktop agnostic launcher. Its goals are usability and beauty,
      performance and extensibility. It is written in C++ and based on the Qt
      framework.
    '';
    homepage = "https://albertlauncher.github.io";
    changelog = "https://github.com/albertlauncher/albert/blob/${finalAttrs.src.rev}/CHANGELOG.md";
    # See: https://github.com/NixOS/nixpkgs/issues/279226
    license = lib.licenses.unfree;
    maintainers = with lib.maintainers; [
      synthetica
      eljamm
    ];
    mainProgram = "albert";
    platforms = lib.platforms.all;
  };
})
