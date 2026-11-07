# Declarative YAML/dict -> image renderer (text, icons, charts, QR/barcodes,
# palette dithering); the engine behind hass-ble-esl's payloads and designer.
# Requested as imagespec[datamatrix], so pystrich comes along.
{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  pillow,
  qrcode,
  python-barcode,
  requests,
  pystrich,
  pyyaml,
  numpy,
  jsonschema,
  pytestCheckHook,
}:

buildPythonPackage rec {
  pname = "imagespec";
  version = "1.1.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "eigger";
    repo = "imagespec";
    rev = "v${version}";
    hash = "sha256-bKQaCcvO4xox5Li670lE79xswq6c9tjiYxvCxl8J1DA=";
  };

  build-system = [ setuptools ];

  dependencies = [
    pillow
    qrcode
    python-barcode
    requests
  ];

  optional-dependencies = {
    datamatrix = [ pystrich ];
    yaml = [ pyyaml ];
    fast = [ numpy ];
  };

  nativeCheckInputs = [
    jsonschema
    pytestCheckHook
  ]
  ++ lib.flatten (lib.attrValues optional-dependencies);

  pythonImportsCheck = [ "imagespec" ];

  meta = {
    description = "Render images from a declarative YAML/dict spec";
    homepage = "https://github.com/eigger/imagespec";
    changelog = "https://github.com/eigger/imagespec/blob/v${version}/CHANGELOG.md";
    license = with lib.licenses; [
      mit
      asl20
    ];
    maintainers = [ ];
  };
}
