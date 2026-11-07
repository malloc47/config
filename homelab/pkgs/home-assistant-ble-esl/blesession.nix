# One instrumented BLE GATT session (connect, notify, stage timings, failure
# reasons) on top of bleak; a requirement of hass-ble-esl. Its Home Assistant
# adapter (blesession.hass) imports homeassistant lazily, so HA is not a
# build dependency.
{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  bleak,
  bleak-retry-connector,
  pytestCheckHook,
  pytest-asyncio,
}:

buildPythonPackage rec {
  pname = "blesession";
  version = "0.9.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "eigger";
    repo = "blesession";
    rev = "v${version}";
    hash = "sha256-3wBfjU25XcfuDoR3ZyYa2wdV6GgpwMEzJlH0So6bsSM=";
  };

  build-system = [ setuptools ];

  dependencies = [
    bleak
    bleak-retry-connector
  ];

  nativeCheckInputs = [
    pytestCheckHook
    pytest-asyncio
  ];

  pythonImportsCheck = [ "blesession" ];

  meta = {
    description = "Instrumented BLE session for Home Assistant BLE integrations";
    homepage = "https://github.com/eigger/blesession";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
