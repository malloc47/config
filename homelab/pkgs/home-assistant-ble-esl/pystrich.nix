# Pure-Python 1D/2D barcode generator; imagespec's optional datamatrix
# element imports it lazily. Taken from PyPI: the GitHub repo's tags stop at
# 0.10.
{
  lib,
  buildPythonPackage,
  fetchPypi,
  hatchling,
  pillow,
  typing-extensions,
}:

buildPythonPackage rec {
  pname = "pystrich";
  version = "0.21";
  pyproject = true;

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-U9w/FL9XHXwcCm01iwsQCuSK5gKk6FnjBwGZpjVjJ1s=";
  };

  build-system = [ hatchling ];

  dependencies = [
    pillow
    typing-extensions
  ];

  pythonImportsCheck = [ "pystrich.datamatrix" ];

  meta = {
    description = "Pure-Python 1D and 2D barcode generator";
    homepage = "https://github.com/mmulqueen/pyStrich";
    license = lib.licenses.asl20;
    maintainers = [ ];
  };
}
