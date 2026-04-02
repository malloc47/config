{ pkgs, config, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  environment.variables = {
    NIX_SSL_CERT_FILE = "/etc/ssl/system-certs.pem";
    SSL_CERT_FILE = "/etc/ssl/system-certs.pem";
  };

  settings = {
    username = "jwaggoner";
    email = "jwaggoner@drwholdings.com";
    profile = "drw";
    fontName = "Roboto Mono";
    fontSize = 16.0;
  };
}
