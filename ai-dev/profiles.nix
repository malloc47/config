# Composable tool profiles for sandbox configuration.
# Each profile bundles packages, domains, and path permissions for a tool ecosystem.
{ pkgs }:
{
  github = {
    packages = [ pkgs.gh ];
    domains = [
      "github.com"
      "api.github.com"
      "objects.githubusercontent.com"
    ];
    allowWrite = [ "$HOME/.config/gh" ];
  };

  python = {
    packages = [
      pkgs.python3
      pkgs.python3Packages.pip
    ];
    domains = [
      "pypi.org"
      "files.pythonhosted.org"
    ];
    allowWrite = [ "$HOME/.cache/pip" ];
  };

  node = {
    packages = [ pkgs.nodejs ];
    domains = [
      "registry.npmjs.org"
      "registry.yarnpkg.com"
    ];
    allowWrite = [
      "$HOME/.npm"
      "node_modules"
    ];
  };

  rust = {
    packages = [
      pkgs.rustc
      pkgs.cargo
    ];
    domains = [
      "crates.io"
      "static.crates.io"
      "index.crates.io"
    ];
    allowWrite = [
      "$HOME/.cargo"
      "target"
    ];
  };

  aws = {
    packages = [ pkgs.awscli2 ];
    domains = [
      "sts.amazonaws.com"
    ];
    allowWrite = [ "$HOME/.aws" ];
  };

  docker = {
    packages = [ pkgs.docker-client ];
    domains = [ ];
    allowWrite = [ "$HOME/.docker" ];
  };

  nix = {
    packages = [
      pkgs.nix
      pkgs.nixfmt-rfc-style
    ];
    domains = [
      "cache.nixos.org"
    ];
    allowWrite = [ ];
  };
}
