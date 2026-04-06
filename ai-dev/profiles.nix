# Composable tool profiles for sandbox configuration.
# Each profile bundles packages, domains, and state paths for a tool ecosystem.
{ pkgs }:
{
  github = {
    packages = [ pkgs.gh ];
    domains = [
      "github.com"
      "api.github.com"
      "objects.githubusercontent.com"
    ];
    stateDirs = [ "$HOME/.config/gh" ];
    stateFiles = [ ];
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
    stateDirs = [ "$HOME/.cache/pip" ];
    stateFiles = [ ];
  };

  node = {
    packages = [ pkgs.nodejs ];
    domains = [
      "registry.npmjs.org"
      "registry.yarnpkg.com"
    ];
    stateDirs = [
      "$HOME/.npm"
      "node_modules"
    ];
    stateFiles = [ ];
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
    stateDirs = [
      "$HOME/.cargo"
      "target"
    ];
    stateFiles = [ ];
  };

  aws = {
    packages = [ pkgs.awscli2 ];
    domains = [
      "sts.amazonaws.com"
    ];
    stateDirs = [ "$HOME/.aws" ];
    stateFiles = [ ];
  };

  docker = {
    packages = [ pkgs.docker-client ];
    domains = [ ];
    stateDirs = [ "$HOME/.docker" ];
    stateFiles = [ ];
  };

  nix = {
    packages = [
      pkgs.nix
      pkgs.nixfmt-rfc-style
    ];
    domains = [
      "cache.nixos.org"
    ];
    stateDirs = [ ];
    stateFiles = [ ];
  };
}
