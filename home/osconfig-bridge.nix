# Bridge module: copies settings from the NixOS/darwin system config
# into the home-manager evaluation. Only import this when home-manager
# runs as a NixOS/darwin submodule (not in standalone mode).
{ osConfig, ... }:
{
  settings = osConfig.settings;
}
