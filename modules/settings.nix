{config, pkgs, lib, ...}:

with lib;

{
  options = {
    settings = {
      name = mkOption {
        default = "Jarrell Waggoner";
        type = with types; uniq string;
      };
      username = mkOption {
        default = "malloc47";
        type = with types; uniq string;
      };
      email = mkOption {
        default = "malloc47@gmail.com";
        type = with types; uniq string;
      };
      vm = mkOption {
        type = types.bool;
        default = false;
      };
      terminal = mkOption {
        default = "alacritty";
        type = with types; uniq string;
      };
      fontSize = mkOption {
        default = 12;
        type = types.int;
      };
      profile = mkOption {
        default = "malloc47";
        type = with types; uniq string;
        description = ''
          Profiles are a higher-level grouping than hosts. They are
          useful to combine multiple related things (e.g. ssh keys)
          that should be available on multiple hosts.
        '';
      };
      xkbFile = mkOption {
        default = "none";
        type = with types; uniq string;
        description = ''
          Filename of the xkb file to load (or "none" if no keyboard
          layout is desired). File is specified without extension and
          must be present in the xkb directory.
        '';
      };
    };
  };
}
