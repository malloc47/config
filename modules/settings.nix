{config, pkgs, lib, ...}:

with lib;

{
  options = {
    settings = {
      name = mkOption {
        default = "Jarrell Waggoner";
        type = with types; uniq str;
      };
      username = mkOption {
        default = "malloc47";
        type = with types; uniq str;
      };
      email = mkOption {
        default = "malloc47@gmail.com";
        type = with types; uniq str;
      };
      vm = mkOption {
        type = types.bool;
        default = false;
      };
      terminal = mkOption {
        default = "alacritty";
        type = with types; uniq str;
      };
      fontName = mkOption {
        default = "Inconsolata";
        type = with types; uniq str;
      };
      fontSize = mkOption {
        default = 12.0;
        type = types.float;
      };
      dpi = mkOption {
        default = 277;
        type = types.ints.positive;
      };
      profile = mkOption {
        default = "malloc47";
        type = with types; uniq str;
        description = ''
          Profiles are a higher-level grouping than hosts. They are
          useful to combine multiple related things (e.g. ssh keys)
          that should be available on multiple hosts.
        '';
      };
      xkbFile = mkOption {
        default = "none";
        type = with types; uniq str;
        description = ''
          Filename of the xkb file to load (or "none" if no keyboard
          layout is desired). File is specified without extension and
          must be present in the xkb directory.
        '';
      };
      extraGroups = mkOption {
        default = [];
        type = with types; listOf str;
        description = ''
          Groups to attach to the default user.
        '';
      };

    };
  };

  config = {
    settings.xkbFile = lib.mkIf (config.settings.vm) (lib.mkDefault "vm");
  };
}
