{
  config,
  pkgs,
  lib,
  ...
}:

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
      displayName = mkOption {
        default = null;
        type = with types; nullOr str;
        description = ''
          Display name for this host, used in the shell prompt instead of
          the system hostname. When null, the prompt falls back to the
          default hostname (%m in zsh, \h in bash).
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
        default = [ ];
        type = with types; listOf str;
        description = ''
          Groups to attach to the default user.
        '';
      };
      sshKeys = mkOption {
        type = types.path;
        description = ''
          Path to directory containing SSH key profiles. Each profile
          is a subdirectory (e.g., malloc47/) containing key files.
        '';
      };
      sshKeyName = mkOption {
        type = with types; uniq str;
        description = ''
          Base name of the SSH key file (without extension) in
          <sshKeys>/<profile>/. Auto-detected from id_ed25519
          with fallback to id_rsa.
        '';
      };
      repositories = mkOption {
        default = [
          {
            url = "git@github.com:malloc47/cv.git";
            target = "~/src/cv";
          }
          {
            url = "git@github.com:malloc47/malloc47.github.com.git";
            target = "~/src/www";
          }
        ];
        type =
          with types;
          listOf (submodule {
            options = {
              url = mkOption {
                type = types.strMatching "^git@.*";
              };
              target = mkOption {
                type = str;
              };
            };
          });
        description = ''
          List of repositories to clone on home-manager activation.

          Each item must be an attribute with a url and target:
          {url = "git@github.com:..."; target = "~/src/repo"}
        '';
      };
    };
  };

  config = {
    settings.xkbFile = lib.mkIf (config.settings.vm) (lib.mkDefault "vm");
    settings.sshKeyName =
      let
        profileDir = config.settings.sshKeys + "/${config.settings.profile}";
      in
      lib.mkDefault (if builtins.pathExists (profileDir + "/id_ed25519") then "id_ed25519" else "id_rsa");
  };
}
