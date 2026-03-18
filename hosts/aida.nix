{ config, pkgs, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  services.openssh.enable = true;

  age.secrets.caddy-basicauth = {
    file = ../secrets/caddy-basicauth.age;
    owner = "caddy";
  };

  services.caddy = {
    enable = true;
    virtualHosts."http://localhost" = {
      extraConfig = ''
        basicauth {
          import ${config.age.secrets.caddy-basicauth.path}
        }
        respond "Hello, world!"
      '';
    };
  };
}
