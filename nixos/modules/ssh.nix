{
  config,
  options,
  lib,
  ...
}:
{
  imports = [ ../../modules/settings.nix ];
  config = lib.mkMerge (
    [
      {
        home-manager.users.${config.settings.username} = {
          imports = [ ../../home/modules/ssh-personal.nix ];
          programs.ssh-personal.enable = true;
        };
      }
    ]
    # sshd config is NixOS-only; nix-darwin's services.openssh doesn't
    # expose the `settings` attribute.
    ++ lib.optionals (options.services.openssh ? settings) [
      {
        services.openssh = {
          enable = lib.mkIf config.settings.vm true;
          settings = {
            X11Forwarding = lib.mkIf config.settings.vm true;
            # Keepalive: 30s * 6 = 180s blip tolerance. SSH-layer keepalive
            # is preferred over kernel TCPKeepAlive (which only fires
            # after ~2h, past most NAT timeouts).
            ClientAliveInterval = lib.mkDefault 30;
            ClientAliveCountMax = lib.mkDefault 6;
            TCPKeepAlive = lib.mkDefault false;
          };
        };
      }
    ]
  );
}
