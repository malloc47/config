{ pkgs, config, ... }:

{
  imports = [
    ../modules/settings.nix
  ];

  settings = {
    fontSize = 14.0;
  };

  age.secrets.nix-access-tokens.file = ../secrets/nix-access-tokens.age;

  nix.extraOptions = ''
    !include ${config.age.secrets.nix-access-tokens.path}
  '';

  nix.buildMachines = [
    {
      hostName = "192.168.1.10";
      systems = [ "x86_64-linux" ];
      sshUser = "malloc47";
      sshKey = "/Users/malloc47/.ssh/id_ed25519";
      maxJobs = 4;
      supportedFeatures = [
        "nixos-test"
        "big-parallel"
      ];
    }
  ];
  nix.distributedBuilds = true;
}
