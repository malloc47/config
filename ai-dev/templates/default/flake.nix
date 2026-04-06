{
  description = "Project dev environment with sandboxed AI tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ai-dev.url = "github:malloc47/config?dir=ai-dev";
  };

  outputs =
    { nixpkgs, ai-dev, ... }:
    ai-dev.lib.forAllSystems (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        ai = ai-dev.lib.${system};
      in
      {
        devShells.${system}.default = ai.mkProjectShell {
          harnesses = [
            "claude-code"
            "opencode"
          ];
          profiles = with ai.profiles; [
            github
            # Add more: python, node, rust, aws, docker, nix
          ];
          # extraDomains = [ ];
          # extraPackages = [ ];
          # extraStateDirs = [ ];
          # unrestrictedHarness = false;
        };
      }
    );
}
