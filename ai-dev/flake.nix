{
  description = "AI coding tools with per-project sandboxing helpers and session orchestration.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      llm-agents,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      mkForSystem =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true; # claude-code is unfree
          };
          agents = llm-agents.packages.${system};
          defaults = import ./defaults.nix { inherit pkgs; };

          # Zellij wrapper with config.kdl baked in via store path.
          zellij-ai = pkgs.writeShellApplication {
            name = "zellij-ai";
            runtimeInputs = [ pkgs.zellij ];
            text = ''
              exec zellij --config ${./zellij-config.kdl} "$@"
            '';
          };

          # Per-project sandboxing: harness+profile system (backed by zerobox)
          sandbox = import ./zerobox-sandbox.nix { inherit pkgs; };
          harnessDefinitions = import ./harnesses.nix { inherit pkgs agents defaults; };
          profileDefinitions = import ./profiles.nix { inherit pkgs; };
          projectLib = import ./lib.nix {
            inherit
              pkgs
              agents
              defaults
              harnessDefinitions
              profileDefinitions
              ;
            mkSandboxUpstream = sandbox.mkSandbox;
          };
        in
        {
          packages = {
            inherit zellij-ai;
            claude-code = agents.claude-code;
            opencode = agents.opencode;
            agent-deck = agents.agent-deck;
            default = pkgs.buildEnv {
              name = "ai-dev-env";
              paths = [
                agents.claude-code
                agents.opencode
                agents.agent-deck
                zellij-ai
                pkgs.zellij
              ];
            };
          };

          devShell = pkgs.mkShell {
            packages = [
              agents.claude-code
              agents.opencode
              agents.agent-deck
              zellij-ai
              pkgs.zellij
            ];
            shellHook = ''
              echo "ai-dev environment ready. Try: claude, opencode, agent-deck, zellij-ai"
            '';
          };

          apps = {
            claude = {
              type = "app";
              program = "${agents.claude-code}/bin/claude";
            };
            opencode = {
              type = "app";
              program = "${agents.opencode}/bin/opencode";
            };
            agent-deck = {
              type = "app";
              program = "${agents.agent-deck}/bin/agent-deck";
            };
            zellij-ai = {
              type = "app";
              program = "${zellij-ai}/bin/zellij-ai";
            };
            default = {
              type = "app";
              program = "${zellij-ai}/bin/zellij-ai";
            };
          };

          lib = projectLib;
        };
    in
    {
      packages = forAllSystems (system: (mkForSystem system).packages);
      devShells = forAllSystems (system: {
        default = (mkForSystem system).devShell;
      });
      apps = forAllSystems (system: (mkForSystem system).apps);
      lib = forAllSystems (system: (mkForSystem system).lib) // {
        inherit forAllSystems;
      };

      templates = {
        default = {
          path = ./templates/default;
          description = "Per-project AI sandbox with claude-code and opencode";
        };
        minimal = {
          path = ./templates/minimal;
          description = "Minimal AI sandbox with just claude-code";
        };
      };

      homeManagerModules = {
        # Installs both layers — convenience for hosts that want everything
        default = import ./home-manager.nix { inherit self; };
        # Sandbox layer: raw claude-code + opencode
        sandbox = import ./sandbox.nix { inherit self; };
        # Session layer: agent-deck + zellij-ai
        session = import ./session.nix { inherit self; };
      };
    };
}
