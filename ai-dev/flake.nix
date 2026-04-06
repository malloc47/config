{
  description = "AI coding tools with per-project sandboxing helpers and session orchestration.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agent-sandbox = {
      url = "github:archie-judd/agent-sandbox.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      llm-agents,
      agent-sandbox,
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

          # Per-project sandboxing helpers (no orchestration tools)
          lib = rec {
            inherit (defaults) sandboxPackages allowedDomains;
            mkSandbox = agent-sandbox.lib.${system}.mkSandbox;

            # Wrap claude-code with project-specific sandbox settings.
            #
            # Usage in a project flake:
            #   inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";
            #   devShells.default = inputs.ai-dev.lib.x86_64-linux.mkProjectShell { };
            #
            # Or with overrides:
            #   devShells.default = inputs.ai-dev.lib.x86_64-linux.mkProjectShell {
            #     extraDomains = [ "pypi.org" "files.pythonhosted.org" ];
            #     extraPackages = [ pkgs.python3 pkgs.poetry ];
            #   };
            mkSandboxedClaude =
              {
                extraDomains ? [ ],
                extraPackages ? [ ],
                extraStateDirs ? [ ],
                extraStateFiles ? [ ],
                extraEnv ? { },
              }:
              mkSandbox {
                pkg = agents.claude-code;
                binName = "claude";
                outName = "claude";
                allowedPackages = defaults.sandboxPackages ++ extraPackages;
                stateDirs = defaults.claudeStateDirs ++ extraStateDirs;
                stateFiles = defaults.claudeStateFiles ++ extraStateFiles;
                restrictNetwork = true;
                allowedDomains = defaults.allowedDomains ++ extraDomains;
                inherit extraEnv;
              };

            mkSandboxedOpencode =
              {
                extraDomains ? [ ],
                extraPackages ? [ ],
                extraStateDirs ? [ ],
                extraEnv ? { },
              }:
              mkSandbox {
                pkg = agents.opencode;
                binName = "opencode";
                outName = "opencode";
                allowedPackages = defaults.sandboxPackages ++ extraPackages;
                stateDirs = defaults.opencodeStateDirs ++ extraStateDirs;
                restrictNetwork = true;
                allowedDomains = defaults.allowedDomains ++ extraDomains;
                inherit extraEnv;
              };

            # Convenience: produce a devShell with sandboxed claude + opencode.
            # Orchestration tools (agent-deck, zellij) are not included —
            # they run outside the sandbox and pick up whatever is on $PATH.
            mkProjectShell =
              {
                extraDomains ? [ ],
                extraPackages ? [ ],
                extraStateDirs ? [ ],
                extraStateFiles ? [ ],
                extraEnv ? { },
                extraShellPackages ? [ ],
              }:
              pkgs.mkShell {
                packages = [
                  (mkSandboxedClaude {
                    inherit
                      extraDomains
                      extraPackages
                      extraStateDirs
                      extraStateFiles
                      extraEnv
                      ;
                  })
                  (mkSandboxedOpencode {
                    inherit
                      extraDomains
                      extraPackages
                      extraStateDirs
                      extraEnv
                      ;
                  })
                ]
                ++ extraShellPackages;
              };
          };
        };
    in
    {
      packages = forAllSystems (system: (mkForSystem system).packages);
      devShells = forAllSystems (system: {
        default = (mkForSystem system).devShell;
      });
      apps = forAllSystems (system: (mkForSystem system).apps);
      lib = forAllSystems (system: (mkForSystem system).lib);

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
