{
  description = "Sandboxed AI coding tools (claude-code, opencode, agent-deck) with Zellij wrapper. Portable across NixOS, non-NixOS Linux, and macOS hosts with nix installed.";

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
          mkSandbox = agent-sandbox.lib.${system}.mkSandbox;

          sandboxPackages = with pkgs; [
            coreutils
            git
            ripgrep
            fd
            gnused
            gnugrep
            findutils
            jq
            which
            nodejs
            curl
            openssh
            diffutils
            patch
            gnutar
            gzip
          ];

          allowedDomains = [
            # Anthropic: API + OAuth login + telemetry
            "api.anthropic.com"
            "platform.claude.com"
            "console.anthropic.com"
            "statsig.anthropic.com"
            "sentry.io"
            # OpenAI (for opencode)
            "api.openai.com"
            # GitHub: git operations + npm registry
            "github.com"
            "api.github.com"
            "objects.githubusercontent.com"
            "registry.npmjs.org"
          ];

          claude = mkSandbox {
            pkg = agents.claude-code;
            binName = "claude";
            outName = "claude";
            allowedPackages = sandboxPackages;
            stateDirs = [
              # CWD-relative: per-project state
              ".claude"
              ".config/claude"
              # HOME-relative: credentials, history, settings
              # $HOME is expanded by bash at runtime, before bwrap overlays tmpfs
              "$HOME/.claude"
              "$HOME/.config/claude"
            ];
            stateFiles = [
              "$HOME/.claude.json"
              "$HOME/.claude.json.lock"
            ];
            restrictNetwork = true;
            inherit allowedDomains;
          };

          opencode = mkSandbox {
            pkg = agents.opencode;
            binName = "opencode";
            outName = "opencode";
            allowedPackages = sandboxPackages;
            stateDirs = [
              ".opencode"
              ".config/opencode"
              "$HOME/.opencode"
              "$HOME/.config/opencode"
            ];
            restrictNetwork = true;
            inherit allowedDomains;
          };

          # Zellij wrapper with config.kdl baked in via store path.
          # Users run `zellij-ai` for the AI dev config; plain `zellij` still works for other uses.
          zellij-ai = pkgs.writeShellApplication {
            name = "zellij-ai";
            runtimeInputs = [ pkgs.zellij ];
            text = ''
              exec zellij --config ${./zellij-config.kdl} "$@"
            '';
          };

          # Meta package: installs everything above in one go
          ai-dev-env = pkgs.buildEnv {
            name = "ai-dev-env";
            paths = [
              claude
              opencode
              agents.agent-deck
              zellij-ai
              pkgs.zellij
            ];
          };
        in
        {
          packages = {
            inherit
              claude
              opencode
              zellij-ai
              ai-dev-env
              ;
            agent-deck = agents.agent-deck;
            default = ai-dev-env;
          };

          devShell = pkgs.mkShell {
            packages = [ ai-dev-env ];
            shellHook = ''
              echo "ai-dev environment ready. Try: claude, opencode, agent-deck, zellij-ai"
            '';
          };

          apps = {
            claude = {
              type = "app";
              program = "${claude}/bin/claude";
            };
            opencode = {
              type = "app";
              program = "${opencode}/bin/opencode";
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
        };
    in
    {
      packages = forAllSystems (system: (mkForSystem system).packages);
      devShells = forAllSystems (system: {
        default = (mkForSystem system).devShell;
      });
      apps = forAllSystems (system: (mkForSystem system).apps);

      homeManagerModules.default = import ./home-manager.nix { inherit self; };
    };
}
