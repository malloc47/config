# ai-dev

Portable subflake providing sandboxed AI coding tools (`claude-code`, `opencode`, `agent-deck`) and a `zellij-ai` session-manager wrapper with a baked-in config. Binaries are wrapped with bubblewrap and a domain-filtering HTTP proxy via [agent-sandbox.nix](https://github.com/archie-judd/agent-sandbox.nix); packaging comes from [llm-agents.nix](https://github.com/numtide/llm-agents.nix).

The subflake is designed to run identically on NixOS, non-NixOS Linux, and macOS hosts that have `nix` installed, so the same tooling follows you onto loaner machines and work VMs.

## Usage

### 1. NixOS / home-manager (declarative)

From a flake that already has home-manager wired up:

```nix
{
  inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";
  # ...
}
```

Then in the home-manager user config:

```nix
{
  imports = [ inputs.ai-dev.homeManagerModules.default ];
  programs.ai-dev.enable = true;
  # programs.ai-dev.installXdgZellijConfig = true;  # opt-in: write zellij config into ~/.config
}
```

### 2. Non-NixOS Linux (Ubuntu work VM, loaner box)

Install [nix](https://nixos.org/download) (the Determinate Systems installer is a good default), then:

```bash
# Persistent install into user profile
nix profile install github:malloc47/config?dir=ai-dev

# Or ephemeral shell
nix develop github:malloc47/config?dir=ai-dev

# Or one-shot run
nix run github:malloc47/config?dir=ai-dev#claude
```

**Ubuntu 24.04 note:** bubblewrap user namespaces are blocked by default AppArmor. One-time fix:

```bash
sudo tee /etc/apparmor.d/bwrap <<'EOF'
abi <abi/4.0>,
include <tunables/global>
profile bwrap /usr/bin/bwrap flags=(unconfined) {
  userns,
  include if exists <local/bwrap>
}
EOF
sudo systemctl reload apparmor
```

### 3. Per-project flake input

Add the subflake to a project's `flake.nix` to get the same sandboxed tools in its devShell:

```nix
{
  inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";

  outputs = { self, nixpkgs, ai-dev }: {
    devShells.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShell {
      packages = [ ai-dev.packages.x86_64-linux.ai-dev-env ];
    };
  };
}
```

## Packages

- `claude` — sandboxed Claude Code CLI
- `opencode` — sandboxed OpenCode CLI
- `agent-deck` — multi-agent orchestration TUI
- `zellij-ai` — Zellij wrapper that loads the baked-in config
- `ai-dev-env` — meta package bundling all of the above (default package)

## Remote Zellij web access

`zellij-ai` starts a web server on `127.0.0.1:8082`. To reach it from another machine:

```bash
ssh -L 8082:localhost:8082 <host>
ssh <host> zellij-ai web --create-token dev
# then browse http://localhost:8082
```
