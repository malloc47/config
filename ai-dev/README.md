# ai-dev

Portable subflake with two independent layers:

1. **Sandbox layer** (`programs.ai-sandbox`): installs unwrapped `claude-code` and `opencode`; exposes `lib` helpers for per-project sandboxing via [agent-sandbox.nix](https://github.com/archie-judd/agent-sandbox.nix).
2. **Session layer** (`programs.ai-session`): installs `agent-deck`, `zellij-ai`, and `zellij` for multi-agent orchestration. Runs outside any sandbox — launches whatever `claude`/`opencode` is on `$PATH`.

## System-level install

### NixOS / home-manager (declarative)

```nix
# flake inputs
inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";
```

```nix
# home-manager config
imports = [ inputs.ai-dev.homeManagerModules.default ];  # imports both layers

programs.ai-sandbox.enable = true;  # claude-code, opencode
programs.ai-session.enable = true;  # agent-deck, zellij-ai, zellij
```

Or import layers individually:

```nix
imports = [ inputs.ai-dev.homeManagerModules.sandbox ];
programs.ai-sandbox.enable = true;
```

```nix
imports = [ inputs.ai-dev.homeManagerModules.session ];
programs.ai-session.enable = true;
# programs.ai-session.installXdgZellijConfig = true;  # opt-in: write to ~/.config/zellij/
```

### Non-NixOS Linux / standalone nix

```bash
nix profile install github:malloc47/config?dir=ai-dev
```

## Per-project sandbox

Scaffold a project with a template:

```bash
nix flake init -t github:malloc47/config?dir=ai-dev          # default (claude + opencode)
nix flake init -t github:malloc47/config?dir=ai-dev#minimal  # claude only
```

Or add a `flake.nix` manually:

```nix
{
  inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";

  outputs = { ai-dev, ... }:
    ai-dev.lib.forAllSystems (system:
      let ai = ai-dev.lib.${system}; in {
        devShells.${system}.default = ai.mkProjectShell {
          harnesses = [ "claude-code" "opencode" ];
          profiles = with ai.profiles; [ github python ];
        };
      });
}
```

Then `nix develop` gives you sandboxed `claude` and `opencode` scoped to that project. On Linux, agents run inside a bubblewrap sandbox with network filtering. On Darwin, agents run unwrapped (bubblewrap is Linux-only). Orchestration tools (agent-deck, zellij-ai) are installed system-wide via the session layer and run outside the sandbox.

### Harnesses

A harness wraps an AI agent with sandbox-appropriate defaults (state dirs, domains, CLI flags). Built-in harnesses:

| Harness | Binary | Auto-configured |
|---------|--------|-----------------|
| `"claude-code"` | `claude` | `~/.claude`, `~/.config/claude`, Anthropic domains, `--add-dir` for extra state dirs |
| `"opencode"` | `opencode` | `~/.opencode`, `~/.config/opencode`, OpenAI domains |

You can also pass a raw derivation as a harness for generic sandboxing.

### Profiles

Profiles bundle packages, domains, and state paths for a tool ecosystem:

| Profile | Packages | Domains | State |
|---------|----------|---------|-------|
| `github` | `gh` | github.com, api.github.com | `~/.config/gh` |
| `python` | `python3`, `pip` | pypi.org, files.pythonhosted.org | `~/.cache/pip` |
| `node` | `nodejs` | registry.npmjs.org, registry.yarnpkg.com | `~/.npm`, `node_modules` |
| `rust` | `rustc`, `cargo` | crates.io, static.crates.io, index.crates.io | `~/.cargo`, `target` |
| `aws` | `awscli2` | sts.amazonaws.com | `~/.aws` |
| `docker` | `docker-client` | — | `~/.docker` |
| `nix` | `nix`, `nixfmt-rfc-style` | cache.nixos.org | — |

### Available lib functions

**`mkProjectShell`** — returns a `mkShell` with sandboxed harnesses:

```nix
mkProjectShell {
  harnesses ? [ "claude-code" "opencode" ];
  profiles ? [];               # composable tool profiles
  restrictNetwork ? true;      # network deny-by-default
  unrestrictedHarness ? false; # --dangerously-skip-permissions for claude
  extraDomains ? [];           # domains to add to the allowlist
  extraPackages ? [];          # packages available inside the sandbox
  extraStateDirs ? [];         # additional read-write directories (also passed as --add-dir to claude)
  extraStateFiles ? [];        # additional read-write files
  extraEnv ? {};               # environment variables passed into the sandbox
  extraShellPackages ? [];     # packages added to the devShell (outside sandbox)
}
```

**`mkSandboxedHarness`** — lower-level, returns a single sandboxed package:

```nix
mkSandboxedHarness "claude-code" {
  profiles ? [];
  restrictNetwork ? true;
  unrestrictedHarness ? false;
  extraDomains ? [];
  extraPackages ? [];
  extraStateDirs ? [];
  extraStateFiles ? [];
  extraEnv ? {};
}
```

**`profiles`** — the built-in profile set, for use with `with ai.profiles; [ github python ]`.

**`mkSandbox`** — re-export of `agent-sandbox.lib.${system}.mkSandbox` for full control.

**`sandboxPackages`** / **`allowedDomains`** — the default lists, for inspection or extension.

**`forAllSystems`** — available at `ai-dev.lib.forAllSystems` for project flakes to avoid hardcoding system strings.

### Default allowed domains

- `api.anthropic.com`, `platform.claude.com`, `console.anthropic.com`, `statsig.anthropic.com`, `sentry.io`
- `api.openai.com`
- `github.com`, `api.github.com`, `objects.githubusercontent.com`, `registry.npmjs.org`

### Default sandbox packages

coreutils, git, ripgrep, fd, gnused, gnugrep, findutils, jq, which, nodejs, curl, openssh, diffutils, patch, gnutar, gzip

## Remote Zellij web access

`zellij-ai` starts a web server on `127.0.0.1:8082`. To reach it from another machine:

```bash
ssh -L 8082:localhost:8082 <host>
ssh <host> zellij-ai web --create-token dev
# then browse http://localhost:8082
```
