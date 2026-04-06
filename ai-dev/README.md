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

Add a `flake.nix` in the project root:

```nix
{
  inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";

  outputs = { ai-dev, ... }:
    let system = "x86_64-linux";
    in {
      devShells.${system}.default = ai-dev.lib.${system}.mkProjectShell {
        # Everything below is optional — defaults cover standard Anthropic/GitHub domains
        extraDomains = [ "pypi.org" "files.pythonhosted.org" ];
        extraPackages = [ /* project-specific tools */ ];
      };
    };
}
```

Then `nix develop` gives you sandboxed `claude` and `opencode` scoped to that project. Orchestration tools (agent-deck, zellij-ai) are installed system-wide via the session layer and run outside the sandbox.

### Available lib functions

**`mkProjectShell`** — returns a `mkShell` with sandboxed claude + opencode:

```nix
mkProjectShell {
  extraDomains ? [];       # domains to add to the default allowlist
  extraPackages ? [];      # packages available inside the sandbox
  extraStateDirs ? [];     # additional read-write directories
  extraStateFiles ? [];    # additional read-write files
  extraEnv ? {};           # environment variables passed into the sandbox
  extraShellPackages ? []; # packages added to the devShell (outside sandbox)
}
```

**`mkSandboxedClaude`** / **`mkSandboxedOpencode`** — lower-level, returns a single sandboxed package:

```nix
mkSandboxedClaude {
  extraDomains ? [];
  extraPackages ? [];
  extraStateDirs ? [];
  extraStateFiles ? [];
  extraEnv ? {};
}
```

**`mkSandbox`** — re-export of `agent-sandbox.lib.${system}.mkSandbox` for full control.

**`sandboxPackages`** / **`allowedDomains`** — the default lists, for inspection or extension in custom `mkSandbox` calls.

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
