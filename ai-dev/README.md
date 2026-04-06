# ai-dev

Portable subflake providing AI coding tools and per-project sandboxing helpers.

**System-level**: installs unwrapped `claude-code`, `opencode`, `agent-deck`, and a `zellij-ai` session-manager wrapper. These go into the user environment via home-manager or `nix profile install`.

**Project-level**: `lib.mkProjectShell`, `lib.mkSandboxedClaude`, and `lib.mkSandboxedOpencode` produce sandboxed wrappers with bubblewrap + domain-filtering proxy via [agent-sandbox.nix](https://github.com/archie-judd/agent-sandbox.nix). Each project customizes its own allowed domains, packages, and state paths.

## System-level install

### NixOS / home-manager (declarative)

```nix
{
  inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";
}
```

```nix
{
  imports = [ inputs.ai-dev.homeManagerModules.default ];
  programs.ai-dev.enable = true;
}
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
        extraPackages = p: with p; [ python3 poetry ];
      };
    };
}
```

Then `nix develop` gives you a sandboxed `claude` and `opencode` scoped to that project.

### Available lib functions

**`mkProjectShell`** — convenience that returns a `mkShell` with sandboxed claude + opencode + raw agent-deck:

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

**`sandboxPackages`** / **`allowedDomains`** — the default lists, if you want to inspect or extend them in a custom `mkSandbox` call.

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
