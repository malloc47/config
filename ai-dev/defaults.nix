# Shared defaults for per-project sandboxes.
# Projects inherit these and override what they need.
{ pkgs }:
{
  # Utility packages available inside every sandbox
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

  # Domains reachable from inside a sandbox (superset — projects can narrow)
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

  # Writable paths for Claude Code
  claudeWritePaths = [
    ".claude"
    ".config/claude"
    "$HOME/.claude"
    "$HOME/.config/claude"
    "$HOME/.claude.json"
    "$HOME/.claude.json.lock"
  ];

  # Writable paths for OpenCode
  opencodeWritePaths = [
    ".opencode"
    ".config/opencode"
    "$HOME/.opencode"
    "$HOME/.config/opencode"
  ];
}
