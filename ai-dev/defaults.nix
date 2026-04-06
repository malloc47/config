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

  # State paths for Claude Code
  claudeStateDirs = [
    ".claude"
    ".config/claude"
    "$HOME/.claude"
    "$HOME/.config/claude"
  ];

  claudeStateFiles = [
    "$HOME/.claude.json"
    "$HOME/.claude.json.lock"
  ];

  # State paths for OpenCode
  opencodeStateDirs = [
    ".opencode"
    ".config/opencode"
    "$HOME/.opencode"
    "$HOME/.config/opencode"
  ];
}
