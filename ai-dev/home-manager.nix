# Combined module: enables both sandbox tools and session orchestration.
# For finer control, import sandbox.nix and session.nix separately.
#
#   imports = [ inputs.ai-dev.homeManagerModules.default ];
#   programs.ai-sandbox.enable = true;
#   programs.ai-session.enable = true;
{ self, ... }:
{
  imports = [
    (import ./sandbox.nix { inherit self; })
    (import ./session.nix { inherit self; })
  ];
}
