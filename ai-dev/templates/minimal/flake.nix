{
  description = "Minimal dev environment with sandboxed Claude Code";

  inputs.ai-dev.url = "github:malloc47/config?dir=ai-dev";

  outputs =
    { ai-dev, ... }:
    {
      devShells = ai-dev.lib.forAllSystems (
        system:
        let
          ai = ai-dev.lib.${system};
        in
        {
          default = ai.mkProjectShell {
            harnesses = [ "claude-code" ];
          };
        }
      );
    };
}
