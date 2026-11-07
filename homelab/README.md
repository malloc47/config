# homelab

The home network: `aida` (Home Assistant, Zigbee/Z-Wave/Matter, ESPHome,
Authelia, Caddy, the e-ink and Kindle dashboards) and `aroldo` (Headscale +
DERP, ntfy, gatus, smokeping). None of this is relevant to work machines, so
it is kept apart from the shared base that work-config consumes.

## Layout

| Path | Contents |
|---|---|
| `default.nix` | `{ inputs, base }` → `{ nixosConfigurations, overlay }` for aida and aroldo |
| `hosts/` | Host configs plus aida's repo-managed app config (`hosts/aida/{home-assistant,esphome,zigbee2mqtt,kindle-weather}`) |
| `modules/` | `home-automation.nix` (aida's HA stack) and the modules it pulls in; `netboot.nix` |
| `hardware/`, `disk/` | gmktec-g10 (aida) and racknerd-vps (aroldo) |
| `pkgs/` | Overlay of HA components, Lovelace cards, ha-puppet, adguardian |
| `secrets/` | agenix secrets for these hosts, with their own `secrets.nix` |
| `docs/` | Runbooks for everything above |

Paths mentioned in files under `homelab/` (comments, docs, commands such as
`cd secrets`) are relative to `homelab/`, so run commands from here. Paths in
the shared base are written as `config: <path>`.

## Boundary

- `homelab/` reaches the base only through `base.*` flake outputs
  (`base.nixosModules.*`, `base.homeManagerModules.*`), never by relative path.
- Nothing in the base imports from `homelab/`; only the top-level `flake.nix`
  wires `default.nix` in, passing `base = self`.

The `homelab-boundary` flake check enforces both. The point is that this
directory can be lifted into its own flake (with `config` as an input and
`base = inputs.config`) without changes to its contents.

The operator commands that act on this tree from a workstation
(`esphome-deploy`, `z2m-foldin`, `ha-foldin`) live in the base as the opt-in
`homeManagerModules.homelab-tools`; `programs.homelab-tools.repo` points them
at this directory.
