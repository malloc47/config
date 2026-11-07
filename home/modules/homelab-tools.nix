{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.homelab-tools;

  # Fold a host's live, UI-editable config files back into the repo baseline
  # so a deploy stays a faithful backup of clickops edits. Review the printed
  # diff and commit; the commit is the backup.
  foldin =
    {
      name,
      stateDir,
      subdir,
      files,
      requireNonEmpty,
    }:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = [
        pkgs.openssh
        pkgs.coreutils
      ];
      text = ''
        host="''${1:-aida}"
        dst="${cfg.repo}/hosts/$host/${subdir}"
        mkdir -p "$dst"
        for f in ${lib.escapeShellArgs files}; do
          tmp="$(mktemp)"
          # shellcheck disable=SC2029 # expand $f locally on purpose
          if ssh "$host" sudo cat "${stateDir}/$f" > "$tmp" 2>/dev/null${lib.optionalString requireNonEmpty " && [ -s \"$tmp\" ]"}; then
            mv "$tmp" "$dst/$f"
          else
            rm -f "$tmp"
            echo "${name}: failed to fetch $f from $host" >&2
            exit 1
          fi
        done
        echo "${name}: updated $dst; review and commit:" >&2
        git -C "${cfg.repo}" diff -- "hosts/$host/${subdir}"
      '';
    };
in
{
  options.programs.homelab-tools = {
    enable = lib.mkEnableOption "homelab operator commands (esphome-deploy, z2m-foldin, ha-foldin)";

    repo = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/src/config/homelab";
      description = "Checkout of the homelab tree whose hosts/<host>/ these commands read and write.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      # Compile and OTA-flash ESPHome devices from the repo configs that
      # `nixos-deploy <host>` installed (hosts/<host>/esphome; docs/esphome.md).
      (pkgs.writeShellApplication {
        name = "esphome-deploy";
        runtimeInputs = [
          pkgs.openssh
          pkgs.coreutils
          pkgs.diffutils
        ];
        text = ''
          host=aida
          if [ "''${1-}" = "-H" ]; then
            host="$2"
            shift 2
          fi
          src="${cfg.repo}/hosts/$host/esphome"
          all=()
          for f in "$src"/*.yaml; do
            if [ -e "$f" ]; then all+=("$(basename "$f" .yaml)"); fi
          done
          if [ $# -eq 0 ]; then
            echo "usage: esphome-deploy [-H host] <device>... | --all" >&2
            echo "devices on $host: ''${all[*]}" >&2
            exit 2
          fi
          if [ "$1" = "--all" ]; then
            set -- "''${all[@]}"
          fi

          # Refuse to flash anything but the working tree: the host's copies
          # only change on `nixos-deploy`.
          names=()
          for dev in "$@"; do
            [ -f "$src/$dev.yaml" ] || {
              echo "esphome-deploy: no $src/$dev.yaml" >&2
              exit 1
            }
            names+=("$dev.yaml")
          done
          ours="$(cd "$src" && sha256sum -- "''${names[@]}" common/*)"
          # shellcheck disable=SC2029 # expand the names locally on purpose
          theirs="$(ssh "$host" sudo sh -c "'cd /var/lib/esphome && sha256sum -- ''${names[*]} common/*'")"
          if [ "$ours" != "$theirs" ]; then
            echo "esphome-deploy: $host's configs differ from $src; run 'nixos-deploy $host' first:" >&2
            diff <(echo "$ours") <(echo "$theirs") >&2 || true
            exit 1
          fi

          failed=()
          for dev in "$@"; do
            unit="esphome-deploy@$dev.service"
            # Follow the unit's log while `systemctl start` blocks on the oneshot.
            # shellcheck disable=SC2029 # expand $unit locally on purpose
            if ! ssh "$host" "sudo journalctl -f -n0 -o cat -u $unit & j=\$!; sudo systemctl start $unit; rc=\$?; sleep 1; sudo kill \$j; exit \$rc"; then
              echo "esphome-deploy: $dev failed; see 'journalctl -u $unit' on $host" >&2
              failed+=("$dev")
            fi
          done
          if [ ''${#failed[@]} -gt 0 ]; then
            echo "esphome-deploy: failed: ''${failed[*]}" >&2
            exit 1
          fi
        '';
      })

      (foldin {
        name = "z2m-foldin";
        stateDir = "/var/lib/zigbee2mqtt";
        subdir = "zigbee2mqtt";
        files = [
          "devices.yaml"
          "groups.yaml"
        ];
        requireNonEmpty = true;
      })

      (foldin {
        name = "ha-foldin";
        stateDir = "/var/lib/hass";
        subdir = "home-assistant";
        files = [
          "automations.yaml"
          "scenes.yaml"
          "scripts.yaml"
        ];
        requireNonEmpty = false;
      })
    ];
  };
}
