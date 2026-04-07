# zerobox-sandbox.nix — mkSandbox wrapper around zerobox (Linux only).
#
# Generates a writeShellApplication that invokes zerobox with the correct
# --allow-read / --allow-write / --allow-net / --env flags.
#
# Darwin is not supported: zerobox's Seatbelt profile explicitly denies
# macOS Keychain access ($HOME/Library/Keychains) and omits the securityd
# Mach services that tools like claude-code need for OAuth credential storage.
{ pkgs }:
let
  zerobox = pkgs.callPackage ./pkgs/zerobox.nix { };
in
{
  mkSandbox =
    {
      pkg,
      binName,
      outName,
      packages ? [ ],
      allowRead ? [ ],
      allowWrite ? [ ],
      # Domain allowlist.  true = unrestricted network; [] = no network.
      allowNet ? [ ],
      env ? { },
    }:
    assert
      pkgs.stdenv.isLinux
      || throw "mkSandbox is Linux-only; zerobox's Seatbelt profile blocks macOS Keychain access";
    let
      # Nix store closure for all packages so zerobox can exec binaries.
      closurePaths = pkgs.writeClosure (packages ++ [ pkg ]);

      # --allow-write implies --allow-read in zerobox, but we pass both
      # explicitly so the intent is clear when debugging wrapper scripts.
      writeFlags = builtins.concatStringsSep " " (
        map (p: ''--allow-write "${p}" --allow-read "${p}"'') allowWrite
      );
      readFlags = builtins.concatStringsSep " " (map (p: ''--allow-read "${p}"'') allowRead);

      # Network flags
      networkFlags =
        if allowNet == true then
          "--allow-net"
        else
          builtins.concatStringsSep " " (map (d: ''--allow-net "${d}"'') allowNet);

      # Environment flags
      envFlags = builtins.concatStringsSep " " (
        map (name: ''--env "${name}=${builtins.toJSON env.${name}}"'') (builtins.attrNames env)
      );

      # PATH for allowed packages
      pathStr = pkgs.lib.makeBinPath (packages ++ [ pkg ]);
    in
    pkgs.writeShellApplication {
      name = outName;
      runtimeInputs = [
        zerobox
        pkgs.coreutils
      ];
      text = ''
        CWD=$(pwd)

        # Ensure paths exist (skip files that already exist)
        ${builtins.concatStringsSep "\n" (
          map (p: ''if [ ! -f "${p}" ]; then mkdir -p "${p}"; fi'') allowWrite
        )}
        ${builtins.concatStringsSep "\n" (
          map (p: ''if [ ! -f "${p}" ]; then mkdir -p "${p}"; fi'') allowRead
        )}

        # Build --allow-read flags for nix store closure
        CLOSURE_READ_FLAGS=""
        while IFS= read -r storePath; do
          CLOSURE_READ_FLAGS="$CLOSURE_READ_FLAGS --allow-read $storePath"
        done < ${closurePaths}

        # Resolve symlink chains for a path and emit --allow-read flags for
        # each intermediate target.  On NixOS, paths like /etc/ssl and
        # $HOME/.config/claude are commonly symlinks into the nix store.
        # bwrap mounts literal paths, so we must also allow the real targets.
        resolve_symlink_flags() {
          local path="$1"
          local mode="$2"  # "read" or "write"
          local current="$path"
          local seen=""
          while [ -L "$current" ]; do
            target=$(readlink "$current")
            # Handle relative symlinks
            if [[ "$target" != /* ]]; then
              target="$(dirname "$current")/$target"
            fi
            # Canonicalize (remove /../, /./  etc.)
            target=$(cd "$(dirname "$target")" 2>/dev/null && echo "$(pwd)/$(basename "$target")" || echo "$target")
            # Avoid loops
            if [[ " $seen " == *" $target "* ]]; then
              break
            fi
            seen="$seen $target"
            echo "--allow-read $target"
            if [ "$mode" = "write" ]; then
              echo "--allow-write $target"
            fi
            current="$target"
          done
        }

        # Resolve symlinks for all configured paths
        SYMLINK_FLAGS=""
        ${pkgs.lib.optionalString (allowWrite != [ ]) ''
          for p in ${builtins.concatStringsSep " " (map (p: ''"${p}"'') allowWrite)}; do
            if [ -e "$p" ]; then
              SYMLINK_FLAGS="$SYMLINK_FLAGS $(resolve_symlink_flags "$p" write)"
            fi
          done
        ''}
        ${pkgs.lib.optionalString (allowRead != [ ]) ''
          for p in ${builtins.concatStringsSep " " (map (p: ''"${p}"'') allowRead)}; do
            if [ -e "$p" ]; then
              SYMLINK_FLAGS="$SYMLINK_FLAGS $(resolve_symlink_flags "$p" read)"
            fi
          done
        ''}
        # Also resolve system paths that are symlinks on NixOS
        for p in /etc/resolv.conf /etc/ssl /etc/passwd /etc/static; do
          if [ -e "$p" ]; then
            SYMLINK_FLAGS="$SYMLINK_FLAGS $(resolve_symlink_flags "$p" read)"
          fi
        done

        # Use the real TMPDIR (macOS uses /var/folders/..., not /tmp)
        REAL_TMPDIR="''${TMPDIR:-/tmp}"

        # shellcheck disable=SC2086
        exec zerobox \
          --allow-read "$CWD" \
          --allow-write "$CWD" \
          --allow-read /etc/passwd \
          --allow-read /etc/resolv.conf \
          --allow-read /etc/ssl \
          --allow-read /etc/static \
          --allow-read "$REAL_TMPDIR" \
          --allow-write "$REAL_TMPDIR" \
          --allow-read /tmp \
          --allow-write /tmp \
          $CLOSURE_READ_FLAGS \
          $SYMLINK_FLAGS \
          ${writeFlags} \
          ${readFlags} \
          ${networkFlags} \
          ${envFlags} \
          --env "PATH=${pathStr}" \
          --env "HOME=$HOME" \
          --env "TERM=$TERM" \
          --env "TMPDIR=$REAL_TMPDIR" \
          -C "$CWD" \
          -- ${pkg}/bin/${binName} "$@"
      '';
    };
}
