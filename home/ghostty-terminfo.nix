# Standalone module that installs a patched xterm-ghostty terminfo entry.
# Useful on any host that might be SSHed into from a Ghostty terminal.
# The upstream terminfo ships colors#256; this patches it to colors#16777216
# so Emacs's C-level init_tty enables truecolor in emacsclient -t.
{
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) stdenv;
  # On macOS (Homebrew install) the terminfo lives inside the .app bundle.
  # On Linux the ghostty package puts it in $out/share/terminfo.
  ghosttyTerminfo =
    if stdenv.isDarwin then
      "/Applications/Ghostty.app/Contents/Resources/terminfo"
    else
      "${pkgs.ghostty}/share/terminfo";
  # Use nix's ncurses 6.x (not macOS system ncurses 5.7) — the system
  # tic only supports 16-bit numeric values, truncating 16777216 to 256.
  infocmp = "${pkgs.ncurses}/bin/infocmp";
  tic = "${pkgs.ncurses}/bin/tic";
  sed = "${pkgs.gnused}/bin/sed";
in
{
  home.activation.ghostty-terminfo = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    GHOSTTY_TERMINFO="${ghosttyTerminfo}"
    if [ -d "$GHOSTTY_TERMINFO" ]; then
      # Remove stale entry so infocmp reads the upstream source, not our
      # own previously-compiled file.  Clear TERMINFO_DIRS to prevent any
      # wrapper or shell-integration paths from interfering.
      rm -f "$HOME/.terminfo/78/xterm-ghostty" "$HOME/.terminfo/x/xterm-ghostty"
      env TERMINFO="$GHOSTTY_TERMINFO" TERMINFO_DIRS="" \
        ${infocmp} -x xterm-ghostty 2>/dev/null \
        | ${sed} 's/colors#\(256\|0x100\)/colors#16777216/' \
        | ${tic} -x -o "$HOME/.terminfo" - 2>/dev/null || true
    fi
  '';
}
