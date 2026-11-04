#!/usr/bin/env bash
# Run one program through the pinned legacy oracle (scripts/oracle/ORACLE.lock).
#
#   run.sh [--interp] <file.con> [-- args...]   compile natively and run (default), or interpret
#   run.sh --path                               print the verified binary's path and exit
#
# The installed binary ($ORACLE_HOME/<commit>/, made by build.sh) is hashed before every use and must
# equal the lock's binary_sha256; anything else is refused, never run. The program is compiled and run
# inside the tag's own dev shell, so the C toolchain is the one the lock pins. The exit status and
# output are the program's; a compile failure exits with the compiler's status, prefixed by a line
# "oracle: compile failed" on stderr so a caller can tell the two apart.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOCK="$HERE/ORACLE.lock"
lock() { sed -n "s/^$1=//p" "$LOCK" | head -1; }
die() { echo "oracle: $*" >&2; exit 125; }
COMMIT="$(lock commit)"; WANT="$(lock binary_sha256)"
DEST="${ORACLE_HOME:-$HOME/oracle}/$COMMIT"
SRC="$DEST/src"; BIN="$SRC/.lake/build/bin/concrete"
[ -x "$BIN" ] || die "no oracle installed at $DEST (run scripts/oracle/build.sh)"
GOT="$(sha256sum "$BIN" | cut -d' ' -f1)"
[ -n "$WANT" ] && [ "$GOT" = "$WANT" ] || die "installed binary is $GOT, the lock says ${WANT:-nothing}; refusing to run it"
[ "${1:-}" = "--path" ] && { echo "$BIN"; exit 0; }
mode=native
[ "${1:-}" = "--interp" ] && { mode=interp; shift; }
[ $# -ge 1 ] || { echo "usage: run.sh [--interp] <file.con> [-- args...] | --path" >&2; exit 2; }
FILE="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"; shift
[ "${1:-}" = "--" ] && shift
NIX=(nix --extra-experimental-features "nix-command flakes")
OUT="$(mktemp -d "${TMPDIR:-/tmp}/oracle-run.XXXXXX")"; trap 'rm -rf "$OUT"' EXIT
if [ "$mode" = interp ]; then
  exec "${NIX[@]}" develop "$SRC" --command "$BIN" "$FILE" --interp -- "$@"
fi
"${NIX[@]}" develop "$SRC" --command "$BIN" "$FILE" -o "$OUT/prog" >"$OUT/compile.log" 2>&1 || {
  rc=$?; echo "oracle: compile failed ($rc)" >&2; cat "$OUT/compile.log" >&2; exit "$rc"; }
"$OUT/prog" "$@"
