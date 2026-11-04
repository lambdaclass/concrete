#!/usr/bin/env bash
# Run one program through the pinned legacy oracle (scripts/oracle/ORACLE.lock).
#
#   run.sh [--interp] <file.con> [-- args...]   compile natively and run (default), or interpret
#   run.sh --path                               print the verified binary's path and exit
#
# The installed binary ($ORACLE_HOME/<commit>/, made by build.sh) is hashed before every use and must
# equal the lock's binary_sha256, and the checkout it reads (std/, flake.nix, flake.lock) must be the
# locked commit with no change and no added file; anything else is refused, never run. The program is compiled and run
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
# The binary is not the whole oracle: it reads std/ from the checkout and runs inside the checkout's
# dev shell (flake.nix, flake.lock). So the checkout itself must be the locked commit, unmodified,
# with nothing added: any tracked change or untracked file outside the ignored build output refuses.
[ "$(git -C "$SRC" rev-parse HEAD 2>/dev/null)" = "$COMMIT" ] || die "the installed checkout is not at $COMMIT"
dirty="$(git -C "$SRC" status --porcelain --untracked-files=all 2>&1)" || die "cannot read the installed checkout's status"
[ -z "$dirty" ] || { printf '%s\n' "$dirty" | head -5 >&2; die "the installed checkout differs from $COMMIT (above); rebuild it with build.sh"; }
[ "${1:-}" = "--path" ] && { echo "$BIN"; exit 0; }
mode=native
[ "${1:-}" = "--interp" ] && { mode=interp; shift; }
[ $# -ge 1 ] || { echo "usage: run.sh [--interp] <file.con> [-- args...] | --path" >&2; exit 2; }
FILE="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"; shift
[ "${1:-}" = "--" ] && shift
# The compiler reads three environment variables (git grep getEnv at the tag): CONCRETE_STD selects
# the standard library and the other two change behaviour, so each is fixed here, never inherited.
NIX=(env -u CONCRETE_MODEL_DIFF_PERTURB -u PROOF_LAYER_MODE CONCRETE_STD="$SRC/std"
     nix --extra-experimental-features "nix-command flakes")
OUT="$(mktemp -d "${TMPDIR:-/tmp}/oracle-run.XXXXXX")"; trap 'rm -rf "$OUT"' EXIT
if [ "$mode" = interp ]; then
  exec "${NIX[@]}" develop "$SRC" --command "$BIN" "$FILE" --interp -- "$@"
fi
"${NIX[@]}" develop "$SRC" --command "$BIN" "$FILE" -o "$OUT/prog" >"$OUT/compile.log" 2>&1 || {
  rc=$?; echo "oracle: compile failed ($rc)" >&2; cat "$OUT/compile.log" >&2; exit "$rc"; }
"$OUT/prog" "$@"
