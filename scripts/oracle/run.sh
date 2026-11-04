#!/usr/bin/env bash
# Run one program through the pinned legacy oracle (scripts/oracle/ORACLE.lock).
#
#   run.sh [--interp] <file.con> [-- args...]   compile natively and run (default), or interpret
#   run.sh --path                               print the verified binary's path and exit
#
# The install ($ORACLE_HOME/<commit>/, from fetch.sh or build.sh) is verified before every use: each
# file against the artifact's manifest, nothing added, and bin/concrete against the lock's
# binary_sha256. Anything else is refused, never run. The compiler reads three environment variables
# (CONCRETE_STD, CONCRETE_MODEL_DIFF_PERTURB, PROOF_LAYER_MODE; git grep getEnv at the tag): the
# first is set to the verified std/ and the other two are removed, never inherited. The program is
# compiled and run inside the artifact's own dev shell (its flake.nix and flake.lock), so the C
# toolchain is the locked one.
#
# Exit status: the program's. A compile failure exits with the compiler's status after a line
# "oracle: compile failed" on stderr. 125 means the oracle itself is unusable (not installed, or
# failed verification): a caller must report that as incomplete, never as a program result.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/oracle/lib.sh
. "$HERE/lib.sh"
die() { echo "oracle: $*" >&2; exit 125; }
D="$(oracle_home)/$(lock commit)"
why="$(oracle_verify_install "$D")" || die "$why (install with scripts/oracle/fetch.sh or build.sh)"
BIN="$D/bin/concrete"
[ "${1:-}" = "--path" ] && { echo "$BIN"; exit 0; }
mode=native
[ "${1:-}" = "--interp" ] && { mode=interp; shift; }
[ $# -ge 1 ] || { echo "usage: run.sh [--interp] <file.con> [-- args...] | --path" >&2; exit 2; }
FILE="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"; shift
[ "${1:-}" = "--" ] && shift
NIX=(env -u CONCRETE_MODEL_DIFF_PERTURB -u PROOF_LAYER_MODE CONCRETE_STD="$D/std"
     nix --extra-experimental-features "nix-command flakes")
OUT="$(mktemp -d "${TMPDIR:-/tmp}/oracle-run.XXXXXX")"; trap 'rm -rf "$OUT"' EXIT
if [ "$mode" = interp ]; then
  "${NIX[@]}" develop "path:$D/shell" --command "$BIN" "$FILE" --interp -- "$@"
  exit $?
fi
"${NIX[@]}" develop "path:$D/shell" --command "$BIN" "$FILE" -o "$OUT/prog" >"$OUT/compile.log" 2>&1 || {
  rc=$?; echo "oracle: compile failed ($rc)" >&2; cat "$OUT/compile.log" >&2; exit "$rc"; }
"$OUT/prog" "$@"
