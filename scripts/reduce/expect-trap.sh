#!/usr/bin/env bash
# Reducer predicate: exit 0 iff the candidate compiles AND the compiled
# binary ABORTS (a runtime trap — abort/SIGABRT from a checked div-by-zero,
# overflow, or bounds check), i.e. it exits with a signal (code >= 128).
#
# Usage: expect-trap.sh <candidate.con>
#
# Used to reduce a "trap-preservation" counterexample: a program that MUST
# trap at runtime. If a constant-fold / optimizer bug silently drops the trap,
# the binary exits normally and this predicate stops reproducing — so the
# reducer keeps exactly the code that carries the trap. The candidate path is
# the last argument (the `external:<cmd>` calling convention).

set -uo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <candidate.con>" >&2
  exit 2
fi

CANDIDATE="${@: -1}"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER" >&2
  exit 2
fi

BIN=$(mktemp)
trap 'rm -f "$BIN"' EXIT

# Must compile cleanly; a compile failure is not a runtime trap.
if ! "$COMPILER" "$CANDIDATE" -o "$BIN" >/dev/null 2>&1; then
  exit 1
fi

"$BIN" >/dev/null 2>&1
rc=$?
# Aborted-by-signal exit codes are 128 + signum (SIGABRT=134, SIGSEGV=139, ...).
[ "$rc" -ge 128 ] && exit 0 || exit 1
