#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# defer / cleanup gate (ROADMAP Phase 6 #7).
#
# `defer <call>;` registers a call to run at scope exit. This gate locks the
# implemented semantics by building examples/defer/cleanup_order and asserting
# the exact output order:
#   - LIFO: the last-registered defer runs first;
#   - runs on every exit path: both early `return` and normal fall-through.
# It also pins the current limitation that the defer body must be a CALL (a block
# body `defer { … }` is rejected) — a documented V1 boundary (docs/DEFER.md).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== defer ordering + exit-path semantics (examples/defer/cleanup_order) ==="
ex="examples/defer/cleanup_order"
if (cd "$ex" && "$C" build >"$TMP/build.txt" 2>&1) && [ -x "$ex/cleanup_order" ]; then
  got="$("$ex/cleanup_order" 2>/dev/null)"
  # f(true): body-start, early-return, then LIFO d2,d1 ; sep ; f(false): body-start, body-end, d2,d1 ; main ret 0
  want=$'body-start\nearly-return\nd2\nd1\n----\nbody-start\nbody-end\nd2\nd1\n0'
  if [ "$got" = "$want" ]; then
    ok "defer runs LIFO on both early-return and fall-through"
  else
    no "defer output order mismatch"; echo "    --- got ---"; printf '%s\n' "$got" | sed 's/^/      /'
  fi
else
  no "cleanup_order example failed to build"; sed 's/^/      /' "$TMP/build.txt" | head -5
fi

echo "=== defer body must be a call (block form rejected) — documented V1 boundary ==="
printf 'mod m { fn main() -> Int { defer { } return 0; } }' > "$TMP/blk.con"
if "$C" "$TMP/blk.con" -o "$TMP/blk.bin" >"$TMP/blk.out" 2>&1; then
  no "block-form defer was accepted (should be rejected in V1)"
else
  ok "block-form defer rejected ($(grep -oE '\([A-Z0-9]+\)' "$TMP/blk.out" | head -1))"
fi

echo ""
echo "DEFER: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
