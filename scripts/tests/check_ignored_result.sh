#!/usr/bin/env bash
# Ignored-result diagnostics gate (ROADMAP Phase 6 #13; docs/IGNORED_RESULT.md).
#
# A `;`-terminated statement expression whose value is a fallible result
# (`Result<…>` or `Option<…>`) silently throws away a possible failure/absence.
# Concrete flags that discard with E0286 unless it is explicitly acknowledged.
# This gate locks:
#   - a discarded `Result`/`Option` statement is rejected with E0286,
#   - the intentional ignore is EXHAUSTIVE handling — `match e { Ok { _ } => {},
#     Err { _ } => {} }` (Concrete is linear: a catch-all `_` arm over a non-Copy
#     value is rejected E0288; `_` may ignore only the Copy payloads) (compiles),
#   - handling the value (`match`) is not a discard (compiles),
#   - a non-must-use value (plain integer) discard is NOT flagged (compiles),
#   - an `Option`/`Result` in trailing VALUE position is not a discard (compiles),
#   - SOUNDNESS: a `_` wildcard cannot drop a resource owner — a leak still
#     errors (E0288), so the acknowledgement can't be abused to drop resources.
#
# Fixtures: tests/programs/ignored_result/.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/ignored_result"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# compiles + runs, asserting the program's stdout
run_expect(){ local n="$1" e="$2"
  if ! "$C" "$D/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  local g; g="$("$TMP/$n.bin" 2>/dev/null)"
  [ "$g" = "$e" ] && ok "$n -> $g" || no "$n: got '$g' want '$e'"; }

reject_with(){ local n="$1" c="$2"
  local out; out="$("$C" "$D/$n.con" -o "$TMP/$n.bin" 2>&1)"
  if echo "$out" | grep -qE 'error\['; then
    echo "$out" | grep -q "($c)" && ok "$n rejected with $c" \
      || { no "$n rejected, wrong code"; echo "$out" | grep -oE '\([A-Z0-9]+\)' | head -1 | sed 's/^/        got: /'; }
  else no "$n: expected rejection ($c), compiled"; fi; }

echo "=== discarded fallible result is rejected (E0286) ==="
reject_with discard_result E0286
reject_with discard_option E0286

echo "=== acknowledged / handled / non-must-use are accepted ==="
run_expect ack_let_underscore 7
run_expect ack_handled 7
run_expect nonmustuse_ok 7
run_expect value_position_ok 7

echo '=== soundness: `_` cannot silently consume a resource owner (E0288) ==='
reject_with resource_underscore_still_errors E0288

echo ""
echo "IGNORED-RESULT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
