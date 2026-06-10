#!/usr/bin/env bash
# Proven-violation enforcement known-hole gate.
#
# A runtime-safety obligation the compiler discharges to `violation` is a
# compile-time PROOF that the program is wrong (e.g. a constant index proven
# out of bounds, a constant zero divisor) — categorically different from an
# `unproven` obligation. Today such proven violations are only REPORTED, not
# enforced: safe code containing them still builds and ships UB.
#
# This gate tracks that hole without pretending it is fixed:
#   1. Reproduces it: the known-violation projects still build.
#   2. Confirms detection works (so only enforcement is missing): the
#      array-bounds violation is classified `VIOLATION` in --report contracts.
# When proven violations become hard errors by default, the builds will fail;
# flip the reproduction assertions to expected-reject at that point.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

OOB="examples/known_holes/proven_oob_index"
DIV="examples/known_holes/proven_div_zero"

echo "=== known hole reproduces: proven safety violations still build ==="
if (cd "$OOB" && "$COMPILER" build >/dev/null 2>&1); then
  ok "constant out-of-bounds index a[5] on [i64;3] still builds (KNOWN HOLE)"
else
  no "constant OOB no longer builds — flip this gate to expected-reject"
fi
rm -f "$OOB/proven_oob_index"

if (cd "$DIV" && "$COMPILER" build >/dev/null 2>&1); then
  ok "literal divide-by-zero 10/0 still builds (KNOWN HOLE)"
else
  no "literal div-by-zero no longer builds — flip this gate to expected-reject"
fi
rm -f "$DIV/proven_div_zero"

echo "=== detection works; only enforcement is missing ==="
OOB_REP="$("$COMPILER" "$OOB/src/main.con" --report contracts 2>&1)"
if printf '%s' "$OOB_REP" | grep -qi "VIOLATION: index out of bounds (constant index)"; then
  ok "compiler classifies the constant OOB as VIOLATION (proven, not unproven)"
else
  no "constant OOB is no longer detected as a VIOLATION — detection regressed"
fi

DIV_REP="$("$COMPILER" "$DIV/src/main.con" --report contracts 2>&1)"
if printf '%s' "$DIV_REP" | grep -qi "division: non-zero divisor"; then
  ok "compiler generates the division non-zero-divisor obligation"
else
  no "division obligation no longer generated — detection regressed"
fi

echo ""
echo "PROVEN-VIOLATION-ENFORCEMENT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
