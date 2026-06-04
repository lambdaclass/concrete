#!/usr/bin/env bash
# `concrete prove` v1.1 CLI gate.
#
# Exercises the three read-only sub-modes on their fixtures and asserts a stable
# substring of each output:
#   --emit-link        on constant_time_tag.ct_compare  (source-linked)
#   --show-obligation  on loop_invariant.count_up        (omega leaf O4)
#   --replay           on loop_invariant.count_up        (omega) + a bv_decide call site
#
# Substrings, not byte-exact snapshots: the output is prose-ish and may gain
# fields; these assertions pin the load-bearing facts.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

PASS=0
FAIL=0

# assert <label> <substring> <command...>
assert_contains() {
  local label="$1"; local needle="$2"; shift 2
  local out; out="$("$@" 2>&1)"
  if printf '%s' "$out" | grep -qF -- "$needle"; then
    echo "  ok   $label — found '$needle'"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label — missing '$needle'"
    printf '%s\n' "$out" | sed 's/^/      /' | head -20
    FAIL=$((FAIL + 1))
  fi
}

# assert_absent <label> <substring> <command...>
assert_absent() {
  local label="$1"; local needle="$2"; shift 2
  local out; out="$("$@" 2>&1)"
  if printf '%s' "$out" | grep -qF -- "$needle"; then
    echo "  FAIL $label — unexpected '$needle'"
    FAIL=$((FAIL + 1))
  else
    echo "  ok   $label — no '$needle'"
    PASS=$((PASS + 1))
  fi
}

CT="examples/constant_time_tag/src/main.con"
LI="examples/loop_invariant/src/main.con"
CC="tests/programs/contract_callsite/main.con"

echo "=== prove --emit-link ==="
assert_contains "emit-link spec"     "#[spec(Concrete.Proof.ctCompareExpr)]" \
  "$COMPILER" prove "$CT" constant_time_tag.ct_compare --emit-link
assert_contains "emit-link proof_by" "#[proof_by(Concrete.Proof.ct_compare_same_tag_correct)]" \
  "$COMPILER" prove "$CT" constant_time_tag.ct_compare --emit-link

echo "=== prove --show-obligation ==="
assert_contains "show-obl O4 concl"  "0 ≤ (8 - i)" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation O4
assert_contains "show-obl O4 status" "proved_by_kernel_decision (omega)" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation O4
assert_contains "show-obl O2 shape"  "loop_preserves" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation O2

echo "=== prove --replay ==="
assert_contains "replay omega closes" "still closes" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --replay
assert_absent   "replay no failures"  "FAIL" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --replay
assert_contains "replay bv_decide"    "still closes" \
  "$COMPILER" prove "$CC" demo.letgood --replay

echo ""
echo "PROVE-CLI: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
