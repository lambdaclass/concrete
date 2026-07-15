#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Smoke test for the wrong-code reducer predicates and wrapper.
#
# Verifies each predicate script parses arguments, calls the compiler,
# and distinguishes pass/fail correctly. Does NOT run a long
# minimization — the reducer engine is exercised by `make
# test-wrong-code` indirectly (corpus entries are stable across
# reductions) and by the demos in docs/REDUCER_WORKFLOW.md.

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

check() {
  local name="$1" expected_rc="$2"; shift 2
  "$@" >/dev/null 2>&1
  local actual_rc=$?
  # Predicate scripts return 0 (match) or 1 (no match); 2+ is usage error.
  if [ "$actual_rc" = "$expected_rc" ]; then
    echo "  ok   $name"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $name (expected exit $expected_rc, got $actual_rc)"
    FAIL=$((FAIL + 1))
  fi
}

REDUCE=scripts/reduce
WRAP=scripts/tests/minimize_wrong_code.sh

# expect-error-code.sh
check "error-code matches E0209 on disagree fixture" 0 \
  "$REDUCE/expect-error-code.sh" E0209 tests/programs/bug_int_match_disagree.con
check "error-code does not match E0209 on a passing program" 1 \
  "$REDUCE/expect-error-code.sh" E0209 tests/programs/fib.con
check "error-code does not match nonexistent code" 1 \
  "$REDUCE/expect-error-code.sh" E9999 tests/programs/bug_int_match_disagree.con

# expect-runtime-output.sh
check "runtime-output matches on chained newtype (42)" 0 \
  "$REDUCE/expect-runtime-output.sh" 42 tests/programs/adversarial/newtype/chained.con
check "runtime-output mismatches when expected is wrong" 1 \
  "$REDUCE/expect-runtime-output.sh" 99 tests/programs/adversarial/newtype/chained.con
check "runtime-output rejects compile-failing program" 1 \
  "$REDUCE/expect-runtime-output.sh" 0 tests/programs/bug_int_match_disagree.con

# expect-oracle-mismatch.sh — known agreeing program → no mismatch.
check "oracle-mismatch returns 1 on agreeing fib (no mismatch)" 1 \
  "$REDUCE/expect-oracle-mismatch.sh" tests/programs/fib.con
check "oracle-mismatch returns 1 on PENDING program (interp unsupported)" 1 \
  "$REDUCE/expect-oracle-mismatch.sh" examples/fixed_capacity/src/main.con

# expect-report-contains.sh
check "report-contains finds 'main' in caps report" 0 \
  "$REDUCE/expect-report-contains.sh" caps main tests/programs/adversarial/newtype/chained.con
check "report-contains does not find a junk substring" 1 \
  "$REDUCE/expect-report-contains.sh" caps definitely-not-in-report tests/programs/adversarial/newtype/chained.con

# Wrapper argument plumbing — must reject missing predicate.
check "wrapper rejects missing --predicate" 2 \
  "$WRAP" tests/programs/fib.con
check "wrapper rejects missing source" 2 \
  "$WRAP" /nonexistent/path.con --predicate error-code:E0209
# Wrapper argument plumbing — must reject when predicate doesn't hold.
# (We pass a passing program with a compile-error predicate.)
check "wrapper refuses to reduce when predicate fails on input" 1 \
  "$WRAP" tests/programs/fib.con --predicate error-code:E0209

echo ""
echo "REDUCER-SMOKE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
