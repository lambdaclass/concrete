#!/usr/bin/env bash
# Phase 3 diagnostic quality checks.
# Source this or run standalone; expects COMPILER and TESTDIR to be set,
# and PASS / FAIL counters to be initialised by the caller.
#
# If invoked standalone, set defaults:
: "${COMPILER:=./.lake/build/bin/concrete}"
: "${TESTDIR:=lean_tests}"
: "${PASS:=0}"
: "${FAIL:=0}"

STANDALONE=false
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    STANDALONE=true
fi

# ---------- 1. Multi-error: all independent errors reported ----------
output=$($COMPILER "$TESTDIR/phase3_diag_multi_error.con" --emit-llvm 2>&1)
if echo "$output" | grep -q "type mismatch in let binding 'a'" \
   && echo "$output" | grep -q "type mismatch in let binding 'b'" \
   && echo "$output" | grep -q "type mismatch in let binding 'c'"; then
    echo "  ok  phase3_diag_multi_error.con reports all 3 independent errors"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_multi_error.con missing expected errors"
    echo "      got: $output"
    FAIL=$((FAIL + 1))
fi

# ---------- 2. Specific location: error on the right line ----------
output=$($COMPILER "$TESTDIR/phase3_diag_specific_location.con" --emit-llvm 2>&1)
# The error must reference line 9 (the deeply-nested let), not any parent line.
if echo "$output" | grep -q "^.*:9:.*error"; then
    echo "  ok  phase3_diag_specific_location.con error points to correct line (9)"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_specific_location.con error not on expected line 9"
    echo "      got: $output"
    FAIL=$((FAIL + 1))
fi

# ---------- 3. No cascade: single root cause produces < 5 errors ----------
output=$($COMPILER "$TESTDIR/phase3_diag_no_cascade.con" --emit-llvm 2>&1)
error_count=$(echo "$output" | grep -c "error\[" || true)
if [ "$error_count" -lt 5 ]; then
    echo "  ok  phase3_diag_no_cascade.con produced $error_count error(s) (< 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_no_cascade.con cascaded into $error_count errors (expected < 5)"
    echo "      got: $output"
    FAIL=$((FAIL + 1))
fi

# ---------- 4. Hint quality: capability error includes hint text ----------
output=$($COMPILER "$TESTDIR/phase3_diag_hint_quality.con" --emit-llvm 2>&1)
if echo "$output" | grep -q "requires File" \
   && echo "$output" | grep -qi "hint:"; then
    echo "  ok  phase3_diag_hint_quality.con includes capability error with hint"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_hint_quality.con missing capability error or hint"
    echo "      got: $output"
    FAIL=$((FAIL + 1))
fi

# ---------- 5. Type mismatch: shows expected and got types ----------
output=$($COMPILER "$TESTDIR/phase3_diag_type_mismatch.con" --emit-llvm 2>&1)
if echo "$output" | grep -q "expected" \
   && echo "$output" | grep -q "got"; then
    echo "  ok  phase3_diag_type_mismatch.con shows both expected and got types"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_type_mismatch.con missing expected/got in error message"
    echo "      got: $output"
    FAIL=$((FAIL + 1))
fi

# ---------- Summary (standalone mode only) ----------
if $STANDALONE; then
    echo ""
    echo "=== Phase 3 Diagnostic Quality ==="
    echo "PASS: $PASS  FAIL: $FAIL"
    if [ "$FAIL" -gt 0 ]; then
        exit 1
    fi
fi
