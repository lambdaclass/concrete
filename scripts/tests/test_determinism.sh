#!/usr/bin/env bash
set -euo pipefail

# Deterministic Artifact Regression Suite
#
# Verifies that all compiler output paths produce identical results across
# two consecutive runs of the same source file. This catches:
# - HashMap/iteration-order drift
# - Timestamp leaks into deterministic output
# - Counter reset bugs
# - Environment-dependent output
#
# Usage:
#   scripts/tests/test_determinism.sh [--quick]
#
# --quick: test 3 representative programs instead of the full set

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
TMPDIR_BASE="/tmp/concrete_determinism_$$"
mkdir -p "$TMPDIR_BASE/run1" "$TMPDIR_BASE/run2"

PASS=0
FAIL=0
SKIP=0

cleanup() {
    rm -rf "$TMPDIR_BASE"
}
trap cleanup EXIT

# Report modes to test (all 18)
REPORT_MODES=(
    caps unsafe layout interface alloc mono authority
    proof eligibility proof-status obligations extraction
    proof-diagnostics effects recursion fingerprints diagnostics-json
    consistency verify
)

# Query kinds to test
QUERY_KINDS=(caps effects proof-status obligations eligibility extraction)

# IR emit modes
EMIT_MODES=(emit-llvm emit-ssa emit-core)

# Test programs — cover simple, medium, and complex
if [ "${1:-}" = "--quick" ]; then
    PROGRAMS=(
        tests/programs/fib.con
        tests/programs/complex_linked_list.con
        tests/programs/pressure_defer_with_borrow.con
    )
else
    PROGRAMS=(
        tests/programs/fib.con
        tests/programs/arithmetic.con
        tests/programs/struct_basic.con
        tests/programs/linear_consume.con
        tests/programs/borrow_mut.con
        tests/programs/defer_basic.con
        tests/programs/complex_linked_list.con
        tests/programs/complex_closure_pipeline.con
        tests/programs/pressure_borrow_in_loop.con
        tests/programs/pressure_defer_with_borrow.con
        tests/programs/pressure_interleaved_linear.con
        tests/programs/pressure_heap_defer_free.con
    )
fi

check_determinism() {
    local desc="$1"
    local file1="$2"
    local file2="$3"

    if diff -q "$file1" "$file2" > /dev/null 2>&1; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        echo "  FAIL: $desc"
        diff "$file1" "$file2" | head -10
    fi
}

echo "=== Deterministic Artifact Regression Suite ==="
echo ""

for prog in "${PROGRAMS[@]}"; do
    base=$(basename "$prog" .con)
    echo "--- $base ---"

    # Test all report modes
    for mode in "${REPORT_MODES[@]}"; do
        $COMPILER "$prog" --report "$mode" > "$TMPDIR_BASE/run1/${base}_report_${mode}.txt" 2>&1 || true
        $COMPILER "$prog" --report "$mode" > "$TMPDIR_BASE/run2/${base}_report_${mode}.txt" 2>&1 || true
        check_determinism "--report $mode ($base)" \
            "$TMPDIR_BASE/run1/${base}_report_${mode}.txt" \
            "$TMPDIR_BASE/run2/${base}_report_${mode}.txt"
    done

    # Test query modes
    for kind in "${QUERY_KINDS[@]}"; do
        $COMPILER "$prog" --query "$kind" > "$TMPDIR_BASE/run1/${base}_query_${kind}.txt" 2>&1 || true
        $COMPILER "$prog" --query "$kind" > "$TMPDIR_BASE/run2/${base}_query_${kind}.txt" 2>&1 || true
        check_determinism "--query $kind ($base)" \
            "$TMPDIR_BASE/run1/${base}_query_${kind}.txt" \
            "$TMPDIR_BASE/run2/${base}_query_${kind}.txt"
    done

    # Test IR emit modes
    for mode in "${EMIT_MODES[@]}"; do
        $COMPILER "$prog" --"$mode" > "$TMPDIR_BASE/run1/${base}_${mode}.txt" 2>&1 || true
        $COMPILER "$prog" --"$mode" > "$TMPDIR_BASE/run2/${base}_${mode}.txt" 2>&1 || true
        check_determinism "--$mode ($base)" \
            "$TMPDIR_BASE/run1/${base}_${mode}.txt" \
            "$TMPDIR_BASE/run2/${base}_${mode}.txt"
    done

    # Test snapshot — strip timestamp before comparison.
    # DOCUMENTED EXCEPTION: the snapshot "timestamp" field is intentionally
    # nondeterministic (wall-clock time of snapshot creation). All other
    # snapshot fields must be deterministic across identical inputs.
    $COMPILER snapshot "$prog" -o "$TMPDIR_BASE/run1/${base}_snapshot.json" > /dev/null 2>&1 || true
    $COMPILER snapshot "$prog" -o "$TMPDIR_BASE/run2/${base}_snapshot.json" > /dev/null 2>&1 || true
    if [ -f "$TMPDIR_BASE/run1/${base}_snapshot.json" ] && [ -f "$TMPDIR_BASE/run2/${base}_snapshot.json" ]; then
        # Strip timestamp field (documented nondeterministic exception)
        python3 -c "
import json, sys
with open(sys.argv[1]) as f: d1 = json.load(f)
with open(sys.argv[2]) as f: d2 = json.load(f)
d1.pop('timestamp', None); d2.pop('timestamp', None)
sys.exit(0 if d1 == d2 else 1)
" "$TMPDIR_BASE/run1/${base}_snapshot.json" "$TMPDIR_BASE/run2/${base}_snapshot.json"
        if [ $? -eq 0 ]; then
            PASS=$((PASS + 1))
        else
            FAIL=$((FAIL + 1))
            echo "  FAIL: snapshot ($base) — content differs (excluding timestamp)"
        fi
    else
        SKIP=$((SKIP + 1))
    fi
done

echo ""
echo "=== Determinism Results ==="
echo "  passed:  $PASS"
echo "  failed:  $FAIL"
echo "  skipped: $SKIP"

if [ "$FAIL" -gt 0 ]; then
    echo ""
    echo "DETERMINISM REGRESSION DETECTED"
    exit 1
fi
